library(gdata)
library(ggplot2)
library(miscTools)
library(xlsx)
library(hash)
library(aod)
library(randomForest)
library(LiblineaR)
library(Matrix)
library(plyr)
library(recommenderlab)
library(scatterplot3d)
library(Metrics)

merged = read.csv( '~/Desktop/speedDating/mergedCrossFeaturesAdded.csv')
n = names(merged)
avgRatings = n[grep("AvgRating", n)]
guesses = n[grep("RatingGuess",n)]
crosses = n[grep("Cross",n)]
probs = n[grep("ProbDec",n)]
prefs = n[grep("Pref",n)]
menTraits = n[grep("M$",n)][3:98]
womenTraits = n[grep("W$",n)][3:98]
waves = unique(merged[["wave"]])
trainIdxs = waves[1:5]
futureTrainIdxs = waves[6:11]
futureTestIdxs = waves[12:16]
train = merged[merged[["wave"]] %in% trainIdxs ,]
futureTrain = merged[merged[["wave"]] %in% futureTrainIdxs ,]
futureTest = merged[merged[["wave"]] %in% futureTestIdxs ,]



getProbs = function(train, test, features, tar, tryCosts){
  bestLogLoss = 1000
  bestProbs = NA
  s=scale(train[features],center=TRUE,scale=TRUE)
  
  for(co in tryCosts){
    
    m=LiblineaR(data=s,labels=factor(train[,tar]),type=0,cost=co,bias=TRUE,verbose=FALSE)
    s2= scale(test[features],attr(s,"scaled:center"),attr(s,"scaled:scale"))
    p=predict(m,s2,prob=TRUE)
    probs = p$probabilities[,"1"]  
    logLoss = round(logLoss(test[[tar]], probs),3)
    print(co)
    print(logLoss)
    if(logLoss < bestLogLoss){
      bestProbs = probs
      bestLogLoss = logLoss
    }
  }
  return(bestProbs)
}

tryCosts = c(10^(-1.2))
h = hash()
h[["train"]] = train
h[["futureTrain"]] = futureTrain
h[["futureTest"]] = futureTest
n = names(train)
avgs = c(n[grep("attrAvgRating|funAvgRating|likeAvgRating",n)])
guesses = n[grep("attrRatingGuess|funRatingGuess|likeRatingGuess",n)]
menAvgs = c(avgs[grep("M$",avgs)], "raterDecAvgW", "avgWaveDecW", "decAvgbyWofM")
womenAvgs = c(avgs[grep("W$",avgs)], "raterDecAvgM", "avgWaveDecM", "decAvgbyMofW")
menGuesses = guesses[grep("M$",guesses)]
womenGuesses = guesses[grep("W$",guesses)]
for(key in keys(h)){
  df = h[[key]]
  df["avgRatingM"] = getProbs(train, df, menAvgs, "decW", tryCosts)
  df["avgRatingW"] = getProbs(train, df, womenAvgs, "decM", tryCosts)
  df["guessRatingM"] = getProbs(train, df, menGuesses, "decW", tryCosts)
  df["guessRatingW"] = getProbs(train, df, womenGuesses, "decM", tryCosts)
  h[[key]] = df
}
n = names(h[["train"]] )
menNames = n[grep("M$", n)]

womenNames = c()
names = c()

menNames = c(menNames[c(9:26,28:29,31:71,73:77,96:100,119:120)])
for(i in 1:length(menNames)){
  names[i] = gsub("M$", "", menNames[i])
  womenNames[i] = gsub("M$", "W", menNames[i])
}
for(i in 1:length(womenNames)){
  womenNames[i] = gsub("byWofW$", "byMofW", womenNames[i])
}

RFsuffixes = c("RFDecM", "RFDecW")
linearSuffixes = c("LinDecM", "LinDecW")
comboSuffixes = c("ScoreDecM", "ScoreDecW")
guessScores = c("guessRatingM", "guessRatingW")
avgScores = c("avgRatingM", "avgRatingW")
targets = c("decM", "decW")
for(i in 1:2){
  tar = targets[i]
  RFsuffix = RFsuffixes[i]
  LinSuffix = linearSuffixes[i]
  comboSuffix = comboSuffixes[i]
  for(nameIdx in 1:length(names)){
    train = h[["train"]]
    print(nameIdx)
    print(names[nameIdx])
    menName = menNames[nameIdx]
    womenName = womenNames[nameIdx]
    features = c(menName, womenName)
    newColNameRF = paste(names[nameIdx],RFsuffix,sep="")
    newColNameLin = paste(names[nameIdx],LinSuffix,sep="")
    newColNameCombo = paste(names[nameIdx],comboSuffix,sep="")
    print(features)
    rf_fit <- randomForest(y=as.factor(train[,tar]), x=train[features], importance=TRUE, ntree=200, m_try = 2)
    print("PastForest")
    for(key in keys(h)){
      df = h[[key]]
      df[[newColNameLin]] = getProbs(train, df, features, tar, tryCosts)
      predictions = predict(rf_fit, df, type="prob")
      df[[newColNameRF]] = predictions[,"1"]
      h[[key]] = df
    }
  }
}


RFsuffixes = c("RFDecM", "RFDecW")
linearSuffixes = c("LinDecM", "LinDecW")
comboSuffixes = c("ScoreDecM", "ScoreDecW")
guessScores = c("guessRatingM", "guessRatingW")
avgScores = c("avgRatingM", "avgRatingW")
targets = c("decM", "decW")
for(i in 1:2){
  tar = targets[i]
  RFsuffix = RFsuffixes[i]
  LinSuffix = linearSuffixes[i]
  comboSuffix = comboSuffixes[i]
  for(nameIdx in c(1:40,72:73)){
    print(names[nameIdx])
    menName = menNames[nameIdx]
    womenName = womenNames[nameIdx]
    features = c(menName, womenName)
    print(features)
    newColNameRF = paste(names[nameIdx],RFsuffix,sep="")
    newColNameLin = paste(names[nameIdx],LinSuffix,sep="")
    newColNameCombo = paste(names[nameIdx],comboSuffix,sep="")
    for(key in keys(h)){
      df = h[[key]]
      df[[newColNameCombo]] = getProbs(h[["futureTrain"]], df, c(features, newColNameRF), tar, tryCosts)
      h[[key]] = df
    }
  }
}
train = h[["train"]]
futureTrain = h[["futureTrain"]]
futureTest = h[["futureTest"]]
n = names(train)
mens = n[grep("DecM",n)]
menScores = mens[grep("Score",mens)]
menLins = mens[grep("Lin",mens)][c(1:40,72:73)]
womens = n[grep("DecW",n)]
womenScores = womens[grep("Score",womens)]
womenLins = womens[grep("Lin",womens)][c(1:40,72:73)]
round(100*cor(futureTrain[c(menScores,lins)], futureTrain["decM"]))
for(i in 1:length(menScores)){
  logLoss1 = round(logLoss(futureTest[["decM"]],futureTest[[menScores[i]]]),3)
  logLoss2 = round(logLoss(futureTest[["decM"]],futureTest[[menLins[i]]]),3)
  cat(menScores[i],": ",logLoss1," ", menLins[i],": ", logLoss2,"\n",sep="")
}
tryCosts = 10^(seq(-4,4,by=0.05))
baseFeatures = c( "raterDecAvgM", "avgWaveDecM", "decAvgbyMofW")
features = c(baseFeatures, "avgRatingLinDecM", "impraceScoreDecM", "funRatingGuessbyWofScoreDecM",
             "artArtActScoreDecM", "museumsArtActScoreDecM", "goOutScoreDecM")
futureTest[["decMGuess"]] = getProbs(futureTrain, futureTest, features, "decM", tryCosts)
futureTest[["decMGuessSolid"]] = ifelse(futureTest[["decMGuess"]] > 0.5, 1, 0)


round(logLoss(futureTest[["decM"]],futureTest[["decMGuess"]]),3)
t = table(futureTest[["decMGuessSolid"]], futureTest[["decM"]])
(t[1,2] + t[2,1])/(nrow(futureTest))