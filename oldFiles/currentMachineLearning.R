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
baselineDecM = c("raterDecAvgM", "decAvgW","avgWaveDecM")
baselineDecW = c("raterDecAvgW", "decAvgM","avgWaveDecW")
mainRatingsDecM = c("attrAvgRatingW", "funAvgRatingW", "likeAvgRatingW")
mainRatingsDecW = gsub("W$","M",mainRatingsDecM)

mainsDecM = c(baselineDecM, mainRatingsDecM)
mainsDecW = c(baselineDecW, mainRatingsDecW)
getProbs = function(train, test, features, tar){
  tryCosts = 10^(seq(-3,3,by=0.3))
  bestLogLoss = 1000
  bestProbs = NA
  s=scale(train[features],center=TRUE,scale=TRUE)
  
  for(co in tryCosts){
    
    m=LiblineaR(data=s,labels=factor(train[,tar]),type=0,cost=co,bias=TRUE,verbose=FALSE)
    s2= scale(test[features],attr(s,"scaled:center"),attr(s,"scaled:scale"))
    p=predict(m,s2,prob=TRUE)
    probs = p$probabilities[,"1"]  
    logLoss = round(logLoss(test[[tar]], probs),4)
    if(logLoss < bestLogLoss){
      bestProbs = probs
      bestLogLoss = logLoss
    }
  }
  return(bestProbs)
}

accuracyFrame = function(df, tar){
  n = names(df)
  probNames = n[grep("ProbDec",n)]
  errorFrame = df[c(1),c()]
  errorFrame["logLoss"] = 0
  errorFrame["overallErrorRate"] = 0
  errorFrame["yesGuessErrorRate"] = 0
  errorFrame["noGuessErrorRate"] = 0
  for(name in probNames){
    probs = df[[name]]
    target = df[[tar]]
    errorFrame[name,] = 0
    guesses = ifelse(probs > 0.5, 1, 0)
    correctYesGuessSum = sum(ifelse(guesses == 1 & target == 1, 1,0))
    incorrectYesGuessSum = sum(ifelse(guesses == 1 & target == 0, 1,0))
    correctNoGuessSum = sum(ifelse(guesses == 0 & target == 0, 1,0))
    incorrectNoGuessSum = sum(ifelse(guesses == 0 & target == 1, 1,0))
    errorFrame[name,"logLoss"] = logLoss(target, probs)
    errorFrame[name, "overallErrorRate"] = (incorrectYesGuessSum + incorrectNoGuessSum)/length(target)
    errorFrame[name, "yesGuessErrorRate"] = (incorrectYesGuessSum)/(incorrectYesGuessSum + correctYesGuessSum)    
    errorFrame[name, "noGuessErrorRate"] = (incorrectNoGuessSum)/(incorrectNoGuessSum + correctNoGuessSum)
    
  }
  return(round(errorFrame,4))
} 

frameHash = hash()
frameHash[["train"]] = train
frameHash[["futureTrain"]] = futureTrain
frameHash[["futureTest"]] = futureTest
newMenTraits = menTraits[c(9:26,28:70,72:96)]
newWomenTraits = gsub("M$", "W", newMenTraits)
traits = c(newMenTraits,newWomenTraits)
features = c(baselineDecM, "attrAvgRatingW", "raterDecAvgW", "tvMiscActWProbDecM")
for(key in keys(frameHash)){
  df = frameHash[[key]]
  df["baselineProbDecM"] = getProbs(frameHash[["train"]], df, baselineDecM, "decM")
  for(name in newWomenTraits){
    print(name)
    df = frameHash[[key]]
    df[paste(name,"ProbDecM",sep="")] = getProbs(frameHash[["train"]], df, c(features, name), "decM")
    frameHash[[key]] = df
  }  
  frameHash[[key]] = df
}


n = names(frameHash[["futureTrain"]])
probs = n[grep("Prob",n)]
menProbs = c("decM", probs[grep("ProbDecM", probs)])
a = accuracyFrame(frameHash[["futureTrain"]][menProbs],"decM")
b = a[5316  - 10000*a["logLoss"]- 40 - 4 - 4 - 6> 0,]["logLoss"]
5316  - 10000*b["logLoss"]- 40 - 4 - 4 - 6 - 5
candidates = rownames(a[a["logLoss"] < 0.527,])[2:18]
candidates = gsub("ProbDecM", "" ,candidates)

RFsuffixes = c("RFDecM", "RFDecW")

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