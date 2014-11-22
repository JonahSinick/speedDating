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

baselineDecM = c("raterDecAvgM", "decAvgW","avgWaveDecM")
baselineDecW = c("raterDecAvgW", "decAvgM","avgWaveDecW")

probs = c("baselineMen","baselineWomen", "baselineMatch", "guessMen", "guessWomen", "guessMatch")
merged[probs] = 0

getProbs = function(train, test, features, tar){
  tryCosts = 10^(seq(-3,3,by = 0.3))
  bestLogLoss = 1000
  bestProbs = NA
  s=scale(train[features],center=TRUE,scale=TRUE)
  bestCost = NA
  for(co in tryCosts){
    m=LiblineaR(data=s,labels=factor(train[,tar]),type=0,cost=co,bias=TRUE,verbose=FALSE)
    s2= scale(test[features],attr(s,"scaled:center"),attr(s,"scaled:scale"))
    p=predict(m,s2,prob=TRUE)
    probs = p$probabilities[,"1"]  
    logLoss = round(logLoss(test[[tar]], probs),4)
    if(logLoss < bestLogLoss){
      bestProbs = probs
      bestLogLoss = logLoss
      bestCost = co
    }
  }
  print(log(bestCost,10))
  return(bestProbs)
}

newestDecMenTraits = c(baselineDecM, "attrAvgRatingW", "fieldsCrossProbTraitDecM",
                       "goalsCrossProbTraitDecM", "datesCrossProbTraitDecM")
newestDecWomenTraits = c(baselineDecW, "attrAvgRatingM", "raceAsianM_raceWhiteWCross",
                         "fieldsCrossProbDecW", "datesCrossProbDecW", "raterDecAvgM",
                         "raceWhiteM_raceWhiteWCross", "raceAsianM_raceAsianWCross")



for(i in waves){
  trainWaves = waves[!(waves==i)] 
  train = merged[merged["wave"] != i,]
  test = merged[merged["wave"] == i,]
  merged[merged["wave"] == i,][["baselineMen"]] =  getProbs(train, test, baselineDecM, "decM")
#   merged[merged["wave"] == i,][["baselineWomen"]] =  getProbs(train, test, baselineDecW, "decW")
#   merged[merged["wave"] == i,][["guessMen"]] =  getProbs(train, test, newestDecMenTraits, "decM")
#   merged[merged["wave"] == i,][["guessWomen"]] =  getProbs(train, test, newestDecWomenTraits, "decW")
}


# merged["baselineMatchGuess"] = merged["baselineMen"]*merged["baselineWomen"]
merged["betterMatchGuess"] = merged["guessMen"]*merged["guessWomen"]

for(i in waves){
  trainWaves = waves[!(waves==i)] 
  train = merged[merged["wave"] != i,]
  test = merged[merged["wave"] == i,]
#   baselineFeatures = c("avgWaveMatch", "baselineMatchGuess", "matchAvgM", "matchAvgW")
  betterFeatures = c("avgWaveMatch", "betterMatchGuess", "matchAvgM", "matchAvgW")
  
  merged[merged["wave"] == i,][["baselineMatch"]] =  getProbs(train, test, baselineFeatures, "match")
  merged[merged["wave"] == i,][["guessMatch"]] =  getProbs(train, test, betterFeatures, "match")
}



accuracyFrame(merged[c("decM", "baselineMen", "guessMen")], "decM")
accuracyFrame(merged[c("decW", "baselineWomen", "guessWomen")], "decW")

accuracyFrame(merged[c("match", "baselineMatchGuess", "betterMatchGuess", "baselineMatch", "guessMatch")], "match")
table(ifelse(merged[["guessMatch"]] > 0.5, 1, 0), merged[["match"]])
for(key in keys(frameHash)){
  df = frameHash[[key]]
  df["baselineProbDecM"] = getProbs(frameHash[["train"]], df, c(baselineDecM), "decM")
  df["baselineProbDecW"] = getProbs(frameHash[["train"]], df, c(baselineDecW), "decW")
  for(i in 1:8){
    df[menGuessRatings[i]] = getProbs(frameHash[["train"]], df, c(menGuessRatings[i], menAvgRatings[i]), "decM")
    df[womenGuessRatings[i]] = getProbs(frameHash[["train"]], df, c(womenGuessRatings[i], womenAvgRatings[i]), "decW")
    
  }
  frameHash[[key]] = df
}
accuracyFrame = function(df, tar){
  n = names(df)
  probNames = n
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
menAvgRatings = newMenTraits[grep("AvgRating", newMenTraits)]
menGuessRatings = newMenTraits[grep("RatingGuess", newMenTraits)]
womenAvgRatings = gsub("M$", "W", menAvgRatings)
womenGuessRatings = gsub("M$", "W", gsub("M$", "W", menAvgRatings))

newCrosses = crosses[c(1:18,20:60,62:length(crosses))]


for(key in keys(frameHash)){
  for(name in c(newMenTraits,newWomenTraits, newCrosses)){
    print(name)
    df = frameHash[[key]]
    df[paste(name,"StrongProbDecM",sep="")] = getProbs(frameHash[["train"]], df, c(baselineDecM, name), "decM")
    df[paste(name,"StrongProbDecW",sep="")] = getProbs(frameHash[["train"]], df, c(baselineDecW, name), "decW")
    frameHash[[key]] = df
  } 
  frameHash[[key]] = df
}

n = names(frameHash[["futureTest"]])
probs = n[grep("ProbDec",n)]
menProbs = c("decM", probs[grep("ProbDecM", probs)])
womenProbs = c("decW", probs[grep("ProbDecW", probs)])
# womenAccuracyFrame = accuracyFrame(frameHash[["futureTest"]][womenProbs],"decW")
menAccuracyFrame = accuracyFrame(frameHash[["futureTest"]][menProbs],"decM")
slice = menAccuracyFrame[10000*(menAccuracyFrame["baselineProbDecM","logLoss"] - menAccuracyFrame["logLoss"]) >=0,]
10000*(slice["baselineProbDecM","logLoss"] - slice["logLoss"])



newestDecMenTraits = c(baselineDecM, "attrAvgRatingW", "fieldsCrossProbTraitDecM",
                       "goalsCrossProbTraitDecM", "datesCrossProbTraitDecM")
newestDecWomenTraits = c(baselineDecW, "attrAvgRatingM", "raceAsianM_raceWhiteWCross",
                         "fieldsCrossProbDecW", "datesCrossProbDecW",
                         "funRatingGuessM", "raterDecAvgM",
                         "raceWhiteM_raceWhiteWCross", "raceAsianM_raceAsianWCross")

for(key in keys(frameHash)){
  df = frameHash[[key]]
  print(key)
  df[["womenGuess"]] = getProbs(frameHash[["train"]], df, newestDecWomenTraits, "decW")
  df[["menGuess"]] = getProbs(frameHash[["train"]], df, newestDecMenTraits, "decM")
  frameHash[[key]] = df
}
probs = n[grep("StrongProbDec",n)]
menProbs = c("decM", "baselineProbDecM", probs[grep("ProbDecM", probs)], "menGuess")
womenProbs = c("decW", "baselineProbDecW", "womenGuess")

womenAccuracyFrame = accuracyFrame(frameHash[["futureTrain"]][c( womenProbs)],"decW")



10000*(womenAccuracyFrame["baselineProbDecW","logLoss"] - womenAccuracyFrame["logLoss"])
womenAccuracyFrame
