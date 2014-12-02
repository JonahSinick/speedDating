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
wealthInds = n[grep("WealthInd",n)]

menTraits = n[grep("M$",n)][c(11:28,30:52,54:59,61:70,73:75,77:96)]
womenTraits = gsub("M$", "W", menTraits)
menAvgRatings = menTraits[grep("AvgRating", menTraits)]
womenAvgRatings = gsub("M$", "W", menAvgRatings)
menGuessRatings = menTraits[grep("RatingGuess", menTraits)]
womenGuessRatings = gsub("M$", "W", gsub("M$", "W", menGuessRatings))
menActs = menTraits[grep("Act", menTraits)]
womenActs = gsub("M$", "W", gsub("M$", "W", menActs))
menPrefs = menTraits[grep("Pref", menTraits)]
womenPrefs = gsub("M$", "W", gsub("M$", "W", menPrefs))
ratingsDecW = c(menAvgRatings, menGuessRatings, womenAvgRatings, womenGuessRatings)

baselineDecM = c("raterDecAvgM", "decAvgW","avgWaveDecM")
baselineDecW = c("raterDecAvgW", "decAvgM","avgWaveDecW", "avgWaveDecM")


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
  return(10000*round(errorFrame,4))
} 


getProbs = function(train, test, features, tar){
  bestLogLoss = 1000
  bestProbs = NA
  s=scale(train[features],center=TRUE,scale=TRUE)
  tryCosts = c(heuristicC(s))
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
  return(bestProbs)
}

frameHash = hash()
waves = unique(merged[["wave"]])
frameHash[["train"]] = merged[merged[["wave"]] %in% waves[c(2:4)],]
frameHash[["test"]] = merged[merged[["wave"]] %in% waves[c(1)],]

features = c(baselineDecW, 
             "datesCrossProbDecW", 
             "fieldsCrossProbDecW", 
             "careersCrossProbDecW",
             "goalsCrossProbDecW", 
             "racesCrossProbDecW",
             "attrAvgRatingM",
             "raterDecAvgM",
             "exphappyW")
for(key in keys(frameHash)){
  if(key == "test"){
    df = frameHash[[key]]
    df["baselineProbDecW"] = getProbs(frameHash[["train"]], df, c(baselineDecW), "decW")
    df["featuresProbDecW"] = getProbs(frameHash[["train"]], df, c(features), "decW")  
    frameHash[[key]] = df
    for(name in c(crosses)){
      print(name)
      df[paste(name,"ProbDecW",sep="")] = getProbs(frameHash[["train"]], df, c(features, name), "decW")
      frameHash[[key]] = df
    } 
  }
  frameHash[[key]] = df
}
n = names(frameHash[["test"]])
probs = n[grep("ProbDec",n)]
menProbs = c("decM", probs[grep("ProbDecM", probs)])
womenProbs = c("decW", probs[grep("ProbDecW", probs)])
womenAccuracyFrame = accuracyFrame(frameHash[["train"]][womenProbs],"decW")
baseline = womenAccuracyFrame["baselineProbDecW", "logLoss"]
currentBest = womenAccuracyFrame["featuresProbDecW", "logLoss"]
# currentBest - womenAccuracyFrame[womenAccuracyFrame["logLoss"] < currentBest - 10 ,]

currentBest - womenAccuracyFrame[womenAccuracyFrame["logLoss"] <= currentBest,]
print(c(baseline,currentBest))

womenAccuracyFrame
