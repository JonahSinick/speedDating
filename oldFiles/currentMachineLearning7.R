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
trainIdxs = waves[1:14]
futureTrainIdxs = waves[15:15]
futureTestIdxs = waves[16:16]
train = merged[merged[["wave"]] %in% trainIdxs ,]
futureTrain = merged[merged[["wave"]] %in% futureTrainIdxs ,]
futureTest = merged[merged[["wave"]] %in% futureTestIdxs ,]
baselineDecM = c("raterDecAvgM", "decAvgW","avgWaveDecM")
baselineDecW = c("raterDecAvgW", "decAvgM","avgWaveDecW")

getProbs = function(train, test, features, tar){
  tryCosts = 0.1
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
  return(bestProbs)
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
  df = frameHash[[key]]
  df["baselineProbDecM"] = getProbs(frameHash[["train"]], df, c(baselineDecM), "decM")
  df["baselineProbDecW"] = getProbs(frameHash[["train"]], df, c(baselineDecW), "decW")
  for(i in 1:8){
    df[menGuessRatings[i]] = getProbs(frameHash[["train"]], df, c(menGuessRatings[i], menAvgRatings[i]), "decM")
    df[womenGuessRatings[i]] = getProbs(frameHash[["train"]], df, c(womenGuessRatings[i], womenAvgRatings[i]), "decW")
    
  }
  frameHash[[key]] = df
}

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
                         "raceWhiteM_raceWhiteWCross")

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
