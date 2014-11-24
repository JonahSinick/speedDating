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


menTraits = n[grep("M$",n)][c(11:59,61,96:112)]
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
baselineDecW = c("raterDecAvgW", "decAvgM","avgWaveDecW")


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
#   tryCosts = 10^(seq(-2,2,by=0.4))
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
  print(bestCost)
  return(bestProbs)
}

waves = unique(merged[["wave"]])
merged[c("baselineProbDecW", "bestGuessProbDecW")] = 0
bestGuessFeatures = c(baselineDecW, 
             "fieldsCrossProbDecW",
             "racesCrossProbDecW",
             "careersCrossProbDecW",
             "goalsCrossProbDecW", 
             "datesCrossProbDecW", 
             "racesManProbDecW",
             "attrAvgRatingM", 
             "funAvgRatingM",
             "probAvgRatingM")

for(w in waves){
  train = merged[merged["wave"] != w,]
  test = merged[merged["wave"] == w,]
  merged[merged["wave"] == w,][["baselineProbDecW"]] = getProbs(train,test,baselineDecW,"decW")
  merged[merged["wave"] == w,][["baselineProbDecW"]] = getProbs(train,test,features,"decW")
}
