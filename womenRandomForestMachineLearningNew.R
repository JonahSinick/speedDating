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


menTraits = n[grep("M$",n)][3:98]
menTraits = menTraits[c(9:26,28:57,59:70,72:96)]
womenTraits = gsub("M$", "W", menTraits)
waves = unique(merged[["wave"]])





menAvgRatings = menTraits[grep("AvgRating", menTraits)]
womenAvgRatings = gsub("M$", "W", menAvgRatings)
menGuessRatings = menTraits[grep("RatingGuess", menTraits)]
womenGuessRatings = gsub("M$", "W", gsub("M$", "W", menGuessRatings))
menActs = menTraits[grep("Act", menTraits)]
womenActs = gsub("M$", "W", gsub("M$", "W", menActs))
menPrefs = menTraits[grep("Pref", menTraits)]
womenPrefs = gsub("M$", "W", gsub("M$", "W", menPrefs))
ratingsDecW = c(menAvgRatings, womenAvgRatings)


merged[c("RFTempProbsDecW","RFProbsDecW", "RFPredictionsDecW")] = 0
features = c("attrAvgRatingM", "attrAvgRatingW", "likeAvgRatingM")
mergedForWomen = merged[c("wave", "decW", features)]
merged[testProbsColName] = 0
waves = unique(merged[["wave"]])
rowSum = 0 
numCorrect = 0
for(i in waves[2:2]){
  test = mergedForWomen[mergedForWomen["wave"] == i,]
  train = mergedForWomen[mergedForWomen["wave"] != i,]
  test["oddsRatioProd"] = 1
  num = 500
  newWaves = waves[waves != i]
  errorSum = 0
  errors = c()
  for(j in 1:num){
    print(c(i,j))
    predictions_j = paste("predictions",toString(j),sep=" ")
    trainIdxs = sample(newWaves,8)
    trueTrain = train[train[["wave"]] %in% trainIdxs,]
    rf = randomForest(y=as.factor(trueTrain[,"decW"]), x=trueTrain[features], importance=TRUE, ntree=500)
    test[predictions_j] =  predict(rf, test, type="prob")[,"1"]
    test[[predictions_j]] = ifelse(test[[predictions_j]] == 0, 0.01, test[[predictions_j]])
    test[[predictions_j]] = ifelse(test[[predictions_j]] == 1, 0.99, test[[predictions_j]])
    OR = test[[predictions_j]]/(1 - test[[predictions_j]])
    test[["oddsRatioProd"]] = test[["oddsRatioProd"]]*OR
    test[[predictions_j]] =  ifelse(test[[predictions_j]] > 0.5,1,0)
    t = table(test[[predictions_j]],test[["decW"]])    
    error = 1 - (t[1,1] + t[2,2])/nrow(test)
    errors[j] = error
    errorSum = errorSum + error
    test[["predictions"]] = (test[["oddsRatioProd"]]/(1 + test[["oddsRatioProd"]]))^(1/j)
    test[["predictions"]] = ifelse(test[["predictions"]] > 0.5,1,0)
    t = table(test[["predictions"]],test[["decW"]])
    betterError = 1 - (t[1,1] + t[2,2])/nrow(test)
    cat("Trial Number: ", toString(j), " Error: ", error, "\n", sep="")
    cat("Trial Number: ", toString(j), " Average Error: ", errorSum/j, "\n", sep="")
    cat("Trial Number: ", toString(j), " Median Error: ", median(errors), "\n", sep="")
    
    cat("Trial Number: ", toString(j), " Better Error: ", betterError, "\n", sep="")
    
    
  }
}

