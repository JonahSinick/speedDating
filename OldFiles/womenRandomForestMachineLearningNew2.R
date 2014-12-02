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
menAvgRatings = menTraits[grep("AvgRating", menTraits)]
womenAvgRatings = gsub("M$", "W", menAvgRatings)
menGuessRatings = menTraits[grep("RatingGuess", menTraits)]
womenGuessRatings = gsub("M$", "W", gsub("M$", "W", menGuessRatings))
menActs = menTraits[grep("Act", menTraits)]
womenActs = gsub("M$", "W", gsub("M$", "W", menActs))
menPrefs = menTraits[grep("Pref", menTraits)]
womenPrefs = gsub("M$", "W", gsub("M$", "W", menPrefs))
ratingsDecW = c(menAvgRatings, menGuessRatings, womenAvgRatings, womenGuessRatings)
features = c(ratingsDecW, menActs, womenActs, womenPrefs, menPrefs, wealthInds, "impraceW", "impreligW",
             "dateW", "dateM", "exphappyW", "goOutM", "goOutW")
merged["waveAvgDecW"] = 0
for(wave in waves){
  merged[merged["wave"] == wave,][["waveAvgDecW"]] = mean(merged[merged["wave"] == wave,][["decW"]])
}

for(w in waves)
merged[c("RFTempProbsDecW","RFProbsDecW", "RFPredictionsDecW")] = 0
features = c(womenActs, menAvgRatings)
mergedForWomen = merged[c("wave", "decW", features)]
waves = unique(merged[["wave"]])
rowSum = 0 
numCorrect = 0
for(i in waves){
  test = mergedForWomen[mergedForWomen["wave"] == i,]
  train = mergedForWomen[mergedForWomen["wave"] != i,]
  test["voteSum"] = 0
  num = 20
  newWaves = waves[waves != i]
  errorSum = 0
  errors = c()
  for(j in 1:num){
    predictions_j = paste("predictions",toString(j),sep=" ")
    trainIdxs = sample(newWaves,8)
    trueTrain = train[train[["wave"]] %in% trainIdxs,]
    rf = randomForest(y=as.factor(trueTrain[,"decW"]), x=trueTrain[features], importance=TRUE, ntree=2000)
    test[predictions_j] =  predict(rf, test, type="prob")[,"1"]
    test[[predictions_j]] = ifelse(test[[predictions_j]] == 0, 0.01, test[[predictions_j]])
    test[[predictions_j]] = ifelse(test[[predictions_j]] == 1, 0.99, test[[predictions_j]])
    test[["voteSum"]] = test[["voteSum"]] + test[[predictions_j]]
    test[[predictions_j]] =  ifelse(test[[predictions_j]] > 0.5,1,0)
    t = table(test[[predictions_j]],test[["decW"]])    
    error = 1 - (t[1,1] + t[2,2])/nrow(test)
    errors[j] = error
    errorSum = errorSum + error
    test[["predictions"]] = ifelse((test[["voteSum"]]/j)  > 0.5,1,0)
    t = table(test[["predictions"]],test[["decW"]])
    betterError = 1 - (t[1,1] + t[2,2])/nrow(test)
    cat("Wave Number: ", toString(i), " Number of rows: ", nrow(test), "\n", sep="")
    cat("Wave Number: ", toString(i), " Percent Yes: ", mean(test[["decW"]]), "\n", sep="")  
    cat("Wave Number: ", toString(i), " Average Error: ", round(100*errorSum/j), "\n", sep="")
    cat("Wave Number: ", toString(i), " Median Error: ", round(100*median(errors)), "\n", sep="")
    cat("Wave Number: ", toString(i), " Better Error: ", round(100*betterError), "\n", sep="")
    cat("\n", sep="")
  }
  cat("Wave Number: ", toString(i), " Number of rows: ", nrow(test), "\n", sep="")
  cat("Wave Number: ", toString(i), " Percent Yes: ", mean(test[["decW"]]), "\n", sep="")  
  cat("Wave Number: ", toString(i), " Average Error: ", round(100*errorSum/num), "\n", sep="")
  cat("Wave Number: ", toString(i), " Median Error: ", round(100*median(errors)), "\n", sep="")
  cat("Wave Number: ", toString(i), " Better Error: ", round(100*betterError), "\n", sep="")
  cat("\n", sep="")
  
}

