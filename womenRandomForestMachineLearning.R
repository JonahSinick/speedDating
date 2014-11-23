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


merged[c("RFTempProbsDecW","RFProbsDecW", "RFPredictionsDecW")] = 0
features = c(ratingsDecW, menActs, womenActs, womenPrefs, menPrefs, wealthInds, "impraceW", "impreligW",
             "dateW", "dateM", "exphappyW", "goOutM", "goOutW")
mergedForWomen = merged[c("wave", "decW", features)]
merged[testProbsColName] = 0
waves = unique(merged[["wave"]])
for(i in 1:length(waves)){
  for(j in (i + 1):length(waves)){
    waveStr = paste(toString(waves[i]),toString(waves[j]),sep="_")
    print(waveStr)
    trainWaves = waves[!(waves %in% waves[c(i,j)])]
    testWaves = waves[(waves %in% waves[c(i,j)])]
    testProbsColName = paste("RFTest",waveStr,sep="_")
    merged[testProbsColName] = 0
    tempTrain = mergedForWomen[mergedForWomen[["wave"]] %in% trainWaves,]
    tempTest = mergedForWomen[(mergedForWomen[["wave"]] %in% testWaves),]
    rfForTrain = randomForest(y=as.factor(tempTrain[,"decW"]), x=tempTrain[features], importance=TRUE, ntree=500)
    merged[merged[["wave"]] %in% testWaves,][[testProbsColName]] = predict(rfForTrain, tempTest, type="prob")[,"1"]
  }
}

for(i in 1:length(waves)){
  for(j in 1:length(waves)){
    if(j < i){
      waveStr1 = paste(toString(waves[j]),toString(waves[i]),sep="_")
      waveStr2 = paste(toString(waves[i]),toString(waves[j]),sep="_")
      oldColName = paste("RFTest",waveStr1,sep="_")
      newColName = paste("RFTest",waveStr2,sep="_")
      merged[newColName] = merged[oldColName]
    }
  }
}


merged["RFProbsDecW"] = 1
for(i in waves){
  for(j in waves){
    if(j != i){
      print(c(i,j))
      waveStr = paste(toString(i),toString(j),sep="_")
      colName =  paste("RFTest",waveStr,sep="_")
      merged[[colName]] = ifelse(merged[[colName]] == 0, 0.01, merged[[colName]])
      merged[[colName]] = ifelse(merged[[colName]] == 1, 0.99, merged[[colName]])
      merged[merged["wave"] == i,][["RFProbsDecW"]]  = merged[merged["wave"] == i,][["RFProbsDecW"]]*(merged[merged["wave"] == i,][[colName]]/(1 - merged[merged["wave"] == i,][[colName]]))
    }
  }
}
merged["RFProbsDecW"] = (merged["RFProbsDecW"])^(1/(length(waves) - 1))
merged["RFProbsDecW"] = (merged["RFProbsDecW"])/(merged["RFProbsDecW"] + 1)


       
write.csv(merged, '~/Desktop/speedDating/mergedRFProbsDecWAdded.csv')
