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

merged = read.csv( '~/Desktop/speedDating/mergedProbsAddedFinal.csv')

merged[n[grep("AvgRating", n)][1:7]] = merged[n[grep("AvgRating", n)][1:7]] - colSums((merged[n[grep("AvgRating", n)][1:7]]))/nrow(merged)
merged[n[grep("AvgRating", n)][8:14]] = merged[n[grep("AvgRating", n)][8:14]] - colSums((merged[n[grep("AvgRating", n)][8:14]]))/nrow(merged)

merged["totalRatingM"] = rowSums(merged[n[grep("AvgRating", n)][1:7]])
merged["totalRatingW"] = rowSums(merged[n[grep("AvgRating", n)][8:14]])

cor(merged[c(n[grep("AvgRating", n)][1:7],"totalRatingM")], merged["decW"])

for(name in n[grep("AvgRating", n)][1:7]){
  merged[paste(name,"Adj",sep="")] = merged[name] - merged["totalRatingM"]/7
}
for(name in n[grep("AvgRating", n)][8:14]){
  merged[paste(name,"Adj",sep="")] = merged[name] - merged["totalRatingW"]/7
}

merged[n[grep("AvgRating", n)][1:7]] = merged[n[grep("AvgRating", n)][1:7]] - merged["totalRatingM"] 

for(w in unique(merged[["wave"]])){
  iidWs = unique((merged[merged["wave"] == w,][["iidW"]]))
  iidMs = unique((merged[merged["wave"] == w,][["iidM"]]))
  print(c(length(iidWs),length(iidMs)))
}
waves = c(7, 9, 11, 12, 19, 21, 4)


for(wave in waves){
  print(wave)
  print(mean(merged[merged["wave"] == wave,][["match"]]))
}
round(100*cor(merged, merged["raterDecLORW"]))
scrambled = merged[sample(1:nrow(merged)),]
train = scrambled[1:floor(nrow(merged)/2),]
test = scrambled[(floor(nrow(merged)/2) + 1):nrow(merged),]


printMetrics = function(target,probs,cutoff=0.5){
  guesses = ifelse(probs > cutoff,1,0)
  TP = sum(ifelse(guesses == 1 & target == 1, 1,0))
  FP = sum(ifelse(guesses == 1 & target == 0, 1,0))
  TN = sum(ifelse(guesses == 0 & target == 0, 1,0))
  FN = sum(ifelse(guesses == 0 & target == 1, 1,0))
  ER = (FP + FN)/length(target)
  FPER = FP/(FP + TP)
  FNER = FN/(FN + TN)
  fracYesFound = (TP)/(TP + FN)
  logLoss = logLoss(target,probs)
  cat("Log Loss: ", round(logLoss,3) , 
      " Error Rate: ", (100*round(ER,3)),
      "% False Pos Rate: ", (100*round(FPER,3)), 
      "% False Neg Rate: ", (100*round(FNER,3)),
      "% Frac Yes Found: ", (100*round(fracYesFound,3)),
      "%\n", sep="")
  print(table(guesses,target))
}
merged["wildGuesses"] = 0
for(i in 1:nrow(merged)){
  set.seed(i); merged[i,"wildGuesses"] = sample(1:1000, 1)/1000
}
merged[["wildGuesses2"]] = ifelse(merged[["wildGuesses"]] >1, 1, 0)
printMetrics(merged[["match"]], merged[["wildGuesses2"]])
printMetrics(merged[["match"]], merged[["genericConjProb"]], cutoff = 0.36)
printMetrics(merged[["match"]], merged[["matchGuess"]], cutoff = 0.36)

