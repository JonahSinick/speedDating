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


singSong = function(){
  print("Singing Song")
}