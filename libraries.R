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
library(arules)


LORsToProbs = function(LORs){
  ORs = exp(LORs)
  probs = ORs/(1 + ORs)
  return(probs)
}

LORColsToProbs = function(df, LORs){
  for(LOR in LORs){
    df[[LOR]] = LORsToProbs(df[[LOR]])
  }
  return(df)
}

adjustProbs = function(probs,threshold=0.01){
  probs = ifelse(probs < threshold,threshold, probs)
  probs = ifelse(probs > 1 - threshold, 1 - threshold, probs)
  return(probs)
}
probColsToLORs = function(df, probNames){
  for(probs in probNames){
    df[[probs]] = probsToLORs(df[[probs]])
  }
  return(df)
}
probsToLORs = function(probs){
  probs = adjustProbs(probs)
  LORs = log(probs/(1 - probs))
  return(LORs)
}



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
niceCors = function(df, rows, cols, multiplier = 100){
  cors = round(multiplier*cor(df[rows],df[cols]))
  return(cors)
}

makeCorDF = function(df, nameHash, colNames, target){
  corDF = data.frame()
  for(key in keys(nameHash)){
    corDF[key,] = 0    
  }
  corDF[colNames] = 0
  for(key in keys(nameHash)){
    replacements = c(matrix(niceCors(df, nameHash[[key]], c(target))))
    for(i in 1:length(replacements)){
      print(corDF)
      corDF[key,i] = replacements[i]
    }
  }
  return(corDF)
}