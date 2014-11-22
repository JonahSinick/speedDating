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

merged = read.csv('~/Desktop/speedDating/mergedRFProbsDecWAdded.csv')
decWs = n[grep("DecW",n)]
n = names(merged)

getProbs = function(train, test, features, tar){
  tryCosts = 10^(c(-1))
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

waves = unique(merged[["wave"]])
baselineDecW = c("raterDecAvgW", "decAvgM","avgWaveDecW")
merged["RFProbsDecW"] = rowSums(merged[335:576])/(length(names((merged[335:576]))))
bestFeaturesDecW = c("RFProbsDecW")

finalSlice = merged[c("wave", "decW", baselineDecW, decWs)]
finalSlice[c("baselineDecW", "bestGuessDecW")] = 0

for(i in waves){
  trainWaves = waves[!(waves==i)] 
  train = finalSlice[finalSlice["wave"] != i,]
  test = finalSlice[finalSlice["wave"] == i,]
  finalSlice[finalSlice["wave"] == i,][["baselineDecW"]] =  getProbs(train, test, baselineDecW, "decW")
  finalSlice[finalSlice["wave"] == i,][["bestGuessDecW"]] =  getProbs(train, test, bestFeaturesDecW, "decW")
}
for(i in waves){
  for(j in waves){
    string = paste(paste("RFTest",toString(i),sep="_"),toString(j),sep="_")
    slice = merged[merged[["wave"]]%in% c(i,j),]
  }
}
accuracyFrame(finalSlice[c("decW", "baselineDecW","bestGuessDecW")], "decW")

for()