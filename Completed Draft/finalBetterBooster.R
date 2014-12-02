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

getProbs = function(train, test, features, tar, costTryType){
  bestLogLoss = 1000
  bestProbs = NA
  s=scale(train[features],center=TRUE,scale=TRUE)
  if(costTryType == "heuristic"){
    tryCosts = c(heuristicC(s))
  } else{
    tryCosts = 10^(seq(-2,0,by=0.1))
  }
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

n = names(merged)



probsToLORs = function(probs){
  probs = ifelse(probs < 0.03, 0.03, probs)
  probs = ifelse(probs > 0.97, 0.97, probs)
  ORs = probs/(1 - probs)
  return(log(ORs))
}

LORsToProbs = function(LORs){
  ORs = exp(LORs)
  probs = ORs/(1 + ORs)
  return(probs)
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


populateDecsInner = function(train,test){
  menBase = c("raterDecLORM", "decLORW", "attrAvgRatingW", "fieldsCrossLORDecM", "likeAvgRatingSignedDiff", "datesCrossLORDecM",
              "gamingMiscActSignedDiff", "sharAvgRatingAbsDiff")
  womenBase = c("raterDecLORW", "decLORM", "attrAvgRatingM", "fieldsCrossLORDecW", "attrAvgRatingAbsDiff",
                "datesCrossLORDecW", "goalsCrossLORDecW", "funAvgRatingAbsDiff", "raterDecLORM", "careersCrossLORDecW")  
  genDecMProb = getProbs(train, test,  c(menBase[1:3], "avgWaveDecM"), "decM","heuristic")
  refDecMProb = getProbs(train, test,  menBase, "decM","heuristic")
  genDecWProb = getProbs(train, test, womenBase[c(1:3,9)], "decW","heuristic")
  refDecWProb = getProbs(train, test, womenBase, "decW","heuristic")
  
  test[["genericDecMProb"]] = test[["genericDecMProb"]] +  probsToLORs(genDecMProb)
  test[["refinedDecMProb"]] = test[["refinedDecMProb"]] + probsToLORs(refDecMProb)
  test[["genericDecWProb"]] = test[["genericDecWProb"]] +  probsToLORs(genDecWProb)
  test[["refinedDecWProb"]] = test[["refinedDecWProb"]] +  probsToLORs(refDecWProb)  
  return(test[c("genericDecMProb", "refinedDecMProb", "genericDecWProb","refinedDecWProb")])
}

populateMatches = function(train, test){
  matchGuessFeatures = c("refinedConjProb", "attrAvgRatingAbsDiff", "goalsWomanLORDecM", 
                         "raterDecLORAbsDiff", "yogaPhysActM", "goalsCrossLORDecW")
  test[["genericConjProbCal"]] = test[["genericConjProbCal"]] +  probsToLORs(getProbs(train, test, c("genericConjProb"), "match","heuristic"))  
  test[["refinedConjProbCal"]] = test[["refinedConjProbCal"]] +  probsToLORs(getProbs(train, test, c("refinedConjProb"), "match","heuristic"))  
  test[["matchGuess"]] = test[["matchGuess"]] +  probsToLORs(getProbs(train, test, matchGuessFeatures, "match","heuristic"))
  return(test[c("genericConjProbCal", "refinedConjProbCal", "matchGuess")])
}



populateProbs = function(newMerged, numTimes1, numTimes2){
  rows = 1:nrow(newMerged)
  numRows = nrow(newMerged)
  scrambledIdxs = sample(rows)
  split = floor(numRows/10)
  probs1 = c("genericDecMProb", "refinedDecMProb", "genericDecWProb","refinedDecWProb")
  probs2 = c("genericConjProb", "refinedConjProb")
  probs3 = c("genericConjProbCal", "refinedConjProbCal", "matchGuess")
  probs = c(probs1,probs2,probs3)
  newMerged[probs] = 0
  for(i in 1:10){
    print(i)
    if(i < 10){
      testIdxs = scrambledIdxs[(1 + (i - 1)*split):(i*split)]
    }
    if(i == 10){
      testIdxs = scrambledIdxs[(1 + (i - 1)*split):numRows]
    }
    print(length(testIdxs))
    trainIdxs = scrambledIdxs[!(scrambledIdxs %in% testIdxs)]
    train = newMerged[trainIdxs,]
    test = newMerged[testIdxs,]  
    c("genericDecMProb", "refinedDecMProb", "genericDecWProb","refinedDecWProb")
    for(j in 1:numTimes1){
      print(c(i,j))
      train1Idxs = sample(1:nrow(train))[1:floor(nrow(train)/2)]
      train1 = train[(1:nrow(train) %in% train1Idxs),]
      train2 = train[!(1:nrow(train) %in% train1Idxs),]
      train[(1:nrow(train) %in% train1Idxs),][probs1] = populateDecsInner(train2, train1)[probs1]
      train[!(1:nrow(train) %in% train1Idxs),][probs1] = populateDecsInner(train1, train2)[probs1]
      test[probs1] = populateDecsInner(train1, test)[probs1]
      test[probs1] = populateDecsInner(train2, test)[probs1]
    }
    train[probs1] = train[probs1]/numTimes1
    test[probs1] = test[probs1]/(2*numTimes1)
    train[["genericConjProb"]] = probsToLORs(LORsToProbs(train[["genericDecMProb"]])*LORsToProbs(train[["genericDecWProb"]]))
    train[["refinedConjProb"]] = probsToLORs(LORsToProbs(train[["refinedDecMProb"]])*LORsToProbs(train[["refinedDecWProb"]]))
    test[["genericConjProb"]] = probsToLORs(LORsToProbs(test[["genericDecMProb"]])*LORsToProbs(test[["genericDecWProb"]]))
    test[["refinedConjProb"]] = probsToLORs(LORsToProbs(test[["refinedDecMProb"]])*LORsToProbs(test[["refinedDecWProb"]]))  
    for(j in 1:numTimes2){
      print(c(i,j))
      train1Idxs = sample(1:nrow(train))[1:floor(nrow(train)/2)]
      train1 = train[(1:nrow(train) %in% train1Idxs),]
      train2 = train[!(1:nrow(train) %in% train1Idxs),]
      test[probs3] = populateMatches(train1, test)[probs3]
      test[probs3] = populateMatches(train2, test)[probs3]
    }
    test[c(probs3)] = test[c(probs3)]/(2*numTimes2)
    newMerged[testIdxs,] = test
  }
  newMerged[probs] = LORsToProbs(newMerged[probs])
  
  return(newMerged)
}

probs = c("genericDecMProb", "refinedDecMProb", "genericDecWProb", 
          "refinedDecWProb", "genericConjProb", "refinedConjProb", "genericConjProbCal", 
          "refinedConjProbCal", "matchGuess")
newMerged = populateProbs(merged, 100, 100)

printMetrics(newMerged[["match"]],newMerged[[probs[7]]], cutoff=0.5)

write.csv(newMerged,  '~/Desktop/speedDating/mergedProbsAddedFinal.csv')

