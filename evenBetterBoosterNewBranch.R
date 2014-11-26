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
  return(test)
}


populateDecsOuter = function(df, numTimes1, probs){
  probs = c("genericDecMProb", "refinedDecMProb", "genericDecWProb", "refinedDecWProb")
  df[["tempIdxs"]]  = 1:nrow(df)
  for(j in 1:numTimes1){
    idxs = sample(df[["tempIdxs"]])
    idxHash = hash()
    idxHash[["1"]] = idxs[1:floor(1*nrow(df)/5)]
    idxHash[["2"]] = idxs[(floor(1*nrow(df)/5) + 1):floor(2*nrow(df)/5)]
    idxHash[["3"]] = idxs[(floor(2*nrow(df)/5) + 1):floor(3*nrow(df)/5)]
    idxHash[["4"]] = idxs[(floor(3*nrow(df)/5) + 1):floor(4*nrow(df)/5)]
    idxHash[["5"]] = idxs[(floor(4*nrow(df)/5) + 1):nrow(df)]
    for(key in keys(idxHash)){
      idxsNew = idxHash[[key]]
      test = df[df[["tempIdxs"]] %in% idxsNew,]
      train = df[!(df[["tempIdxs"]] %in% idxsNew),]
      modified = populateDecsInner(train,test)
      df[(df[["tempIdxs"]] %in% idxsNew),][probs] = df[(df[["tempIdxs"]] %in% idxsNew),][probs] + modified[probs]
    }  
    df[["genericConjProb"]] = probsToLORs(LORsToProbs(df[["genericDecMProb"]])*LORsToProbs(df[["genericDecWProb"]]))
    df[["refinedConjProb"]] = probsToLORs(LORsToProbs(df[["refinedDecMProb"]])*LORsToProbs(df[["refinedDecWProb"]]))
  }
  df[probs] = df[probs]/numTimes1

  df = df[, names(df) != "tempIdxs"]  
  return(df)
}
populateMatches = function(train, test){
  matchGuessFeatures = c("refinedConjProb", "attrAvgRatingAbsDiff", "goalsWomanLORDecM", 
                         "raterDecLORAbsDiff", "yogaPhysActM", "goalsCrossLORDecW")
  test[["genericConjProbCal"]] = test[["genericConjProbCal"]] +  probsToLORs(getProbs(train, test, c("genericConjProb"), "match","heuristic"))  
  test[["refinedConjProbCal"]] = test[["refinedConjProbCal"]] +  probsToLORs(getProbs(train, test, c("refinedConjProb"), "match","heuristic"))  

  test[["matchGuess"]] = test[["matchGuess"]] +  probsToLORs(getProbs(train, test, matchGuessFeatures, "match","heuristic"))
  return(test)
}



addProbsMiddle = function(df, numTimes1, numTimes2){
  probs = c("genericDecMProb", "refinedDecMProb", "genericDecWProb", 
            "refinedDecWProb", "genericConjProb", "genericConjProbCal", "refinedConjProb", 
            "refinedConjProbCal", "matchGuess")
  df[probs] = 0
  rows = 1:nrow(df)
  numRows = nrow(df)
  set.seed(430); scrambledIdxs = sample(rows, floor(numRows/2))
  df1 = df[rows %in% scrambledIdxs,]
  df2 = df[!(rows %in% scrambledIdxs),]
  df1 = populateDecsOuter(df1, numTimes1)
  df2 = populateDecsOuter(df2, numTimes1)  
  for(i in 1:numTimes2){
    train1 = df1[sample(1:nrow(df1), floor(4*nrow(df1)/5)),]
    train2 = df2[sample(1:nrow(df2), floor(4*nrow(df2)/5)),]
    
    df2 = populateMatches(train1, df2)
    df1 = populateMatches(train2, df1)
    df[rows %in% scrambledIdxs,] =  df1
    df[!(rows %in% scrambledIdxs),] = df2
  }
  
  df[probs] = df[probs]/numTimes2
  return(df)
}


addProbsOuter = function(df, numTimes1, numTimes2,numTimes3){
  probs = c("genericDecMProb", "refinedDecMProb", "genericDecWProb", 
            "refinedDecWProb", "genericConjProb", "genericConjProbCal", "refinedConjProb", 
            "refinedConjProbCal", "matchGuess")
  df[probs] = 0
  df[["oldIdxs"]] = 1:nrow(df)
  for(i in 1:numTimes3){
    newDF = addProbsMiddle(df, numTimes1, numTimes2)
    newDF = newDF[order(newDF["oldIdxs"]),]    
    df[probs] = df[probs] + newDF[probs]
  }
  df[probs] = df[probs]/numTimes3
  df[probs] = LORsToProbs(df[probs])
  return(df)
}


newMerged = addProbsOuter(merged, 1,10,10)
printMetrics(newMerged[["match"]],newMerged[["matchGuess"]])

printMetrics(newMerged[["decM"]],newMerged[["refinedDecMProb"]])
