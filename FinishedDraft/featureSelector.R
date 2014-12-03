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



populateDecsInner = function(train,test){
  menBase = c("raterDecLORM", "decLORW", "attrAvgRatingW", "LORWaveDecAbsDiff")
  
  menFeatures = c(menBase, "fieldsCrossLORDecM", "attrAvgRatingAbsDiff", "decLORSignedDiff","careersCrossLORDecM", "datesCrossLORDecM")
  womenBase = c("raterDecLORW", "decLORM", "attrAvgRatingM", "avgWaveDecW")
  womenFeatures= c(womenBase, "fieldsCrossLORDecW", "datesCrossAvgDecW", "careersCrossLORDecW", "attrAvgRatingAbsDiff", "racesCrossAvgDecW",
               "raceWhiteM_raceAsianWCross", "expnumAbsDiff", "fieldsCrossLORDecM")
  genDecMProb = getProbs(train, test,  menBase, "decM","heuristic")
  refDecMProb = getProbs(train, test,  menFeatures, "decM","heuristic")
  genDecWProb = getProbs(train, test, womenBase, "decW","heuristic")
  refDecWProb = getProbs(train, test, womenFeatures, "decW","heuristic")
  
  test[["genericDecMProb"]] = test[["genericDecMProb"]] +  probsToLORs(genDecMProb)
  test[["refinedDecMProb"]] = test[["refinedDecMProb"]] + probsToLORs(refDecMProb)
  test[["genericDecWProb"]] = test[["genericDecWProb"]] +  probsToLORs(genDecWProb)
  test[["refinedDecWProb"]] = test[["refinedDecWProb"]] +  probsToLORs(refDecWProb)  
  return(test[c("genericDecMProb", "refinedDecMProb", "genericDecWProb","refinedDecWProb")])
}

populateMatches = function(train, test){
  matchGuessFeatures = c("refinedConjProb", "goalsCrossLORMatch",  "careersCrossLORMatch",   "likeAvgRatingAbsDiff",   "fieldsCrossLORMatch")
  
  test[["genericConjProbCal"]] = test[["genericConjProbCal"]] +  probsToLORs(getProbs(train, test, c("genericConjProb"), "match","heuristic"))  
  test[["refinedConjProbCal"]] = test[["refinedConjProbCal"]] +  probsToLORs(getProbs(train, test, c("refinedConjProb"), "match","heuristic"))  
  test[["matchGuess"]] = test[["matchGuess"]] +  probsToLORs(getProbs(train, test, matchGuessFeatures, "match","heuristic"))
  return(test[c("genericConjProbCal", "refinedConjProbCal", "matchGuess")])
}



featureSelector = function(df, startingFeatures, features, target, numTries, finalLength, fractionTrain){
  currentFeatures = startingFeatures
  remainingFeatures = features
  for(i in 1:(finalLength - length(startingFeatures))){
    
    cat("Length of current features: ", length(currentFeatures), " Length of remaining features: ", length(remainingFeatures), "\n",sep="")
    print(currentFeatures)
    votes = evenBetterBooster(df, currentFeatures, remainingFeatures, target, numTries, "heuristic", fractionTrain)
    if(length(votes) >= 1){
      sorted = sort(10000*votes, decreasing= TRUE)
      print(sorted)
      remainingFeatures = names(sorted[sorted >= 1])
      if(length(features) > 0 ){
        currentFeatures[length(currentFeatures) + 1] = remainingFeatures[[1]]
      } 
    }
  }
  h = hash()
  h[["remainingFeatures"]] = remainingFeatures
  h[["currentFeatures"]] = currentFeatures
  return(h)
}



populateProbs = function(newMerged, numTimes1, numTimes2, fractionTrain){
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
    for(j in 1:numTimes1){
      print(c(i,j))
      train1Idxs = sample(1:nrow(train))[1:floor(fractionTrain*nrow(train))]
      train1 = train[(1:nrow(train) %in% train1Idxs),]
      train2 = train[!(1:nrow(train) %in% train1Idxs),]
      train[(1:nrow(train) %in% train1Idxs),][probs1] = populateDecsInner(train2, train1)[probs1]
      train[!(1:nrow(train) %in% train1Idxs),][probs1] = populateDecsInner(train1, train2)[probs1]      
      trainNewIdxs = sample(1:nrow(train))[1:floor(1*nrow(train)/2)]
      trainNew = train[(1:nrow(train) %in% trainNewIdxs),]
      test[probs1] = populateDecsInner(trainNew, test)[probs1]
    }
    train[probs1] = train[probs1]/numTimes1
    test[probs1] = test[probs1]/(numTimes1)
    train[["genericConjProb"]] = probsToLORs(LORsToProbs(train[["genericDecMProb"]])*LORsToProbs(train[["genericDecWProb"]]))
    train[["refinedConjProb"]] = probsToLORs(LORsToProbs(train[["refinedDecMProb"]])*LORsToProbs(train[["refinedDecWProb"]]))
    test[["genericConjProb"]] = probsToLORs(LORsToProbs(test[["genericDecMProb"]])*LORsToProbs(test[["genericDecWProb"]]))
    test[["refinedConjProb"]] = probsToLORs(LORsToProbs(test[["refinedDecMProb"]])*LORsToProbs(test[["refinedDecWProb"]]))  
    for(j in 1:numTimes2){
      print(c(i,j))
      train1Idxs = sample(1:nrow(train))[1:floor(fractionTrain*nrow(train))]
      trainNew = train[(1:nrow(train) %in% train1Idxs),]
      test[probs3] = populateMatches(trainNew, test)[probs3]
    }
    test[c(probs3)] = test[c(probs3)]/(numTimes2)
    newMerged[testIdxs,] = test
  }
  newMerged[probs] = LORsToProbs(newMerged[probs])
  
  return(newMerged)
}

n = names(merged)
menTraits = n[grep("M$",n)][c(11:19,30:52,54:59,106:121)]
womenTraits = gsub("M$", "W", menTraits)
matchTraits = matchTraits[grep( "LOR",matchTraits)]
crosses = n[grep("Cross$", n)]
diffs = n[grep("Diff$",n)][c(17:114,137:138)]


myFeatures = c(menTraits, womenTraits, diffs, crosses)
menBase = c("raterDecLORM", "decLORW", "attrAvgRatingW")

menBase = c(menBase, c("LORWaveDecAbsDiff",
                       "fieldsCrossLORDecM",
                       "attrAvgRatingAbsDiff", 
                       "decLORSignedDiff",
                       "careersCrossLORDecM",
                       "datesCrossLORDecM"))
selectedFeaturesM = featureSelector(df = merged, startingFeatures = menBase, 
                              features= myFeatures, target = "decM", numTries = 10, 
                              finalLength = 10, fractionTrain = 0.66)



womenBase = c("raterDecLORW", "decLORM", "attrAvgRatingM", "avgWaveDecW")



selectedFeaturesW = featureSelector(df = merged, startingFeatures = womenBase, 
                              features= myFeatures, target = "decW", numTries = 300, 
                              finalLength = 20, fractionTrain = 0.66)

newMerged = populateProbs(merged, 100, 100, 0.66)
printMetrics(newMerged[["decM"]], newMerged[["genericDecMProb"]])
printMetrics(newMerged[["decM"]], newMerged[["refinedDecMProb"]])
printMetrics(newMerged[["decW"]], newMerged[["genericDecWProb"]])
printMetrics(newMerged[["decW"]], newMerged[["refinedDecWProb"]])

printMetrics(newMerged[["match"]], newMerged[["genericConjProbCal"]])
printMetrics(newMerged[["match"]], newMerged[["refinedConjProbCal"]])
printMetrics(newMerged[["match"]], newMerged[["matchGuess"]])

matchBase= c("refinedConjProb", "goalsCrossLORMatch",  "careersCrossLORMatch",   "likeAvgRatingAbsDiff",   "fieldsCrossLORMatch", "goOutAbsDiff")
matchStuff = n[grep("Match$",n)]
matchStuff = matchStuff[grep("LOR", matchStuff)]
matchFeatures = c(diffs, matchStuff[1:15], crosses)
selectedFeaturesMatch = featureSelector(df = newMerged, startingFeatures = matchBase, 
                                    features= c("goOutAbsDiff"), target = "match", numTries = 300, 
                                    finalLength = 20, fractionTrain = 0.66)

newMerged = populateProbs(newMerged, 25, 25, 0.66)
write.csv(newMerged, '~/Desktop/speedDating/mergedProbsAddedFinal.csv')
