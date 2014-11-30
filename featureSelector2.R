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



evenBetterBooster = function(df, base, tries, tar, numTimes,type){
  totalLogLoss = 0
  votes = hash()
  for(feature in tries){
    votes[[feature]] = 0
  }
  for(i in 1:numTimes){
    if(i %% 5 == 0){
      print(i)
    }
    idxs = sample(1:nrow(df))
    startIdx = 1
    midIdx = floor(2*(nrow(df)/3))
    trainIdxs = idxs[startIdx:midIdx]
    testIdxs = idxs[(midIdx + 1):nrow(df)]
    train = df[trainIdxs,]
    test = df[testIdxs,]
    baseLogLoss = logLoss(test[[tar]], getProbs(train, test, base,tar,type))
    totalLogLoss = baseLogLoss + totalLogLoss
    for(feature in tries){
      newLogLoss = logLoss(test[[tar]], getProbs(train, test, c(base,feature),tar,type))
      votes[[feature]] = votes[[feature]] + baseLogLoss -  newLogLoss 
    }
  }
  print((totalLogLoss/numTimes))
  for(feature in tries){
    votes[[feature]] = (votes[[feature]]/numTimes)
  }
  return(values(votes))
}


n = names(merged)


featureSelector = function(df, currentFeatures, features, target, numTries, finalLength){
  for(i in 1:finalLength){
    remainingFeatures = features
    cat("Length of current features: ", length(currentFeatures), " Length of remaining features: ", length(remainingFeatures), "\n",sep="")
    print(currentFeatures)
    votes = evenBetterBooster(merged, currentFeatures, remainingFeatures, target, numTries, "heuristic")
    if(length(votes) > 0){
      sorted = sort(10000*votes, decreasing= TRUE)
      remainingFeatures = names(sorted[sorted > 0])
      if(length(features) > 0){
        currentFeatures[length(currentFeatures) + 1] = remainingFeatures[[1]]
      } 
    }
  }
  return(currentFeatures)
}


menTraits = n[grep("M$",n)][c(11:19,30:52,54:59,106:121)]
womenTraits = gsub("M$", "W", menTraits)
crosses = n[grep("Cross$", n)]
diffs = n[grep("Diff$",n)][c(17:114,137:138)]


features = c(menTraits, womenTraits, diffs, crosses)
menBase = c("raterDecLORM", "decLORM", "attrAvgRatingW")
womenBase = c("raterDecLORW", "decLORM", "attrAvgRatingM", "avgWaveDecW", 
              "fieldsCrossLORDecW", "datesCrossAvgDecW", "careersCrossLORDecW",
              "attrAvgRatingAbsDiff", "racesCrossAvgDecW")
newFeatures = featureSelector(merged, menBase, features, "decM", 50, 10)




votes = evenBetterBooster(merged, womenBase[1:9], c(), "decW", 100, "heuristic")
sorted = sort(10000*values(votes),decreasing= TRUE)
sorted
tries = names(sorted)[1:90]
newTries = names(sorted)[1:20]

n = names(merged)

n = names(merged)

menTraits = n[grep("M$",n)][c(11:19,30:52,54:59,88:121)]
womenTraits = gsub("M$", "W", menTraits)
matchTraits = n[grep("Match$",n)]
matchTraits = matchTraits[grep( "LOR",matchTraits)]
diffs = n[grep("Diff$",n)][c(17:114,137:138)]


features =  c(matchTraits, diffs)
matchBase = c("refinedConjProb", "fieldsCrossLORMatch", "careersCrossLORMatch", "goalsCrossLORMatch",
              "likeAvgRatingAbsDiff", "goOutAbsDiff", "attrAvgRatingAbsDiff")
votes = evenBetterBooster(newMerged, matchBase, tries, "match", 100, "heuristic")
sorted = sort(10000*values(votes),decreasing= TRUE)
sorted
tries = names(sorted)[1:11]



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
  menBase = c("raterDecLORM", "decLORW", "attrAvgRatingW", "likeAvgRatingSignedDiff", "fieldsCrossLORDecM")
  womenBase = c("raterDecLORW", "decLORM", "attrAvgRatingM", "avgWaveDecW", 
                "fieldsCrossLORDecW", "datesCrossAvgDecW", "careersCrossLORDecW",
                "attrAvgRatingAbsDiff", "racesCrossAvgDecW")
  genDecMProb = getProbs(train, test,  c(menBase[1:3], "avgWaveDecM"), "decM","heuristic")
  refDecMProb = getProbs(train, test,  menBase, "decM","heuristic")
  genDecWProb = getProbs(train, test, womenBase[c(1:4)], "decW","heuristic")
  refDecWProb = getProbs(train, test, womenBase, "decW","heuristic")
  
  test[["genericDecMProb"]] = test[["genericDecMProb"]] +  probsToLORs(genDecMProb)
  test[["refinedDecMProb"]] = test[["refinedDecMProb"]] + probsToLORs(refDecMProb)
  test[["genericDecWProb"]] = test[["genericDecWProb"]] +  probsToLORs(genDecWProb)
  test[["refinedDecWProb"]] = test[["refinedDecWProb"]] +  probsToLORs(refDecWProb)  
  return(test[c("genericDecMProb", "refinedDecMProb", "genericDecWProb","refinedDecWProb")])
}

populateMatches = function(train, test){
  matchGuessFeatures = c("refinedConjProb", "fieldsCrossLORMatch", "careersCrossLORMatch", "goalsCrossLORMatch",
                         "likeAvgRatingAbsDiff", "goOutAbsDiff", "attrAvgRatingAbsDiff")
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
      trainNewIdxs = sample(1:nrow(train))[1:floor(1*nrow(train)/2)]
      trainNew = train[(1:nrow(train) %in% trainNewIdxs),]
      test[probs1] = populateDecsInner(trainNew, test)[probs1]
    }
    train[probs1] = train[probs1]/numTimes1
    test[probs1] = test[probs1]/(numTimes1)
    train[["genericConjProb"]] = probsToLORs(LORsToProbs(train[["genericDecMProb"]])*LORsToProbs(train[["genericDecWProb"]]))
    train[["refinedConjProb"]] = probsToLORs(LORsToProbs(train[["genericDecMProb"]])*LORsToProbs(train[["refinedDecWProb"]]))
    test[["genericConjProb"]] = probsToLORs(LORsToProbs(test[["genericDecMProb"]])*LORsToProbs(test[["genericDecWProb"]]))
    test[["refinedConjProb"]] = probsToLORs(LORsToProbs(test[["genericDecMProb"]])*LORsToProbs(test[["refinedDecWProb"]]))  
    for(j in 1:numTimes2){
      print(c(i,j))
      train1Idxs = sample(1:nrow(train))[1:floor(1*nrow(train)/2)]
      trainNew = train[(1:nrow(train) %in% train1Idxs),]
      test[probs3] = populateMatches(trainNew, test)[probs3]
    }
    test[c(probs3)] = test[c(probs3)]/(numTimes2)
    newMerged[testIdxs,] = test
  }
  newMerged[probs] = LORsToProbs(newMerged[probs])
  
  return(newMerged)
}

probs = c("genericDecMProb", "refinedDecMProb", "genericDecWProb", 
          "refinedDecWProb", "genericConjProb", "refinedConjProb", "genericConjProbCal", 
          "refinedConjProbCal", "matchGuess")
newMerged = populateProbs(merged, 100, 100)


printMetrics(newMerged[["decM"]], newMerged[[probs[1]]])
printMetrics(newMerged[["decM"]], newMerged[[probs[2]]])
printMetrics(newMerged[["decW"]], newMerged[[probs[3]]])
printMetrics(newMerged[["decW"]], newMerged[[probs[4]]])
printMetrics(newMerged[["match"]], newMerged[[probs[5]]])
printMetrics(newMerged[["match"]], newMerged[[probs[7]]])

printMetrics(newMerged[["match"]], newMerged[[probs[8]]])
printMetrics(newMerged[["match"]], newMerged[[probs[9]]])

write.csv(newMerged, '~/Desktop/speedDating/mergedProbsAddedFinal.csv')
 