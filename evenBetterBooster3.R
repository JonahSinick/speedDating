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
    if(i %% 10 == 0){
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
  return(votes)
}


n = names(merged)

menTraits = n[grep("M$",n)][c(11:19,30:52,54:59,97:113)]
womenTraits = gsub("M$", "W", menTraits)
crosses = n[grep("Cross$",n)]
diffs = n[grep("Diff$",n)][17:148]
features = c(menTraits, womenTraits, crosses, diffs)
menBase = c("raterDecLORM", "decLORW", "attrAvgRatingW", "fieldsCrossLORDecM", "likeAvgRatingSignedDiff", "datesCrossLORDecM",
            "gamingMiscActSignedDiff", "sharAvgRatingAbsDiff")
# bestFeatures = evenBetterBooster(merged,menBase, c("sharAvgRatingAbsDiff"), "decM", 10,"heuristic")
10000*round(sort(values(bestFeatures), decreasing=TRUE),4)

newNames = names(sort(values(bestFeatures), decreasing=TRUE))[1:55] 

#..............

womenBase = c("raterDecLORW", "decLORM", "attrAvgRatingM", "fieldsCrossLORDecW", "attrAvgRatingAbsDiff",
              "datesCrossLORDecW", "goalsCrossLORDecW", "funAvgRatingAbsDiff", "raterDecLORM", "careersCrossLORDecW")
# bestFeatures = evenBetterBooster(merged,womenBase[c(1:7)], newNames[c(3,5,6:10)], "decW", 300,"heuristic")
10000*round(sort(values(bestFeatures), decreasing=TRUE),4)

newNames = names(sort(values(bestFeatures), decreasing=TRUE))[1:10]


boostedPredictor = function(df, features, tar, numTimes, numChunks=10){
  df["answer"] = 0
  splits = c(seq(1, floor(nrow(df)/numChunks),by=numChunks), (nrow(df)) - 1)
  for(i in 1:numChunks){
    print(i)
    idxs = 1:nrow(df)
    testSeg = seq(splits[i],(splits[i+1] - 1),by=1)
    trainSeg = idxs[!(idxs %in% testSeg)]
    test = df[testSeg,]
    train = df[trainSeg,]
    for(j in 1:numTimes){
      idxs = sample(1:nrow(train))
      startIdx = 1
      midIdx = floor(1*(nrow(df)/2))
      train1 = train[startIdx:midIdx,]
      train2 = train[(midIdx + 1):nrow(train),]
      testProbs1 = getProbs(train1, test, features, tar,"heuristic")
      testProbs2 = getProbs(train2, test, features, tar,"heuristic")
      df[testSeg,][["answer"]] =  df[testSeg,][["answer"]]  + probsToLORs(testProbs1) + probsToLORs(testProbs2)
    }
  }
  df[["answer"]] = df[["answer"]]/(2*numTimes)
  df[["answer"]] = LORsToProbs(df[["answer"]])
  printMetrics(df[[tar]],df[["answer"]])
  return(df[["answer"]])
}

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
      " Error Rate: ", round(ER,3),
      " False Pos Rate: ", round(FPER,3), 
      " False Neg Rate: ", round(FNER,3),
      " Frac Yes Found: ", round(fracYesFound,3),
      "\n", sep="")
  print(table(guesses,target))
}




menBase = c("raterDecLORM", "decLORW", "attrAvgRatingW", "fieldsCrossLORDecM", "likeAvgRatingSignedDiff", "datesCrossLORDecM",
            "gamingMiscActSignedDiff", "sharAvgRatingAbsDiff")

womenBase = c("raterDecLORW", "decLORM", "attrAvgRatingM", "fieldsCrossLORDecW", "attrAvgRatingAbsDiff",
              "datesCrossLORDecW", "goalsCrossLORDecW", "funAvgRatingAbsDiff", "raterDecLORM", "careersCrossLORDecW")

featureHash = hash()
featureHash[["genericDecM"]] = c(menBase[1:3], "avgWaveDecM")
featureHash[["genericDecW"]] = c(womenBase[c(1:3,9)], "avgWaveDecW")
featureHash[["refinedDecM"]] = menBase
featureHash[["refinedDecW"]] = womenBase
featureHash[["matchGuess"]] = c("refinedConjProb", "attrAvgRatingAbsDiff", 
                                "goalsWomanLORDecM", "raterDecLORAbsDiff",
                                "yogaPhysActM", "goalsCrossLORDecW")

addProbs = function(df, featureHash, numTimes){
  probs = c("genericDecMProb", "refinedDecMProb", "genericDecWProb", 
            "refinedDecWProb", "genericConjProb", "refinedConjProb", 
            "refinedConjProbCal", "matchGuess")
  df[probs] = 0
  numRows = nrow(df)
  split = floor(numRows/10)
  set.seed(427); scrambledIdxs = sample(1:nrow(df))
  splits = c(seq(1,nrow(df),by=split),numRows + 1)
  print(splits)
  testIdxs = hash()
  trainIdxs = hash()
  for(i in 1:11){
    testIdxs[[toString(i)]] = scrambledIdxs[splits[i]:(splits[i + 1] - 1)]
    trainIdxs[[toString(i)]] = scrambledIdxs[!(scrambledIdxs %in% testIdxs[[toString(i)]])]
  }
  for(i in 1:11){
    train = df[trainIdxs[["i"]],]
    test = df[testIdxs[["i"]],]
    print(nrow(test))
    print(nrow(train))
    for(j in 1:numTimes){
      idxs = sample(1:nrow(train))
      startIdx = 1
      midIdx = floor(1*(nrow(train)/2))
      train1 = train[startIdx:midIdx,]
      train2 = train[(midIdx + 1):nrow(train),]
      test[["genericDecMProb"]] = test[["genericDecMProb"]] +  probsToLORs(getProbs(train1, test, featureHash[["genericDecM"]], "decM","heuristic"))
      print("Here")
      test[["refinedDecMProb"]] = test[["refinedDecMProb"]]  + probsToLORs(getProbs(train1, test, featureHash[["refinedDecM"]], "decM","heuristic"))
      test[["genericDecWProb"]] = test[["genericDecWProb"]] + probsToLORs(getProbs(train1, test, featureHash[["genericDecW"]], "decW","heuristic"))
      test[["refinedDecWProb"]] = test[["refinedDecWProb"]] +  probsToLORs(getProbs(train1, test, featureHash[["refinedDecW"]], "decW","heuristic"))
      test[["refinedConjProb"]] = test[["refinedConjProb"]] + probsToLORs(LORsToProbs(test[["refinedDecMProb"]])*LORsToProbs(test[["refinedDecWProb"]]))
      
      train2[["genericDecMProb"]] = probsToLORs(getProbs(train1, train2, featureHash[["genericDecM"]], "decM","heuristic"))
      train2[["refinedDecMProb"]] = probsToLORs(getProbs(train1, train2, featureHash[["refinedDecM"]], "decM","heuristic"))
      train2[["genericDecWProb"]] = probsToLORs(getProbs(train1, train2, featureHash[["genericDecW"]], "decW","heuristic"))
      train2[["refinedDecWProb"]] = probsToLORs(getProbs(train1, train2, featureHash[["refinedDecW"]], "decW","heuristic"))
      
      
      train2[["refinedConjProb"]] = probsToLORs(LORsToProbs(train2[["refinedDecMProb"]])*LORsToProbs(train2[["refinedDecWProb"]]))
      
      test[["refinedConjProbCal"]] = test[["refinedConjProbCal"]] +  probsToLORs(getProbs(train2, test, c("refinedConjProb"), "match","heuristic"))
      test[["matchGuess"]] = test[["matchGuess"]] +  probsToLORs(getProbs(train2, test, featureHash[["matchGuess"]], "match","heuristic"))
    }
    df[testIdxs,probs] =  test[probs]
  }
  return(df)
}


merged = addProbs(merged, featureHash, 1)
# 
# 
# 
# 
# merged[["genericDecMProb"]] = boostedPredictor(merged,genericDecM, "decM", 2)
# merged[["genericDecWProb"]] = boostedPredictor(merged,genericDecW, "decW", 400)
# merged[["genericConjProb"]] = merged[["genericDecMProb"]]*merged[["genericDecWProb"]]
# 
# 
# merged[["refinedDecMProb"]] = boostedPredictor(merged,refinedDecM, "decM", 400)
# 
# 
# merged[["refinedDecWProb"]] = boostedPredictor(merged,refinedDecW, "decW", 400)
# 
# 
# merged[["refinedConjProb"]] = merged[["refinedDecMProb"]]*merged[["refinedDecWProb"]]
# 
# 
# 
# merged[["genericConjProbCal"]] = boostedPredictor(merged,c("genericConjProb"), "match", 100)
# merged[["refinedConjProbCal"]] = boostedPredictor(merged,c("refinedConjProb"), "match", 100)
# 
# 
# votes = evenBetterBooster(merged,"refinedConjProbCal", c(mergedBase[2:6]), "match", 400,"heuristic")
# 10000*round(sort(values(votes), decreasing=TRUE),4)
# 
# 
