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

#..............

womenBase = c("raterDecLORW", "decLORM", "attrAvgRatingM", "fieldsCrossLORDecW", "attrAvgRatingAbsDiff",
              "datesCrossLORDecW", "goalsCrossLORDecW", "funAvgRatingAbsDiff", "raterDecLORM", "careersCrossLORDecW")


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
      " Error Rate: ", (100*round(ER,3)),
      "% False Pos Rate: ", (100*round(FPER,3)), 
      "% False Neg Rate: ", (100*round(FNER,3)),
      "% Frac Yes Found: ", (100*round(fracYesFound,3)),
      "%\n", sep="")
  print(table(guesses,target))
}




populateDecsAndConjs = function(train,test){
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
  test[["genericConjProb"]] = test[["genericConjProb"]] +  probsToLORs(LORsToProbs(test[["genericDecMProb"]])*LORsToProbs(test[["genericDecWProb"]]))
  test[["refinedConjProb"]] = test[["refinedConjProb"]] +  probsToLORs(LORsToProbs(test[["refinedDecMProb"]])*LORsToProbs(test[["refinedDecWProb"]]))
  return(test)
}

populateTestProbs = function(train,test, numTimes, probs){
  train[["tempIdxs"]]  = 1:nrow(train)
  matchGuessFeatures = c("refinedConjProb", "attrAvgRatingAbsDiff", "goalsWomanLORDecM", 
                         "raterDecLORAbsDiff", "yogaPhysActM", "goalsCrossLORDecW")
  for(j in 1:numTimes){
    print(j)
    idxs = sample(train[["tempIdxs"]])
    split = floor(nrow(train)/2)
    train1 = train[train[["tempIdxs"]] %in% idxs[1:split],]
    train2 = train[train[["tempIdxs"]] %in% idxs[(split + 1):nrow(train)],]
    test = populateDecsAndConjs(train1,test)
    train2 = populateDecsAndConjs(train1,train2)
    test[["refinedConjProb"]] = test[["refinedConjProb"]]/j
    test[["genericConjProb"]] = test[["genericConjProb"]]/j
    test[["genericConjProbCal"]] = test[["genericConjProbCal"]] +  probsToLORs(getProbs(train2, test, c("genericConjProb"), "match","heuristic"))
    test[["refinedConjProbCal"]] = test[["refinedConjProbCal"]] +  probsToLORs(getProbs(train2, test, c("refinedConjProb"), "match","heuristic"))
    test[["matchGuess"]] = test[["matchGuess"]] +  probsToLORs(getProbs(train2, test, matchGuessFeatures, "match","heuristic"))
    test[["refinedConjProb"]] = j*test[["refinedConjProb"]]
    test[["genericConjProb"]] = j*test[["genericConjProb"]]
  }
  test[probs] = LORsToProbs((test[probs]/numTimes))
  return(test[probs])
}

addProbs = function(df, numTimes){
  probs = c("genericDecMProb", "refinedDecMProb", "genericDecWProb", 
            "refinedDecWProb", "genericConjProb", "genericConjProbCal", "refinedConjProb", 
            "refinedConjProbCal", "matchGuess")
  df[probs] = 0
  rows = 1:nrow(df)
  numRows = nrow(df)
  split = floor(numRows/10)
  set.seed(430); scrambledIdxs = sample(rows)
  splits = c(seq(1,numRows,by=split),numRows + 1)
  testIdxs = hash()
  trainIdxs = hash()
  checker = hash()
  for(i in 1:11){
    testIdxs[[toString(i)]] = scrambledIdxs[splits[i]:(splits[i + 1] - 1)]
    trainIdxs[[toString(i)]] = rows[!(rows %in% testIdxs[[toString(i)]])]
  }
  for(r in rows){
    checker[[toString(r)]] = 0
  }
  for(i in 1:11){
    cat("EYE!", i, "\n")
    train = df[trainIdxs[[toString(i)]],]
    test = df[testIdxs[[toString(i)]],]
    train[probs] = 0
    df[testIdxs[[toString(i)]],probs] =  populateTestProbs(train,test, numTimes, probs)
  }
  return(df)
}




probs = c("genericDecMProb", "refinedDecMProb", "genericDecWProb", 
          "refinedDecWProb", "genericConjProb","genericConjProbCal","refinedConjProb", 
          "refinedConjProbCal", "matchGuess")
mergedModifsimp = addProbs(merged, 1)
mergedModif = addProbs(merged, 100)
dfHash = hash()

dfHash[["1"]] = mergedModifsimp
dfHash[["100"]] = mergedModif
for(key in keys(dfHash)){
  print(key)
  df = dfHash[[key]]
  printMetrics(df[["decM"]],df[["genericDecMProb"]])
}
for(key in keys(dfHash)){
  print(key)
  df = dfHash[[key]]
  printMetrics(df[["decM"]],df[["refinedDecMProb"]])
}
for(key in keys(dfHash)){
  print(key)
  df = dfHash[[key]]
  printMetrics(df[["decW"]],df[["genericDecWProb"]])
}


for(key in keys(dfHash)){
  print(key)
  df = dfHash[[key]]
  printMetrics(df[["decW"]],df[["refinedDecWProb"]])
}

for(key in keys(dfHash)){
  print(key)
  df = dfHash[[key]]
  printMetrics(df[["match"]],df[["genericConjProb"]])
}

for(key in keys(dfHash)){
  print(key)
  df = dfHash[[key]]
  printMetrics(df[["match"]],df[["refinedConjProb"]])
}


for(key in keys(dfHash)){
  print(key)
  df = dfHash[[key]]
  printMetrics(df[["match"]],df[["genericConjProbCal"]])
}


for(key in keys(dfHash)){
  print(key)
  df = dfHash[[key]]
  printMetrics(df[["match"]],df[["refinedConjProbCal"]])
}


for(key in keys(dfHash)){
  print(key)
  df = dfHash[[key]]
  printMetrics(df[["match"]],df[["matchGuess"]])
}

for(df in c(mergedModifsimp, mergedModif)){printMetrics(df[["decM"]],df[["refinedDecMProb"]])}
for(df in c(mergedModifsimp, mergedModif)){printMetrics(df[["decW"]],df[["genericDecWProb"]])}
for(df in c(mergedModifsimp, mergedModif)){printMetrics(df[["decW"]],df[["refinedDecWProb"]])}
for(df in c(mergedModifsimp, mergedModif)){printMetrics(df[["match"]],df[["genericConjProb"]])}
for(df in c(mergedModifsimp, mergedModif)){printMetrics(df[["match"]],df[["genericConjProbCal"]])}
for(df in c(mergedModifsimp, mergedModif)){printMetrics(df[["match"]],df[["refinedConjProb"]])}
for(df in c(mergedModifsimp, mergedModif)){printMetrics(df[["match"]],df[["refinedConjProbCal"]])}
for(df in c(mergedModifsimp, mergedModif)){printMetrics(df[["match"]],df[["matchGuess"]])}

