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
"exphappyM"
menBase = c("raterDecLORM", "decLORW", "attrAvgRatingW", "fieldsCrossLORDecM", "likeAvgRatingSignedDiff", "datesCrossLORDecM",
            "gamingMiscActSignedDiff", "sharAvgRatingAbsDiff")
bestFeatures = evenBetterBooster(merged,menBase[1:8], c("sharAvgRatingAbsDiff"), "decM", 1000,"heuristic")
10000*round(sort(values(bestFeatures), decreasing=TRUE),4)

newNames = names(sort(values(bestFeatures), decreasing=TRUE))[1:55] 

#..............

womenBase = c("raterDecLORW", "decLORM", "attrAvgRatingM", "fieldsCrossLORDecW", "attrAvgRatingAbsDiff",
              "datesCrossLORDecW", "goalsCrossLORDecW", "funAvgRatingAbsDiff", "raterDecLORM", "careersCrossLORDecW")
bestFeatures = evenBetterBooster(merged,womenBase[c(1:7)], newNames[c(3,5,6:10)], "decW", 300,"heuristic")
10000*round(sort(values(bestFeatures), decreasing=TRUE),4)

newNames = names(sort(values(bestFeatures), decreasing=TRUE))[1:10]


boostedPredictor = function(df, features, tar, numTimes, numChunks=10){
  df[1:nrow(df),"oldIndexs"] = 1:nrow(df)
  chunkSize = nrow(df)/chunkSize
  scrambledIdxs = set.seed(427); sample(1:nrow(df))
  df = df[scrambledIdxs,]
  df["answer"] = 0
  splits = c(seq(0, nrow(df) - 1,by=chunkSize), nrow(df))
  for(i in 1:numChunks){
    print(i)
    idxs = 1:nrow(df)
    testSeg = seq(splits[i] + 1,splits[i+1],by=1)
    print(testSeg)
    trainSeg = idxs[!(idxs %in% testSeg)]
    test = df[testSeg,]
    train = df[trainSeg,]
    for(j in 1:numTimes){
      idxs = sample(1:nrow(train))
      startIdx = 1
      midIdx = floor(1*(nrow(train)/2))
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
  df[order(df[["oldIndexs"]]),]
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


genericDecM = c(menBase[1:3], "avgWaveDecM")
genericDecW = c(womenBase[c(1:3,9)], "avgWaveDecW")
refinedDecM = menBase
refinedDecW = womenBase


merged[["genericDecMProb"]] = boostedPredictor(merged,genericDecM, "decM", 2)
merged[["genericDecWProb"]] = boostedPredictor(merged,genericDecW, "decW", 400)
merged[["genericConjProb"]] = merged[["genericDecMProb"]]*merged[["genericDecWProb"]]


for(i in 1:10){
  start = floor(1 + (i-1)(nrow(merged)/10))
  test = merged[]
}
merged[["refinedDecMProb"]] = boostedPredictor(merged,refinedDecM, "decM", 400)


merged[["refinedDecWProb"]] = boostedPredictor(merged,refinedDecW, "decW", 400)


merged[["refinedConjProb"]] = merged[["refinedDecMProb"]]*merged[["refinedDecWProb"]]



merged[["genericConjProbCal"]] = boostedPredictor(merged,c("genericConjProb"), "match", 100)
merged[["refinedConjProbCal"]] = boostedPredictor(merged,c("refinedConjProb"), "match", 100)


mergedBase = c("refinedConjProbCal", "attrAvgRatingAbsDiff", "goalsWomanLORDecM", "raterDecLORAbsDiff",
               "yogaPhysActM", "goalsCrossLORDecW")
votes = evenBetterBooster(merged,"refinedConjProbCal", c(mergedBase[2:6]), "match", 400,"heuristic")
10000*round(sort(values(votes), decreasing=TRUE),4)


slice = merged[1:25, 1:10]
slice[1:25,"oldIndexs"] = 1:25

set.seed(427); scrambled = sample(1:nrow(slice))
scrambSlice = slice[scrambled,]
order(scrambSlice[,])
