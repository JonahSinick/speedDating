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
merged = read.csv('~/Desktop/speedDating/mergedBinariesHandled.csv')

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


probsToLORs = function(probs){
  probs = ifelse(probs < 0.02, 0.02, probs)
  probs = ifelse(probs > 0.98, 0.98, probs)
  ORs = probs/(1 - probs)
  return(log(ORs))
}


LORsToProbs = function(LORs){
  ORs = exp(LORs)
  probs = ORs/(1 + ORs)
  return(probs)
}

evenBetterBooster = function(df, base, tries, tar, numTimes,type, fractionTrain){
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
    midIdx = floor(fractionTrain*nrow(df))
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


n = names(merged)
myFeatures =n[grep("RaterAvg|RateeAvg|WaveAvg|field|career|imprace|imprelig|Pref|race|goal|date|goOut",n)]
decFeatures = myFeatures[grep("dec",myFeatures)]
for(feat in decFeatures){
  merged[[feat]] = probsToLORs(merged[[feat]])  
}
newFeatures = myFeatures[!(myFeatures %in% myFeatures[grep("CD|After|raceM|raceW|goalW|goalM", myFeatures)])]

menBase = c("attrRateeAvgAdjM", "likeRateeAvgAdjM", "decRateeAvgM", "decRaterAvgM", "sharRateeAvgAdjW")
names = selectedFeaturesM[["remainingFeatures"]]
selectedFeaturesM = featureSelector(df = merged, startingFeatures = c(menBase), 
                                    features= names[1], target = "decRatingM", numTries = 1000, 
                                    finalLength = 6, fractionTrain = 0.66)


womenBase = c("decRaterAvgW", "attrRateeAvgAdjW", "decRateeAvgW", "date4Mdate6WCross")
names = selectedFeaturesW[["remainingFeatures"]]
selectedFeaturesW = featureSelector(df = merged, startingFeatures = womenBase, 
                                    features= names, target = "decRatingW", numTries = 100, 
                                    finalLength = 7, fractionTrain = 0.66)
selectedFeaturesW[["remainingFeatures"]]




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
