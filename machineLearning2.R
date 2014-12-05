source("libraries.R")


getProbs = function(train, test, features, tar){
  s=scale(train[features],center=TRUE,scale=TRUE)
  co = c(heuristicC(s))
  m=LiblineaR(data=s,labels=factor(train[,tar]),type=0,cost=co,bias=TRUE,verbose=FALSE)
  s2= scale(test[features],attr(s,"scaled:center"),attr(s,"scaled:scale"))
  p=predict(m,s2,prob=TRUE)
  probs = p$probabilities[,"1"]  
  return(probs)
}





featuresSelector = function(train, test, currentFeatures, remainingFeatures, tar, fracTrain, numTimes, numRequested){
  h = hash()
  origLen = length(currentFeatures)
  for(i in 1:numRequested){
    cat("Length of current features: ", length(currentFeatures), " Length of remaining features: ", length(remainingFeatures), "\n",sep="")
    scores = names(featureSelector(train, test, currentFeatures, remainingFeatures, tar, fracTrain, numTimes))
    if(length(scores) > 1 &  length(currentFeatures) < (numRequested + length(origLen))){
      remainingFeatures = names(scores)[scores >=5]  
      currentFeatures[length(currentFeatures) + 1] = remainingFeatures[1]
      remainingFeatures = remainingFeatures[-1:0]
      
      h[["currentFeatures"]] = currentFeatures
      
      h[["remainingFeatures"]] = remainingFeatures
    }
    else{
      h[["remainingFeatures"]] = NA
    }
  }
  return(h)
}

featureSelector = function(train, test, base, tries, tar, fracTrain, numTimes, thres){
  totalLogLoss = 0
  scores = hash()
  for(feature in tries){
    scores[[feature]] = 0
  }
  for(i in 1:numTimes){
    set.seed = i; idxs =  sample(1:nrow(train))
    startIdx = 1
    midIdx = floor(nrow(train)*fracTrain)
    trainIdxs = idxs[startIdx:midIdx]
    trainTemp = train[trainIdxs,]
    baseLogLoss = logLoss(test[[tar]], getProbs(trainTemp, test, base,tar))
    totalLogLoss = baseLogLoss + totalLogLoss
    for(feature in tries){
      tempFeatures = c(base,feature)
      newLogLoss = logLoss(test[[tar]], getProbs(train, test, tempFeatures,tar))
      scores[[feature]] = scores[[feature]] + baseLogLoss - newLogLoss 
    }
  }
  for(feature in tries){
    scores[[feature]] = (scores[[feature]]/numTimes)
  }
  scores = values(scores)
  sorted = round(10000*sort(scores, decreasing= TRUE))
  sorted = sorted[sorted >= thres]
  baseLogLoss = round(10000*baseLogLoss)
  bestFeature = names(sorted[1])
  print(sorted)
  cat("Original LogLoss: ", baseLogLoss, " Best Log Loss ", baseLogLoss - sorted[1], " Best feature: ", bestFeature, "\n", sep="")
  currentFeatures = c(base, bestFeature)
  cat("currentFeatures:", "\n", sep="")
  for(c in currentFeatures){
    cat(c, "\n", sep="")
  }
  return(sorted)
}


