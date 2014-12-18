


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

