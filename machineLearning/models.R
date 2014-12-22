getProbs = function(train, test, features, tar, model="linear"){
  if(model == "linear"){
    s=scale(train[features],center=TRUE,scale=TRUE)
    co = c(heuristicC(s))
    m=LiblineaR(data=s,labels=factor(train[,tar]),type=0,cost=co,bias=TRUE,verbose=FALSE)
    s2= scale(test[features],attr(s,"scaled:center"),attr(s,"scaled:scale"))    
    predictions =predict(m,s2,prob=TRUE)
    probs = predictions$probabilities[,"1"]  
  }
  if(model == "forest"){
    rf_fit <- randomForest(y=as.factor(train[,tar]), x=train[features], importance=TRUE, ntree=400)
    predictions = predict(rf_fit, test, type="prob")
    probs = predictions[,"1"]  
  }
  probs = ifelse(probs < 0.0001, 0.0001, probs)
  probs = ifelse(probs > 0.9999, 0.9999, probs)
  return(probs)
}


