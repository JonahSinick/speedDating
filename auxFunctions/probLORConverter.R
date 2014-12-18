

LORsToProbs = function(LORs){
  ORs = exp(LORs)
  probs = ORs/(1 + ORs)
  return(probs)
}

LORColsToProbs = function(df, LORs){
  for(LOR in LORs){
    df[[LOR]] = LORsToProbs(df[[LOR]])
  }
  return(df)
}

adjustProbs = function(probs,threshold=0.01){
  probs = ifelse(probs < threshold,threshold, probs)
  probs = ifelse(probs > 1 - threshold, 1 - threshold, probs)
  return(probs)
}
probColsToLORs = function(df, probNames){
  for(probs in probNames){
    df[[probs]] = probsToLORs(df[[probs]])
  }
  return(df)
}
probsToLORs = function(probs){
  probs = adjustProbs(probs)
  LORs = log(probs/(1 - probs))
  return(LORs)
}

