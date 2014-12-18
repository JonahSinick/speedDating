eventCV = function(df, features, tar, newColName, model, thres=10){
  waves = unique(df[["wave"]])
  df[newColName] = 0
  for(wave in waves){  
    train = df[df["wave"] != wave,]  
    test = df[df["wave"] == wave,] 
    df[df["wave"] == wave,][[newColName]] = getProbs(train, test, features, tar, model)
  }
  if(logLoss(df[[tar]], df[[newColName]]) < thres){
    printMetrics(df[[tar]], df[[newColName]])    
  }
  return(df)
}

