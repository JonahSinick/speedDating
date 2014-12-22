makeProbs = function(df, features, tar, newColName, model, thres=10){
  waves = unique(df[["wave"]])
  df[newColName] = 0
  for(wave in waves){  
    train = df[df["wave"] != wave,]  
    test = df[df["wave"] == wave,] 
    df[df["wave"] == wave,][[newColName]] = getProbs(train, test, features, tar, model)
  }
  return(df)
}





uniquesCV = function(df, oldFeatures, newFeature, tar, colName){
  answerHash = hash()
  df = makeProbs(df, oldFeatures, tar, "orig", "linear", thres=10)
  df = makeProbs(df, c(oldFeatures, newFeature), tar, "new", "linear", thres=10)
  oldLL = logLoss(df[[tar]], df[["orig"]])
  newLL = logLoss(df[[tar]], df[["new"]])
  uniques = unique(df[[colName]])
  goods = c()
  bads = c()
  oks = c()
  for(unique in uniques){
    slice = df[df[colName] == unique,c(tar,"orig", "new")]
    diff =  logLoss(slice[[tar]], slice[["orig"]]) - logLoss(slice[[tar]], slice[["new"]])
    if(diff > 0){
      goods = c(goods, unique)
    }
    if(diff < 0){
      bads = c(bads, unique)
    }
    if(diff == 0){
      oks = c(oks, unique)
    }
  }
#   if(round((oldLL - newLL), 4) >= 0){
    print(newFeature)
    cat("Original Log Loss: ", round(oldLL,4), " New Log Loss: ", round(newLL, 4), " Difference: ", round((oldLL - newLL), 4),  " # Goods: ", length(goods), " # Bads: ", length(bads), " # OKs: ", length(oks), "\n", sep="")  
#   }
  
}