LORsToProbs = function(obj){
  OR = exp(obj)
  probs = OR/(1 + OR)
  return(probs)
}
NAFixer = function(x) {ifelse(is.na(x), median(x, na.rm=TRUE), x)}

addPrComps = function(df, features, prcompNames){
  p = prcomp(scale(df[features]))
  ndf = as.data.frame(p$x)[seq_len(length(prcompNames))]
  df[prcompNames] = round(scale(ndf),2)
  return(df)
}


makePreds = function(df, model, predName, groupHash){
  df[df$train == 1,predName] = fitted(model)
  df[df$train == 0,predName] = LORsToProbs(predict(model, df[df$train == 0,]))
  groups = keys(groupHash)
  for(group in groups){
    colNames = groupHash[[group]]
    coefNames = gsub("$","Coef",colNames)
    df[coefNames] = 0
    groupCoefs = coef(model)[[group]]
    for(groupID in unique(df[[group]])){
      df[df[group] == groupID,coefNames] = groupCoefs[toString(groupID),colNames]
    }
  }
  return(df)
}