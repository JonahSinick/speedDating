
addScoreCols = function(df, genType, opGenType, probType){
  df["tempIdx"] = 0
  for(i in 1:nrow(df)){
    df[i,"tempIdx"] = i
  }
  recType = paste(probType,"Rec",sep="")
  orderType = paste(probType,"Order",sep="")
  matchType = paste(probType,"Match",sep="")
  names = gsub("$", genType, c(recType,orderType,matchType))
  df[names] = 0
  iidType = paste("iid",genType,sep="")
  for(iid in unique(df[[iidType]])){
    slice = df[df[iidType] ==iid, ]
    ordered = slice[order(-slice[probType]),]
    ordered[names[1]]= ordered[paste("iid",opGenType,sep="")]
    for(i in 1:nrow(ordered)){
      ordered[i,names[2]] = i
    }
    ordered[names[3]] = ordered["match"]
    ordered = ordered[order(ordered["tempIdx"]),]
    df[df[iidType] ==iid,][[names[1]]] = ordered[[names[1]]]
    df[df[iidType] ==iid,][[names[2]]] = ordered[[names[2]]]
    df[df[iidType] ==iid,][[names[3]]] = ordered[[names[3]]]
  }
  df = df[!(names(df) == "tempIdx")]
  return(df)
}



createTopMetrics = function(df, probNames){
  topMetrics = data.frame()
  topMetrics[1:22,] = 0
  for(probName in probNames){
    for(i in 1:22){
      tops = df[df[paste(probName,"OrderW",sep="")] <= i  & df[paste(probName,"OrderM",sep="")] <= i,]
      topMetrics[i,paste(probName,"NumDates",sep="")] = nrow(tops)
      matches = tops[tops["match"] ==1,]
      topMetrics[i,paste(probName,"NumMatches",sep="")] = nrow(matches)
      topMetrics[i,paste(probName,"AtLeastOne",sep="")] = length(unique(matches[["iidM"]])) + length(unique(matches[["iidW"]]))
    }
    topMetrics[paste(probName,"PercentMatch",sep="")] = topMetrics[paste(probName,"NumMatches",sep="")]/topMetrics[paste(probName,"NumDates",sep="")]
    topMetrics[paste(probName,"FracFound",sep="")] = topMetrics[paste(probName,"NumMatches",sep="")]/nrow(df[df["match"] == 1,])
    topMetrics[paste(probName,"FracPFound",sep="")] =  topMetrics[paste(probName,"AtLeastOne",sep="")]/103
  }
  n = names(topMetrics)
  changes = n[grep("PercentMatch|FracFound|FracPFound",n)]
  topMetrics = round(topMetrics,2)
  topMetrics[changes] = 100*topMetrics[changes]
  return(topMetrics)
}







# write.csv(scoreSlice, '~/Desktop/speedDatingFinal/scoreSlice.csv')
# write.csv(topMetrics, '~/Desktop/speedDatingFinal/topMetrics.csv')
# 
