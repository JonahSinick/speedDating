


makeCrossHash = function(df, colNames){
  n = names(df)
  crossHash = hash()
  for(colName in colNames){
    crossHash[[colName]] = n[grep(colName,n)]
  }
  for(key in keys(crossHash)){
    otherName = paste(key,"Other",sep="")
    df[otherName] = 1
    for(cross in crossHash[[key]]){
      df[[otherName]] = ifelse(df[[cross]] == 1, 0, df[[otherName]])
    }
    crossHash[[key]] = c(crossHash[[key]], otherName)
  }
  answer = hash()
  answer[["crossHash"]] = crossHash
  answer[["df"]] = df
  return(answer)
}


makeCrossesAndFreqs = function(df, crossHash){
  for(key in keys(crossHash)){
    features = crossHash[[key]]
    mFeatures = gsub("$" ,"M", features)
    wFeatures = gsub("$" ,"W", features)
    crossFeatures = c()
    for(mFeature in mFeatures){
      for(wFeature in wFeatures){
        crossName = gsub("$","Cross",paste(mFeature,wFeature,sep=""))     
        df[crossName] = df[mFeature]*df[wFeature]
        crossFeatures = c(crossFeatures, crossName)        
      }
    }
    probDecM = paste(key,"ProbDecM",sep="")
    probDecW = paste(key,"ProbDecW",sep="")
    df[c(probDecM, probDecW)] = 0
    for(cross in crossFeatures){
      print(cross)
      slice = df[df[cross] == 1,][c("iidM","iidW","decM", "decW")]
      sDecM = sum(slice[["decM"]])
      sDecW = sum(slice[["decW"]])
      for(i in 1:nrow(slice)){
        iidM = slice[i,"iidM"]
        iidW = slice[i,"iidW"]
        iidMs = unique(slice[["iidM"]])
        iidWs = unique(slice[["iidW"]])
        if(length(iidMs) >= 5 & length(iidWs) >= 5){
          iidMs = iidMs[iidMs != iidM]
          iidWs = iidWs[iidWs != iidW]
          surIIDM = sample(iidMs, 1)
          surIIDW = sample(iidWs, 1)
          smallSlice = slice[slice["iidM"] == iidM | slice["iidW"] == iidW,]
          sliceSurM = slice[slice["iidM"] == surIIDM,]
          sliceSurW = slice[slice["iidW"] == surIIDW,]
          removesDecM = sum(smallSlice[["decM"]])
          addsDecM = sum(sliceSurM[["decM"]])
          removesDecW = sum(smallSlice[["decW"]])
          addsDecW = sum(sliceSurW[["decW"]])
          df[df["iidM"] == iidM & df["iidW"] == iidW,probDecM] = (sDecM - removesDecM + addsDecM)/(nrow(slice) - nrow(smallSlice) + nrow(sliceSurM))
          df[df["iidM"] == iidM & df["iidW"] == iidW,probDecW] = (sDecW - removesDecW + addsDecW)/(nrow(slice) - nrow(smallSlice) + nrow(sliceSurW))
        }
      }
    }
    df[[probDecM]] = ifelse(df[[probDecM]] == 0, 0.48, df[[probDecM]])
    df[[probDecW]] = ifelse(df[[probDecW]] == 0, 0.33, df[[probDecW]])
    df[[gsub("Prob","LOR", probDecM)]] = probsToLORs(df[[probDecM]])
    df[[gsub("Prob","LOR", probDecW)]] = probsToLORs(df[[probDecW]])
  }
  return(df)
}

