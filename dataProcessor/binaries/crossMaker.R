


makeCrossHash = function(df, colNames){
  n = names(df)
  crossHash = hash()
  for(colName in origColNames){
    crossHash[[paste(colName),"s",sep=""]]
  }
  return(crossHash)
}


makeCrossesAndFreqs = function(df, crossHash){
  for(key in keys(crossHash)){
    print(key)
    colNames = c(paste(key,"WomanAvgDecM",sep=""), paste(key,"WomanAvgDecW",sep=""), paste(key,"WomanAvgMatch",sep=""),
                 c(paste(key,"ManAvgDecM",sep=""), paste(key,"ManAvgDecW",sep=""), paste(key,"ManAvgMatch",sep="")),
                   paste(key,"CrossAvgDecM",sep=""), paste(key,"CrossAvgDecW",sep=""), paste(key,"CrossAvgMatch",sep=""))
                 df[colNames] = df[c("avgWaveDecM", "avgWaveDecW", "avgWaveMatch",
                                     "avgWaveDecM", "avgWaveDecW", "avgWaveMatch",
                                     "avgWaveDecM","avgWaveDecW" , "avgWaveMatch" )]
                 vars = crossHash[[key]] 
                 menVars = vars[grep("M$", vars)]
                 womenVars = vars[grep("W$", vars)]
                 decsAndMatch = c("decM", "decW", "match")
                 for(womanVar in womenVars){
                   slice = df[df[womanVar] == 1, decsAndMatch]
                   if(nrow(slice) >= 20){        
                     df[df[womanVar] == 1,][colNames[1]] = (sum(slice[[1]]) - 1)/(nrow(slice) - 1)
                     df[df[womanVar] == 1,][colNames[2]] = (sum(slice[[2]]) - 1)/(nrow(slice) - 1)
                     df[df[womanVar] == 1,][colNames[3]] = (sum(slice[[3]]) - 1)/(nrow(slice) - 1)
                   }
                 }
                 for(manVar in menVars){
                   slice = df[df[manVar] == 1, decsAndMatch]
                   if(nrow(slice) >= 20){
                     df[df[manVar] == 1,][colNames[4]] = (sum(slice[[1]]) - 1)/(nrow(slice) - 1)
                     df[df[manVar] == 1,][colNames[5]] = (sum(slice[[2]]) - 1)/(nrow(slice) - 1)
                     df[df[manVar] == 1,][colNames[6]] = (sum(slice[[3]]) - 1)/(nrow(slice) - 1)
                   }
                 }
                 for(manVar in menVars){
                   for(womanVar in womenVars){
                     tempColName = paste(manVar,womanVar,sep="_")
                     newColName = paste(tempColName,"Cross",sep="")
                     df[newColName] = df[manVar]*df[womanVar]
                     slice = df[df[manVar] == 1 & df[womanVar] == 1,decsAndMatch]
                     if(nrow(slice) >= 15){
                       df[df[manVar] == 1 & df[womanVar] == 1,][colNames[7]] = (sum(slice[[1]]) - 1)/(nrow(slice) - 1)
                       df[df[manVar] == 1 & df[womanVar] == 1,][colNames[8]] = (sum(slice[[2]]) - 1)/(nrow(slice) - 1)
                       df[df[manVar] == 1 & df[womanVar] == 1,][colNames[9]] = (sum(slice[[3]]) - 1)/(nrow(slice) - 1)
                     }
                   }
                 }
  }  
  probs = n[grep("AvgDec|AvgMatch|decAvg|DecAvg|avgWave", n)]
  for(prob in probs){
    lor = gsub("Avg|avg", "LOR", prob)
    merged[[lor]] = probsToLORs(merged[[prob]])
  }
  return(df)
}

