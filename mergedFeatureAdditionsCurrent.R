merged = read.csv( '~/Desktop/speedDating/merged.csv')
n = names(merged)

merged[c("matchAvgM", "matchAvgW")] = 0
slice = merged[c("iidW", "pidW", "match")]
for(i in unique(merged[["iidW"]])){
  newSlice = slice[slice["iidW"] == i, "match"]
  merged[merged["iidW"] == i,][["matchAvgW"]]  = (sum(newSlice[["match"]]) - newSlice[["match"]])/(nrow(newSlice) - 1)
}
for(i in unique(merged[["pidW"]])){
  newSlice = slice[slice["pidW"] == i, "match"]
  merged[merged["pidW"] == i,][["matchAvgM"]]  = (sum(newSlice[["match"]]) - newSlice[["match"]])/(nrow(newSlice) - 1)
}

merged[["avgDecMPair"]] = sqrt(merged[["raterDecAvgM"]]*merged[["decAvgbyMofW"]])
merged[["avgDecWPair"]] = sqrt(merged[["raterDecAvgW"]]*merged[["decAvgbyWofM"]])


merged[["avgMatchPair"]] = 1 - sqrt((1 - merged[["matchAvgM"]])*(1 - merged[["matchAvgW"]]))

merged[c("avgWaveDecM", "avgWaveDecW", "avgWaveMatch")] = 0

for(w in unique(merged[["wave"]])){
  slice = merged[merged["wave"] == w, c("decM", "decW", "match")]
  mSum = sum(slice[["decM"]])
  wSum = sum(slice[["decW"]])
  matchSum = sum(slice[["match"]])
  merged[merged["wave"] == w,][["avgWaveDecM"]] = (mSum - slice[["decM"]])/(nrow(slice) - 1)
  merged[merged["wave"] == w,][["avgWaveDecW"]] = (wSum - slice[["decW"]])/(nrow(slice) - 1)
  merged[merged["wave"] == w,][["avgWaveMatch"]] = (matchSum - slice[["match"]])/(nrow(slice) - 1)
}


crossHash = hash()

crossHash[["races"]] = n[grep("^race*M", n)]
crossHash[["fields"]] = n[grep("field",n)]
crossHash[["careers"]] = n[grep("career",n)]
crossHash[["goals"]] = n[grep("goal",n)]
crossHash[["dates"]] = n[grep("date",n)][c(2:8, 10:16)]


makeCrossesAndFreqs = function(df, crossHash){
  for(key in keys(crossHash)){
    print(key)
    colNames = c(paste(key,"WomanTraitDecM",sep=""), paste(key,"WomanTraitDecW",sep=""), paste(key,"WomanTraitMatch",sep=""),
                 paste(key,"ManTraitDecM",sep=""), paste(key,"ManTraitDecW",sep=""), paste(key,"ManTraitMatch",sep=""),
                 paste(key,"CrossTraitDecM",sep=""), paste(key,"CrossTraitDecW",sep=""), paste(key,"CrossTraitMatch",sep=""))
    df[colNames] = df[c("decAvgbyMofW", "raterDecAvgW", "matchAvgW",
                        "decAvgbyWofM", "raterDecAvgM", "matchAvgM",
                        "avgDecMPair","avgDecWPair" , "avgMatchPair" )]
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
        newColName = paste(manVar,womanVar,sep="_")
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
  return(df)
}

crossHash = hash()

crossHash[["races"]] = n[grep("^race", n)]
crossHash[["fields"]] = n[grep("field",n)]
crossHash[["careers"]] = n[grep("career",n)]
crossHash[["goals"]] = n[grep("goal",n)]
crossHash[["dates"]] = n[grep("date",n)][c(2:8, 10:16)]

merged = makeCrossesAndFreqs(merged,crossHash)

write.csv(merged, '~/Desktop/speedDating/mergedCrossFeaturesAdded.csv')
