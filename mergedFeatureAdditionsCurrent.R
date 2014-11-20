merged = read.csv( '~/Desktop/speedDating/merged.csv')
n = names(merged)

merged[c("matchAvgM", "matchAvgW")] = 0
slice = merged[c("iidW", "iidM", "match")]
for(i in unique(merged[["iidW"]])){
  newSlice = slice[slice["iidW"] == i,]
  merged[merged["iidW"] == i,][["matchAvgW"]]  = (sum(newSlice[["match"]]) - newSlice[["match"]])/(nrow(newSlice) - 1)
}
for(i in unique(merged[["iidM"]])){
  newSlice = slice[slice["iidM"] == i,]
  merged[merged["iidM"] == i,][["matchAvgM"]]  = (sum(newSlice[["match"]]) - newSlice[["match"]])/(nrow(newSlice) - 1)
}
merged[c("avgWaveDecM", "avgWaveDecW", "avgWaveMatch")] = 0

merged[c("avgWaveDecM", "avgWaveDecW", "avgWaveMatch")] = 0
for(w in unique(merged[["wave"]])){
  slice = merged[merged["wave"] == w,]
  mSum = sum(slice[["decM"]])
  wSum = sum(slice[["decW"]])
  matchSum = sum(slice[["match"]])
  print(matchSum)
  merged[merged["wave"] == w,][["avgWaveDecM"]] = (mSum - slice[["decM"]])/(nrow(slice) - 1)
  merged[merged["wave"] == w,][["avgWaveDecW"]] = (wSum - slice[["decW"]])/(nrow(slice) - 1)
  merged[merged["wave"] == w,][["avgWaveMatch"]] = (matchSum - slice[["match"]])/(nrow(slice) - 1)
}

merged[["avgDecMPair"]] = sqrt(merged[["raterDecAvgM"]]*merged[["decAvgW"]])
merged[["avgDecWPair"]] = sqrt(merged[["raterDecAvgW"]]*merged[["decAvgM"]])

merged[["avgMatchPair"]] = 1 - sqrt((1 - merged[["matchAvgM"]])*(1 - merged[["matchAvgW"]]))




crossHash = hash()

crossHash[["races"]] = n[grep("^race*M", n)]
crossHash[["fields"]] = n[grep("field",n)]
crossHash[["careers"]] = n[grep("career",n)]
crossHash[["goals"]] = n[grep("goal",n)]
crossHash[["dates"]] = n[grep("date",n)][c(2:8, 10:16)]


makeCrossesAndFreqs = function(df, crossHash){
  for(key in keys(crossHash)){
    print(key)
    colNames = c(paste(key,"WomanProbDecM",sep=""), paste(key,"WomanProbDecW",sep=""), paste(key,"WomanProbMatch",sep=""),
                 c(paste(key,"ManProbDecM",sep=""), paste(key,"ManProbDecW",sep=""), paste(key,"ManProbMatch",sep="")),
                   paste(key,"CrossProbTraitDecM",sep=""), paste(key,"CrossProbDecW",sep=""), paste(key,"CrossProbMatch",sep=""))
                 df[colNames] = df[c("decAvgW", "raterDecAvgW", "matchAvgW",
                                     "decAvgM", "raterDecAvgM", "matchAvgM",
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
  return(df)
}

crossHash = hash()

crossHash[["races"]] = n[grep("^race", n)]
crossHash[["fields"]] = n[grep("field",n)]
crossHash[["careers"]] = n[grep("career",n)]
crossHash[["goals"]] = n[grep("goal",n)]
crossHash[["dates"]] = n[grep("date",n)][c(2:8, 10:16)]

merged = makeCrossesAndFreqs(merged,crossHash)
n = names(merged)
badCols = names(merged[,colSums(merged) <= 60])
badMenCols = badCols[grep("M$",badCols)]
badWomenCols = badCols[grep("W$",badCols)]
badMenColsWomen = gsub("M$", "W", badMenCols)
badWomenColsMen = gsub("W$", "M", badWomenCols)
drops = c(badCols, badMenColsWomen, badWomenColsMen)
merged = merged[,!(names(merged) %in% drops)]
write.csv(merged, '~/Desktop/speedDating/mergedCrossFeaturesAdded.csv')


