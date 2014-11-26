merged = read.csv( '~/Desktop/speedDating/merged.csv')
merged = merged[merged[["wave"]] %in% c(2, 4, 7, 9,11,17),]
n = names(merged)
menRatings = c(n[grep("RatingM",n)][1:8])
menAvgs = c(n[grep("RatingM",n)][9:16])
womenRatings = c(n[grep("RatingW",n)][1:8])
womenAvgs = c(n[grep("RatingW",n)][9:16])
merged[c("matchAvgM", "matchAvgW", "avgWaveDecM", "avgWaveDecW", "avgWaveMatch")] = 0
slice = merged[c("iidW", "iidM", menRatings, womenRatings)]

for(j in 1:length(womenRatings)){
  wr = womenRatings[[j]]
  tempSlice = slice[,c("iidW", "iidM", wr)]
  tempSlice[paste(wr,"fixedAvg",sep="")] = 0
  for(iidW in unique(tempSlice[["iidW"]])){
    tempSlice2 =  tempSlice[tempSlice["iidW"] == iidW,]
    for(iidM in unique(tempSlice2[["iidM"]])){
      print(c(wr, iidW,iidM))
      newSlice = tempSlice2[tempSlice2["iidM"] != iidM,]
      val =  (sum(newSlice[[wr]]) + sample(newSlice[[wr]],1))/(nrow(tempSlice2))
      tempSlice[tempSlice["iidW"] == iidW & tempSlice["iidM"] == iidM, paste(wr,"fixedAvg",sep="")] = val
    }
  }
  merged[[womenAvgs[j]]] = tempSlice[[paste(wr,"fixedAvg",sep="")]]
}

for(j in 1:length(menRatings)){
  mr = menRatings[[j]]
  tempSlice = slice[,c("iidM", "iidW", mr)]
  tempSlice[paste(mr,"fixedAvg",sep="")] = 0
  for(iidM in unique(tempSlice[["iidM"]])){
    tempSlice2 =  tempSlice[tempSlice["iidM"] == iidM,]
    for(iidW in unique(tempSlice2[["iidW"]])){
      print(c(mr, iidM,iidW))
      newSlice = tempSlice2[tempSlice2["iidW"] != iidW,]
      val =  (sum(newSlice[[mr]]) + sample(newSlice[[mr]],1))/(nrow(tempSlice2))
      tempSlice[tempSlice["iidM"] == iidM & tempSlice["iidW"] == iidW, paste(mr,"fixedAvg",sep="")] = val
    }
  }
  merged[[menAvgs[j]]] = tempSlice[[paste(mr,"fixedAvg",sep="")]]
}

finalSlice = merged[c("iidW", "iidM", "wave", "decW", "decM", "match", 
                      "decAvgM", "decAvgW", "raterDecAvgW", "raterDecAvgM", 
                      "matchAvgM", "matchAvgW", "avgWaveDecM", "avgWaveDecW", "avgWaveMatch")]

for(iidW in unique(finalSlice[["iidW"]])){
  tempSlice =  finalSlice[finalSlice["iidW"] == iidW,]
  for(iidM in unique(tempSlice[["iidM"]])){
    newSlice = tempSlice[tempSlice["iidM"] != iidM,]
    val =  (sum(newSlice[["decW"]]) + sample(newSlice[["decW"]],1))/(nrow(tempSlice))
    finalSlice[finalSlice["iidM"] == iidM & finalSlice["iidW"] == iidW, "raterDecAvgW"] = val
    val2 =  (sum(newSlice[["decM"]]) + sample(newSlice[["decM"]],1))/(nrow(tempSlice))
    finalSlice[finalSlice["iidM"] == iidM & finalSlice["iidW"] == iidW, "decAvgW"] = val2
    val3 =  (sum(newSlice[["match"]]) + sample(newSlice[["match"]],1))/(nrow(tempSlice))
    finalSlice[finalSlice["iidM"] == iidM & finalSlice["iidW"] == iidW, "matchAvgW"] = val3
  }
}
for(iidM in unique(finalSlice[["iidM"]])){
  tempSlice =  finalSlice[finalSlice["iidM"] == iidM,]
  for(iidW in unique(tempSlice[["iidW"]])){
    newSlice = tempSlice[tempSlice["iidW"] != iidW,]
    val =  (sum(newSlice[["decM"]]) + sample(newSlice[["decM"]],1))/(nrow(tempSlice))
    finalSlice[finalSlice["iidM"] == iidM & finalSlice["iidW"] == iidW, "raterDecAvgM"] = val
    val2 =  (sum(newSlice[["decW"]]) + sample(newSlice[["decW"]],1))/(nrow(tempSlice))
    finalSlice[finalSlice["iidM"] == iidM & finalSlice["iidW"] == iidW, "decAvgM"] = val2
    val3 =  (sum(newSlice[["match"]]) + sample(newSlice[["match"]],1))/(nrow(tempSlice))
    finalSlice[finalSlice["iidM"] == iidM & finalSlice["iidW"] == iidW, "matchAvgM"] = val3
  }
}

for(w in unique(finalSlice[["wave"]])){
  print(w)
  slice = finalSlice[finalSlice["wave"] == w,]
  for(i in row(slice)[,1]){
    newSlice = slice[row(slice)[,1] != i,]
    valM = (sum(newSlice[["decM"]]) + sample(newSlice[["decM"]],1))/(nrow(slice))
    valW = (sum(newSlice[["decW"]]) + sample(newSlice[["decW"]],1))/(nrow(slice))
    valMatch = (sum(newSlice[["match"]]) + sample(newSlice[["match"]],1))/(nrow(slice))
    slice[i,"avgWaveDecM"] = valM
    slice[i,"avgWaveDecW"] = valW
    slice[i,"avgWaveMatch"] = valMatch
  }
  finalSlice[finalSlice["wave"] == w,][["avgWaveDecM"]] = slice[["avgWaveDecM"]]
  finalSlice[finalSlice["wave"] == w,][["avgWaveDecW"]] = slice[["avgWaveDecW"]]
  finalSlice[finalSlice["wave"] == w,][["avgWaveMatch"]] = slice[["avgWaveMatch"]]
}
for(w in unique(finalSlice[["wave"]])){
  slice = finalSlice[finalSlice["wave"] == w,]
  print("wave")
  print(w)
  print("nrow")
  print(nrow(slice))
  print("decM")
  print(mean(slice[["decM"]]))
  print(mean(slice[["avgWaveDecM"]]))
  print("decW")
  print(mean(slice[["decW"]]))
  print(mean(slice[["avgWaveDecW"]]))
  print("decW")
  print(mean(slice[["decW"]]))
  print(mean(slice[["avgWaveDecW"]]))
  print("match")
  print(mean(slice[["match"]]))
  print(mean(slice[["avgWaveMatch"]]))  
}





merged[c("iidW", "iidM", "wave", "decW", "decM", "match", 
         "decAvgM", "decAvgW", "raterDecAvgW", "raterDecAvgM", 
         "matchAvgM", "matchAvgW", "avgWaveDecM", "avgWaveDecW", "avgWaveMatch")] = finalSlice



merged["numMen"] = 0
merged["numWomen"] = 0

for(w in unique(merged[["wave"]])){
  print(length(unique(merged[merged["wave"] == w,][["iidM"]])))
  merged[merged["wave"] == w,]["numMen"] = length(unique(merged[merged["wave"] == w,][["iidM"]]))
  merged[merged["wave"] == w,]["numWomen"] = length(unique(merged[merged["wave"] == w,][["iidW"]]))
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
badCols = n[colSums(merged) <= 120]
badMenCols = badCols[grep("M$",badCols)]
badWomenCols = badCols[grep("W$",badCols)]
badMenColsWomen = gsub("M$", "W", badMenCols)
badWomenColsMen = gsub("W$", "M", badWomenCols)
drops = c(badCols, badMenColsWomen, badWomenColsMen)
merged = merged[,!(names(merged) %in% drops)]


probsToLORs = function(probs){
  probs = ifelse(probs < 0.03, 0.03, probs)
  probs = ifelse(probs > 0.97, 0.97, probs)
  ORs = probs/(1 - probs)
  return(log(ORs))
}

n = names(merged)

probs = n[grep("AvgDec|AvgMatch|decAvg|DecAvg", n)]

for(prob in probs){
  lor = gsub("Avg", "LOR", prob)
  merged[[lor]] = probsToLORs(merged[[prob]])
}

menTraits = n[grep("M$",n)][c(3:59,97:113)]
n = names(merged)
LORs = n[grep("LOR",n)]
womenTraits = gsub("M$", "W", menTraits)
signedDiff = gsub("M$", "SignedDiff", menTraits)
absDiff = gsub("M$", "AbsDiff", menTraits)

for(i in 1:length(menTraits)){
  merged[signedDiff[i]] = merged[menTraits[i]] - merged[womenTraits[i]]
  merged[absDiff[i]] = abs(merged[signedDiff[i]])
}




write.csv(merged, '~/Desktop/speedDating/mergedCrossFeaturesAdded.csv')
