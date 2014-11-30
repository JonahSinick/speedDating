library(gdata)
library(ggplot2)
library(miscTools)
library(xlsx)
library(hash)
library(aod)
library(randomForest)
library(LiblineaR)
library(Matrix)
library(plyr)
library(recommenderlab)
library(scatterplot3d)
library(Metrics)


merged = read.csv( '~/Desktop/speedDating/mergedProbsAddedFinal.csv')

probs = names(merged)[425:433]
recTypes = gsub("$", "Rec", probs)
recTypesDecM = gsub("$", "RecDecM", probs)
recTypesDecW = gsub("$", "RecDecW", probs)
recTypesMatch = gsub("$", "RecMatch", probs)


menRecDF = merged[c("iidW", "iidM", "wave", "decM", "decW", "match", probs)]
for(i in 1:length(probs)){
  menRecDF[c(recTypes[i], recTypesDecM[i],recTypesDecW[i], recTypesMatch[i])] = 0
}
menRecDF = menRecDF[order(-menRecDF["iidM"]),]
for(iidM in unique(menRecDF[["iidM"]])){
  for(i in 1:length(probs)){
    slice = menRecDF[menRecDF["iidM"] == iidM, c(probs[i], "iidW", "decM", "decW", "match")]
    slice = slice[order(-slice[probs[i]]),]
    features = c(recTypes[i], recTypesDecM[i], recTypesDecW[i], recTypesMatch[i])
    menRecDF[menRecDF["iidM"] == iidM,features] = slice[c("iidW", "decM", "decW", "match")]
  }
}
menRecScoreFrame = data.frame()
menRecScoreFrame[1:length(unique(menRecDF[["iidM"]])),] = 0
menRecScoreFrame[["iidM"]] = unique(menRecDF[["iidM"]])
menRecScoreFrame["numPartners"] = 0
menRecScoreFrame["numMatches"] = 0


for(topN in 1:10){
  menRecScoreFrame[paste("baseline",toString(topN),sep="_")] = 0
  for(recType in recTypes){
    colName = gsub("$", "topN", recType)
    
    menRecScoreFrame[paste(colName,toString(topN),sep="_")] = 0
  }
}

for(iidM in unique(menRecScoreFrame[["iidM"]])){
  slice = menRecDF[menRecDF["iidM"] == iidM,"match"]
  scrambledSlice = sample(slice)
  for(n in 1:10){
    menRecScoreFrame[menRecScoreFrame["iidM"] == iidM, paste("baseline",toString(n),sep="_")] =sum(scrambledSlice[1:n]) 
  }
}



for(iidM in unique(menRecScoreFrame[["iidM"]])){
  for(recType in recTypes){
    colName = gsub("$", "topN", recType)
    toBeSummed =  gsub("$", "Match", recType)
    slice = menRecDF[menRecDF["iidM"] == iidM, toBeSummed]
    menRecScoreFrame[menRecScoreFrame["iidM"] == iidM,"numPartners"]  = length(slice)
    menRecScoreFrame[menRecScoreFrame["iidM"] == iidM,"numMatches"]  = sum(slice)
    for(n in 1:10){
      menRecScoreFrame[menRecScoreFrame["iidM"] == iidM,paste(colName,toString(n),sep="_")] = sum(slice[1:n])
    }
  }
}

baselinePercents = c()
refinedMenPercents = c()
bestGuessPercents = c()
maxPercents = c()

for(i in 1:10){
  baseline = menRecScoreFrame[[paste("baseline",toString(i),sep="_")]]
  refinedMen = menRecScoreFrame[[paste("refinedDecMProbRectopN",toString(i),sep="_")]]
  bestGuess = menRecScoreFrame[[paste("matchGuessRectopN",toString(i),sep="_")]]
  m = menRecScoreFrame[[paste("numMatches")]]
  
  baselinePercents[i] = table(baseline > 0)[[2]]/length(baseline)
  refinedMenPercents[i] = table(refinedMen > 0)[[2]]/length(refinedMen)
  bestGuessPercents[i] = table(bestGuess > 0)[[2]]/length(bestGuess)
  maxPercents[i] = table(m > 0)[[2]]/length(m)
  
}


Percent = 100*baselinePercents
plot(Percent, type="o", col="blue",xlim=c(1,7), ylim=c(15,75),
     xlab=expression(paste('N')))
title(main=">= 1 Match in Top N Recs (men)", col.main="red", font.main=4)
lines(100*refinedMenPercents, type="o", col="red")

lines(100*bestGuessPercents, type="o", col="green")
lines(100*maxPercents, type="o", col="purple")
legend(4.2, 35, c("Baseline","Men prefs", "Composite"), cex=0.8, 
       col=c("blue","red", "green"), pch=21:23, lty=1:3);

menRecScoreFrameOld = menRecScoreFrame
menRecScoreFrame = menRecScoreFrame[menRecScoreFrame[["numMatches"]] > 0,]
percentPossible = c()
for(i in 1:10){
  possible =  menRecScoreFrame[["numMatches"]]
  baseline = menRecScoreFrame[[paste("baseline",toString(i),sep="_")]]/menRecScoreFrame[["numMatches"]]
  refinedMen = menRecScoreFrame[[paste("refinedDecMProbRectopN",toString(i),sep="_")]]/menRecScoreFrame[["numMatches"]]
  bestGuess = menRecScoreFrame[[paste("matchGuessRectopN",toString(i),sep="_")]]/menRecScoreFrame[["numMatches"]]
  baseline = ifelse(is.na(baseline), 1, baseline)
  refinedMen = ifelse(is.na(refinedMen), 1, refinedMen)
  bestGuess = ifelse(is.na(bestGuess), 1, bestGuess)
  baselinePercents[i] = table(baseline == 1)[[2]]/length(baseline)
  refinedMenPercents[i] = table(refinedMen == 1)[[2]]/length(refinedMen)
  bestGuessPercents[i] = table(bestGuess == 1)[[2]]/length(bestGuess)
  percentPossible[i] = table(possible <= i)[[2]]/length(possible)
}
  
Percent = 100*baselinePercents
plot(Percent, type="o", col="blue",xlim=c(0,10), ylim=c(0,100))
title(main="All matches found", col.main="red", font.main=4)
lines(100*refinedMenPercents, type="o", col="red")
lines(100*bestGuessPercents, type="o", col="green")
lines(100*percentPossible, type="o", col="purple")


























#saglads;ghdl;askhglk;dshagkd;lsa 



womenRecDF = merged[c("iidW", "iidM", "wave", "decM", "decW", "match", probs)]
for(i in 1:length(probs)){
  womenRecDF[c(recTypes[i], recTypesDecM[i],recTypesDecW[i], recTypesMatch[i])] = 0
  
}
womenRecDF = womenRecDF[order(-womenRecDF["iidM"]),]
for(iidW in unique(womenRecDF[["iidW"]])){
  for(i in 1:length(probs)){
    slice = womenRecDF[womenRecDF["iidW"] == iidW, c(probs[i], "iidM", "decM", "decW", "match")]
    slice = slice[order(-slice[probs[i]]),]
    features = c(recTypes[i], recTypesDecM[i], recTypesDecW[i], recTypesMatch[i])
    womenRecDF[womenRecDF["iidW"] == iidW,features] = slice[c("iidM", "decM", "decW", "match")]
  }
}
womenRecScoreFrame = data.frame()
womenRecScoreFrame[1:length(unique(womenRecDF[["iidW"]])),] = 0
womenRecScoreFrame[["iidW"]] = unique(womenRecDF[["iidW"]])
womenRecScoreFrame["numPartners"] = 0
womenRecScoreFrame["numMatches"] = 0


for(topN in 1:10){
  womenRecScoreFrame[paste("baseline",toString(topN),sep="_")] = 0
  for(recType in recTypes){
    colName = gsub("$", "topN", recType)
    
    womenRecScoreFrame[paste(colName,toString(topN),sep="_")] = 0
  }
}

for(iidW in unique(womenRecScoreFrame[["iidW"]])){
  slice = womenRecDF[womenRecDF["iidW"] == iidW,"match"]
  scrambledSlice = sample(slice)
  for(n in 1:10){
    womenRecScoreFrame[womenRecScoreFrame["iidW"] == iidW, paste("baseline",toString(n),sep="_")] =sum(scrambledSlice[1:n]) 
  }
}



for(iidW in unique(womenRecScoreFrame[["iidW"]])){
  for(recType in recTypes){
    colName = gsub("$", "topN", recType)
    toBeSummed =  gsub("$", "Match", recType)
    slice = womenRecDF[womenRecDF["iidW"] == iidW, toBeSummed]
    womenRecScoreFrame[womenRecScoreFrame["iidW"] == iidW,"numPartners"]  = length(slice)
    womenRecScoreFrame[womenRecScoreFrame["iidW"] == iidW,"numMatches"]  = sum(slice)
    for(n in 1:10){
      womenRecScoreFrame[womenRecScoreFrame["iidW"] == iidW,paste(colName,toString(n),sep="_")] = sum(slice[1:n])
    }
  }
}

baselinePercents = c()
genericWomenPercents = c()
bestGuessPercents = c()
for(i in 1:10){
  baseline = womenRecScoreFrame[[paste("baseline",toString(i),sep="_")]]
  refinedWomen = womenRecScoreFrame[[paste("refinedDecWProbRectopN",toString(i),sep="_")]]
  bestGuess = womenRecScoreFrame[[paste("matchGuessRectopN",toString(i),sep="_")]]
  baselinePercents[i] = table(baseline > 0)[[2]]/length(baseline)
  refinedWomenPercents[i] = table(refinedWomen > 0)[[2]]/length(refinedWomen)
  bestGuessPercents[i] = table(bestGuess > 0)[[2]]/length(bestGuess)
}


Percent = 100*baselinePercents
plot(Percent, type="o", col="blue",xlim=c(2,6), ylim=c(30,75))
title(main="At least one match in Top N (women)", col.main="red", font.main=4)
lines(100*refinedWomenPercents, type="o", col="red")
lines(100*genericMatchGuessPercents, type="o", col="purple")
lines(100*bestGuessPercents, type="o", col="green")

legend(4.2, 43, c("Baseline","Women prefs only", "Composite"), cex=0.8, 
       col=c("blue","red", "green"), pch=21:23, lty=1:3);


#NEW STUFF
df = read.csv('~/Desktop/speedDating/speedDatingData.csv')

waves = unique(menRecDF[["wave"]])

currentWave = menRecDF[menRecDF["wave"] == 11,c("iidM", "iidW", "match", "decM", "decW", "matchGuess", "matchGuessRec")]
df = df[df["wave"] == 11,]

ordering = data.frame()
matches = data.frame()
orderingW = data.frame()
matchesW = data.frame()
for(iidM in unique(currentWave[["iidM"]])){
  ordering[toString(iidM),] = 0
  matches[toString(iidM),] = 0
}
for(iidW in unique(currentWave[["iidW"]])){
  orderingW[toString(iidW),] = 0
  matchesW[toString(iidW),] = 0
}
for(i in 1:length(unique(currentWave[["iidM"]]))){
  ordering[paste("round",toString(i), sep="_")] = 0
  orderingW[paste("round",toString(i), sep="_")] = 0
  
  matches[paste("round",toString(i), sep="_")] = 0
  matchesW[paste("round",toString(i), sep="_")] = 0
}
womenSeq = hash()
menSeq = hash()
for(iidM in unique(currentWave[["iidM"]])){
  menSeq[[toString(iidM)]] = c(0)
}
for(iidW in unique(currentWave[["iidW"]])){
  womenSeq[[toString(iidW)]] = c(0)
}
sortedWave7 = currentWave[order(-currentWave[["matchGuess"]]),]
sortedWave7 = sortedWave7[c("iidM", "iidW", "match", "matchGuess")]

s = 1:length(unique(currentWave[["iidM"]]))
for(i in 1:nrow(sortedWave7)){
  iidM = sortedWave7[i,"iidM"]
  iidW = sortedWave7[i,"iidW"]
  menEligibles = s[!(s %in% menSeq[[toString(iidM)]])]
  womenEligibles =  s[!(s %in% womenSeq[[toString(iidW)]])]
  eligibles = menEligibles[menEligibles %in% womenEligibles]
  if(length(eligibles) == 0){
    print("hello!")
  }
  else{
    idx = min(eligibles)
    menSeq[[toString(iidM)]][idx] = idx
    womenSeq[[toString(iidW)]][idx] = idx
    ordering[toString(iidM),idx] = iidW
    orderingW[toString(iidW),idx] = iidM
    
    matches[toString(iidM),idx] = sortedWave7[i,"match"]
    matchesW[toString(iidW),idx] = sortedWave7[i,"match"]
    
    
  }
}



df = df[order(df["order"]),]
dfW = df[df["gender"]==0,]

df = df[df["gender"]==1,]


dfMatrix = matrix(df[["match"]], nrow = length(unique(currentWave[["iidM"]])), ncol = length(unique(currentWave[["iidM"]])))
matchMatrix = as.matrix(matches)
dfWMatrix = matrix(dfW[["match"]], nrow = length(unique(currentWave[["iidM"]])), ncol = length(unique(currentWave[["iidM"]])))
matchMatrixW = as.matrix(matchesW)

baselineScore = c()
comparisonScore = c()
for(i in 1:21){
  baselineScore[i] = table(dfMatrix[,1:i])[[2]]
  comparisonScore[i] = table(matchMatrix[,1:i])[[2]]  
}



plot(baselineScore, type="o", col="blue",xlim=c(1,21), ylim=c(0,65),
     xlab=expression(paste('N')))
title(main=">= 1 Match in Top N Recs (men)", col.main="red", font.main=4)
lines(comparisonScore, type="o", col="red")


baselineScore = c()
comparisonScore = c()
for(i in 2:21){
  baselineScore[i] = table(rowSums(dfWMatrix[,1:1:i]) > 0)[[2]]
  comparisonScore[i] = table(rowSums(matchMatrixW[,1:i]) > 0)[[2]]
}



plot(baselineScore, type="o", col="blue",xlim=c(1,21), ylim=c(0,21),
     xlab=expression(paste('N')))
title(main=">= 1 Match in Top N Recs (men)", col.main="red", font.main=4)
lines(comparisonScore, type="o", col="red")

