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

waves = unique(menRecDF[["wave"]])

wave7 = menRecDF[menRecDF["wave"] == 7,c("iidM", "iidW", "match", "decM", "decW", "matchGuess", "matchGuessRec")]



ordering = data.frame()
finalRecs[1:16,] = 0
for(i in 1:16){
  finalRecs[paste("round", toString(i), sep="_")] = 0
}
remainings = c()
iidsM = unique(wave7[["iidM"]])
for(iidM in iidsM){
  slice = wave7[wave7["iidM"] == iidM,]
  finalRecs[c(paste(toString(iidM),"probs",sep="_"),paste(toString(iidM),"recs",sep="_"))] = slice[c("matchGuess", "matchGuessRec")]
  finalRecs[c(paste(toString(iidM),"remaining",sep="_"))] = slice["matchGuessRec"]
}

remainings = n[grep( "remaining", n)]
for(i in 1:16){
  for(j in 1:16){
    for(iidW in final)
    slice = finalRecs[finalRecs[paste("round", toString(i), sep="_")] == 0 ,]
    slice = slice[order(slice[c(paste(toString(iidM),"probs",sep="_")])]
  }
}


for(iidM in unique(wave7[["iidM"]])){
  finalRecs[toString(iidM),][variables[1:2]] = wave7[wave7["iidM"] == iidM,][variables[1:2]]
}


x = toString(i)

iidWMatrix[variables] = 0
  for(iidM in unique(menRecDF[["iidM"]])){
    iidWMatrix[iidM,variables[1:2]] = menRecDF[variables[1:2]]
  }


for(iidM in unique(wave7[["iidM"]])){
  slice = wave7[wave7["iidM"] == iidM,]
  iidWMatrix[toString(iidM),] = c(slice[["matchGuessRec"]], slice[["matchGuess"]],seq(0,0,length.out = 16))
}

n = names(iidWMatrix)

newrecs = n[grep("newrec",n)]
recs = n[grep("rec",n)][1:16]
for(j in 1:16){
  for(iidM in unique(wave7[["iidM"]])){
    iidM = toString(iidM)
    takens1 = iidWMatrix[[newrecs[j]]]
    takens2 = as.list(iidWMatrix[iidM, newrecs])
    for(i in 1:16){
      if(iidWMatrix[iidM,newrecs[j]] == 0 & !(iidWMatrix[iidM,recs[i]] %in% takens1 ) & !(iidWMatrix[iidM,recs[i]] %in% takens2) ){ 
        iidWMatrix[iidM,newrecs[j]] = iidWMatrix[iidM,recs[i]]
      }      
    }
  }
}

iidWMatrix[newrecs]
