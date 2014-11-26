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


merged = read.csv( '~/Desktop/speedDating/mergedGuessesAdded.csv')

probs = names(merged)[231:238]
recTypes = gsub("$", "Rec", probs)
recTypesDecM = gsub("$", "RecDecM", probs)
recTypesDecW = gsub("$", "RecDecW", probs)
recTypesMatch = gsub("$", "RecMatch", probs)


menRecDF = merged[c("iidW", "iidM", "decM", "decW", "match", probs)]
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
n = names(menRecScoreFrame)
fives = n[grep("_5", n)]
slice = menRecScoreFrame[fives]
for(name in fives){
  print(name)
  print(mean(menRecScoreFrame[[name]]))
}

n[grep("baseline_|calBetter|betterDecM", n)]
h = hash()
baselinePercents = c()
manOnlyPercents = c()
calMatchPercents = c()
baselineAvgs = c()
manOnlyAvgs = c()
calMatchAvgs = c()
for(i in 1:10){
  baselineSlice = menRecScoreFrame[[paste("baseline",toString(i),sep="_")]]
  manSlice = menRecScoreFrame[[paste("betterDecMProbsRectopN",toString(i),sep="_")]]
  calProbSlice = menRecScoreFrame[[paste("calBetterProbRectopN",toString(i),sep="_")]]
  baselinePercents[i] = table(baselineSlice > 0)[[2]]/length(baselineSlice)
  manOnlyPercents[i] = table(manSlice > 0)[[2]]/length(manSlice)
  calMatchPercents[i] = table(calProbSlice > 0)[[2]]/length(calProbSlice)
  baselineAvgs[i] = mean(baselineSlice)
  manOnlyAvgs[i] = mean(manSlice)
  calMatchAvgs[i] = mean(calProbSlice)
}


Percent = 100*baselinePercents[1:7]
plot(Percent, type="o", col="blue",xlim=c(2,6), ylim=c(30,75))
title(main="At least one match in Top N (men)", col.main="red", font.main=4)

lines(100*manOnlyPercents, type="o", col="red")
lines(100*calMatchPercents, type="o", col="green")
legend(4.2, 43, c("Baseline","Men prefs only", "Composite"), cex=0.8, 
       col=c("blue","red", "green"), pch=21:23, lty=1:3);





matches = baselineAvgs
plot(AverageNumMatches, type="o", col="blue",xlim=c(3,7),ylim=c(0.3,2))
title(main="Average number of matches (men)", col.main="red", font.main=4)

lines(manOnlyAvgs, type="o", col="red")
lines(calMatchAvgs, type="o", col="green")
legend(4.2, 43, c("Baseline","Men prefs only", "Composite"), cex=0.8, 
       col=c("blue","red", "green"), pch=21:23, lty=1:3);

























#saglads;ghdl;askhglk;dshagkd;lsa 



womenRecDF = merged[c("iidW", "iidM", "decM", "decW", "match", probs)]
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
n = names(womenRecScoreFrame)
fives = n[grep("_5", n)]
slice = womenRecScoreFrame[fives]
for(name in fives){
  print(name)
  print(mean(womenRecScoreFrame[[name]]))
}

n[grep("baseline_|calBetter|betterDecW", n)]
h = hash()
baselinePercents = c()
womanOnlyPercents = c()
calMatchPercents = c()
baselineAvgs = c()
womanOnlyAvgs = c()
calMatchAvgs = c()
for(i in 1:10){
  baselineSlice = womenRecScoreFrame[[paste("baseline",toString(i),sep="_")]]
  womanSlice = womenRecScoreFrame[[paste("betterDecWProbsRectopN",toString(i),sep="_")]]
  calProbSlice = womenRecScoreFrame[[paste("calBetterProbRectopN",toString(i),sep="_")]]
  baselinePercents[i] = table(baselineSlice > 0)[[2]]/length(baselineSlice)
  womanOnlyPercents[i] = table(womanSlice > 0)[[2]]/length(womanSlice)
  calMatchPercents[i] = table(calProbSlice > 0)[[2]]/length(calProbSlice)
  baselineAvgs[i] = mean(baselineSlice)
  womanOnlyAvgs[i] = mean(womanSlice)
  calMatchAvgs[i] = mean(calProbSlice)
}


Percent = 100*baselinePercents[1:10]
plot(Percent, type="o", col="blue",xlim=c(2,10), ylim=c(30,80))
title(main="At least one match in Top N (women)", col.main="red", font.main=4)

lines(100*womanOnlyPercents, type="o", col="red")
lines(100*calMatchPercents, type="o", col="green")
lines(seq(79,79,length.out = 10), type="o", col="purple")

legend(4.2, 43, c("Baseline","Women prefs only", "Composite", "Limit"), cex=0.8, 
       col=c("blue","red", "green"), pch=21:23, lty=1:3);





matches = baselineAvgs
plot(AverageNumMatches, type="o", col="blue",xlim=c(3,7),ylim=c(0.3,2))
title(main="Average number of matches (women)", col.main="red", font.main=4)

lines(womanOnlyAvgs, type="o", col="red")
lines(calMatchAvgs, type="o", col="green")
legend(4.2, 43, c("Baseline","Women prefs only", "Composite"), cex=0.8, 
       col=c("blue","red", "green"), pch=21:23, lty=1:3);



