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
df = read.csv('~/Desktop/speedDating/speedDatingData.csv')
waves = c(7, 9, 11, 12, 19, 21, 4)
merged = merged[merged[["wave"]] %in% waves,c("wave", "iidM", "iidW", "match", "matchGuess")]
df = df[df[["wave"]] %in% waves & df["gender"] == 0, c("wave", "iid", "pid", "order")]

merged[c("order", "betterOrder", "oldOrderQuart", "betterOrderQuart")] = 0


for(i in 1:nrow(merged)){
  iidM = merged[i,"iidM"]
  iidW = merged[i,"iidW"]
  merged[i,"order"] = slice[slice[["iid"]] == iidW & slice[["pid"]] == iidM, "order"]
}

scoreFrame = data.frame()

for(wave in waves){
  scoreFrame[toString(wave),] = 0
}

scoreFrame["total",] = 0
scoreFrame[c("numDates", "numPartners", "numMatches")] = 0
colNames = c("baseNum_", "betterNum_", "baseNumM_", "betterNumM_", "baseNumW_", "betterNumW_")
for(name in colNames){
  for(i in 1:4){
    scoreFrame[gsub("$", toString(i), colNames)] = 0
  }
}




for(wave in waves){
  waveC = merged[merged["wave"] == wave,]
  sorted = waveC[order(-waveC["matchGuess"]),]
  orderFrame = data.frame()
  for(iidW in unique(sorted[["iidW"]])){
    orderFrame[toString(iidW),] = 0
  }
  for(iidM in unique(sorted[["iidM"]])){
    orderFrame[toString(iidM)] = 0
  }
  len = nrow(orderFrame)
  dividerWaveC = nrow(waveC)/4
  dividerIIDs = len/4
  for(quarter in 1:4){
    newData = data.frame()
    topPortion = sorted[1:floor(quarter*dividerWaveC),]
    for(iidW in unique(topPortion[["iidW"]])){
      newData[toString(iidW),] = 0
    }
    for(iidM in unique(topPortion[["iidM"]])){
      newData[,toString(iidM)] = 0
    }
    for(i in 1:nrow(topPortion)){
      iidM = topPortion[i,"iidM"]
      iidW = topPortion[i,"iidW"]
      newData[toString(iidW),toString(iidM)] = 1
    }
    
    newData = newData[names(sort(rowSums(newData))),]
    newData = newData[,names(sort(colSums(newData)))]
    iidWs = rownames(newData)
    iidMs = colnames(newData)
    for(iidW in iidWs){
      for(iidM in iidMs){
        manTakens = orderFrame[,iidM]
        womanTakens = orderFrame[iidW,]
        sequence = 1:len
        frees = sequence[!(sequence %in% c(manTakens, womanTakens))]
        eligibleFrees = frees[frees <= floor(quarter*dividerIIDs)]
        if(length(eligibleFrees) > 0 & orderFrame[iidW,iidM] == 0){
          orderFrame[iidW,iidM] = max(eligibleFrees)
          merged[merged["iidM"] == iidM & merged["iidW"] == iidW, c("betterOrder", "betterOrderQuart")] = c(max(eligibleFrees), quarter)
        }
      }
    }
  }
}
origMatchMatrixHash = hash()
betterMatchMatrixHash = hash()
for(wave in waves){
  slice = merged[merged["wave"] == wave,]
  m = max(slice[["newOrder"]])
  slice = slice[order(slice["newOrder"]),]
  betterMatchMatrixHash[[toString(wave)]] = matrix(slice[["match"]], nrow=m, ncol=m)
  slice = slice[order(slice["order"]),]
  origMatchMatrixHash[[toString(wave)]] = matrix(slice[["match"]], nrow=m, ncol=m)
}

evalHash = hash()
for(wave in waves){
  orig = origMatchMatrixHash[[toString(wave)]]
  better = betterMatchMatrixHash[[toString(wave)]]
  string = paste("wave",toString(wave),sep="")
  evalHash[[paste(string,"numPeople",sep="_")]] = ncol(orig)
  evalHash[[paste(string,"numMatches",sep="_")]] = table(orig)[[2]]
  for(i in 1:4){
    newString = paste(string,paste("quartile",toString(i),sep="_"),sep="_")
    origSlice = orig[,1:floor(i*ncol(orig)/4)]
    betterSlice = better[,1:floor(i*ncol(better)/4)]
    evalHash[[paste(newString, "betterNum")]] = table(betterSlice)[[2]]
    evalHash[[paste(newString, "origNum")]] = table(origSlice)[[2]]
    evalHash[[paste(newString, "origAtLeastOne")]] = evalHash[[paste(string,"numMatches",sep="_")]]-  table(rowSums(origSlice) > 0)[[1]]
    evalHash[[paste(newString, "betterAtLeastOne")]] = evalHash[[paste(string,"numMatches",sep="_")]]  - table(rowSums(betterSlice) > 0)[[1]]
  }
}
waveC = merged[merged["wave"] == wave,]
sorted = waveC[order(-waveC["matchGuess"]),]
firstPortion = sorted[1:(floor(nrow(sorted)/4)),]
newData = data.frame()
for(iidW in unique(firstPortion[["iidW"]])){
  newData[toString(iidW),] = 0
}
for(iidM in unique(firstPortion[["iidM"]])){
  newData[toString(iidM)] = 0
}
for(i in 1:nrow(firstPortion)){
  iidM = firstPortion[i,"iidM"]
  iidW = firstPortion[i,"iidW"]
  newData[toString(iidW),toString(iidM)] = 1
}

for(iid in c(iidsW, iidsM)){
  
}
newData


sortedAgain = newData[names(sort(rowSums(newData))),]
clonedAgain = sortedAgain
clonedAgain[1:14,] =0 
sortedAgain
for(i in 1:nrow(sortedAgain)){
  eligibles = names(clonedAgain[colSums(clonedAgain) < 5 ])
  print(eligibles)
  eligibles = names(sortedAgain[sortedAgain[i,eligibles] == 1])
  print(eligibles)
}
