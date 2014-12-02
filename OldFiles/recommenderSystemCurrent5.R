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
slice = df[df[["wave"]] %in% waves & df["gender"] == 0, c("wave", "iid", "pid", "order")]

merged["order"] = 0

for(i in 1:nrow(merged)){
  iidM = merged[i,"iidM"]
  iidW = merged[i,"iidW"]
  merged[i,"order"] = slice[slice[["iid"]] == iidW & slice[["pid"]] == iidM, "order"]
}
merged["newOrder"] = 0

for(wave in c(7)){
  waveC = merged[merged["wave"] == wave,]
  sorted = waveC[order(-waveC["matchGuess"]),]
  newData = data.frame()
  for(iidW in unique(firstPortion[["iidW"]])){
    newData[toString(iidW),] = 0
  }
  for(iidM in unique(firstPortion[["iidM"]])){
    newData[toString(iidM)] = 0
  }
  quartile1 = sorted[1:(floor(nrow(sorted)/4)),]
  quartile2 = sorted[2:(floor(nrow(sorted)/4)),]
  quartile2 = sorted[2:(floor(nrow(sorted)/4)),]
  

  for(i in 1:nrow(firstPortion)){
    iidM = firstPortion[i,"iidM"]
    iidW = firstPortion[i,"iidW"]
    newData[toString(iidW),toString(iidM)] = 1
  }
  
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

for(iid in c(iidsW, iidsM)){
  
}
newData


