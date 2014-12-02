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
  merged[i,"order"] = df[df[["iid"]] == iidW & df[["pid"]] == iidM, "order"]
}





scorer = function(merged, split, signRow, signCol, maxOrMin){
  scoreFrame = data.frame()
  scoreFrame[c("numDates", "numPartners", "numMatches")] = 0
  for(wave in waves){
    scoreFrame[toString(wave),] = 0
  }
  colNames = c("baseNum_", "betterNum_", "baseNumM_", "betterNumM_", "baseNumW_", "betterNumW_")
  
  
  for(name in colNames){
    for(i in 1:2){
      scoreFrame[gsub("$", toString(i), colNames)] = 0
    }
  }
  for(wave in unique(merged[["wave"]])){
    
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
    scoreFrame[toString(wave),c("numDates", "numPartners", "numMatches")] = c(nrow(waveC), len, sum(waveC[["match"]]))
    dividerWaveC = nrow(waveC)/split
    dividerIIDs = len/split
    for(quarter in 1:split){
      conditions = (merged["wave"] == wave) & (merged["order"] <= quarter*dividerIIDs) & (merged["oldOrderQuart"] == 0)
      merged[conditions,][["oldOrderQuart"]] = quarter
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
      
      newData = newData[names(sort(signRow*rowSums(newData))),]
      newData = newData[,names(sort(signCol*colSums(newData)))]
      iidWs = rownames(newData)
      iidMs = colnames(newData)
      for(iidW in iidWs){
        for(iidM in iidMs){
          manTakens = orderFrame[,iidM]
          womanTakens = orderFrame[iidW,]
          sequence = 1:len
          frees = sequence[!(sequence %in% c(manTakens, womanTakens))]
          eligibleFrees = frees[frees <= quarter*dividerIIDs]
          if(length(eligibleFrees) > 0 & orderFrame[iidW,iidM] == 0){
            if(maxOrMin == "max"){
              orderFrame[iidW,iidM] = max(eligibleFrees)
            }
            else{
              orderFrame[iidW,iidM] = min(eligibleFrees)
            }
            merged[merged["iidM"] == as.integer(iidM) & merged["iidW"] == as.integer(iidW), "betterOrder"] = orderFrame[iidW,iidM]
            merged[merged["iidM"] == as.integer(iidM) & merged["iidW"] == as.integer(iidW), "betterOrderQuart"] = quarter
          }
        }
      }
    }
  }
  for(wave in waves){
    slice = merged[merged["wave"] == wave,]
    for(i in 1:split){
      olderOrderSlice = slice[slice[["oldOrderQuart"]] %in% 1:i,]
      betterOrderSlice = slice[slice[["betterOrderQuart"]] %in% 1:i,]
      scoreFrame[toString(wave), paste("baseNum",toString(i),sep="_")] = sum(olderOrderSlice[["match"]])
      scoreFrame[toString(wave), paste("betterNum",toString(i),sep="_")] = sum(betterOrderSlice[["match"]])
      scoreFrame[toString(wave), paste("baseNumM",toString(i),sep="_")] = length(unique(olderOrderSlice[["iidM"]]))
      scoreFrame[toString(wave), paste("betterNumM",toString(i),sep="_")] = length(unique(betterOrderSlice[["iidM"]]))
      scoreFrame[toString(wave), paste("baseNumW",toString(i),sep="_")] = length(unique(olderOrderSlice[["iidW"]]))
      scoreFrame[toString(wave), paste("betterNumW",toString(i),sep="_")] = length(unique(betterOrderSlice[["iidW"]]))
    }
  }
  for(wave in waves){
    slice = merged[merged["wave"] == wave,]
    for(i in 1:split){
      olderOrderSlice = slice[slice[["oldOrderQuart"]] %in% 1:i,]
      betterOrderSlice = slice[slice[["betterOrderQuart"]] %in% 1:i,]
      menMatchesOld = 0
      womenMatchesOld = 0
      menMatchesBetter = 0
      womenMatchesBetter = 0
      
      for(j in unique(slice[["iidM"]])){
        if(sum(olderOrderSlice[olderOrderSlice["iidM"] == j,][["match"]]) > 0){
          menMatchesOld = menMatchesOld + 1
        }
        if(sum(betterOrderSlice[betterOrderSlice["iidM"] == j,][["match"]]) > 0){
          menMatchesBetter = menMatchesBetter + 1
        }
      }
      for(j in unique(slice[["iidW"]])){
        if(sum(olderOrderSlice[olderOrderSlice["iidW"] == j,][["match"]]) > 0){
          womenMatchesOld = womenMatchesOld + 1
        }
        if(sum(betterOrderSlice[betterOrderSlice["iidW"] == j,][["match"]]) > 0){
          womenMatchesBetter = womenMatchesBetter + 1
        }
      }
      scoreFrame[toString(wave), paste("baseNum",toString(i),sep="_")] = sum(olderOrderSlice[["match"]])
      scoreFrame[toString(wave), paste("betterNum",toString(i),sep="_")] = sum(betterOrderSlice[["match"]])
      scoreFrame[toString(wave), paste("baseNumM",toString(i),sep="_")] = menMatchesOld
      scoreFrame[toString(wave), paste("betterNumM",toString(i),sep="_")] = menMatchesBetter
      scoreFrame[toString(wave), paste("baseNumW",toString(i),sep="_")] = womenMatchesOld
      scoreFrame[toString(wave), paste("betterNumW",toString(i),sep="_")] = womenMatchesBetter
    }
  }
  return(scoreFrame)
}

scoreFrame = scorer(merged,4,1,1,"max")

colSums(scoreFrame)
