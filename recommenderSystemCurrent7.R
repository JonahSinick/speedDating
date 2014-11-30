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



makeScoreFrame = function(df, waveOrderSplit){
  df[c("oldOrderQuart", "betterOrderQuart")] = 0
  
  scoreFrame = data.frame()
  waves = unique(df[["wave"]])
  len = length(unique(df[["iidM"]]))
  for(wave in waves){
    scoreFrame[toString(wave),] = 0
  }
  scoreFrame[c("numDates", "numPartners", "numMatches")] = 0  
  colNames = c("baseNum_", "betterNum_", "baseNumM_", "betterNumM_", "baseNumW_", "betterNumW_")
  for(name in colNames){
    for(i in 1:waveOrderSplit){
      scoreFrame[gsub("$", toString(i), colNames)] = 0
    }
  }
  for(wave in waves){
    slice = df[df["wave"] == wave,]
    scoreFrame[toString(wave),c("numDates", "numPartners", "numMatches")] = c(nrow(slice), length(unique(slice[["iidM"]])), sum(slice[["match"]]))
    divider = length(unique(slice[["order"]]))/waveOrderSplit
    for(i in 1:waveOrderSplit){
      limit = floor(i*divider)      
      olderOrderSlice = slice[slice[["order"]] %in% 1:limit,]
      betterOrderSlice = slice[slice[["betterOrder"]] %in% 1:limit,]
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





scorer = function(df, waveOrderSplit, signRow, signCol, maxOrMin, frac){
  df[c("oldOrderQuart", "betterOrder", "betterOrderQuart")] = 0
  for(wave in unique(df[["wave"]])){
    waveC = df[df["wave"] == wave,]
    sorted = waveC[order(-waveC["matchGuess"]),]
    orderFrame = data.frame()
    for(iidW in unique(sorted[["iidW"]])){
      for(iidM in unique(sorted[["iidM"]])){
        orderFrame[toString(iidW),toString(iidM)] = 0
      }      
    }
    otherDivider = length(unique(sorted[["order"]]))/waveOrderSplit
    len = nrow(orderFrame)
    divider = nrow(sorted)*frac
    maxQuantile = ceiling(1/frac)
    
    for(quantile in 1:maxQuantile){
      newData = data.frame()
      final = min(ceiling(quantile*divider), nrow(sorted))
      topPortion = sorted[1:final,]
      print(nrow(topPortion))
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
          frees = frees[frees <= quantile*floor(len/waveOrderSplit)]
          print(sequence)
          print(frees)
          if(length(frees) > 0 & orderFrame[iidW,iidM] == 0){
            if(maxOrMin == "max"){
              orderFrame[iidW,iidM] = max(frees)
            }
            else{
              orderFrame[iidW,iidM] = min(frees)
            }
            df[df["iidM"] == as.integer(iidM) & df["iidW"] == as.integer(iidW), "betterOrder"] = orderFrame[iidW,iidM]
          }
        }
      }
    }
    otherDivider = length(unique(slice[["order"]]))/waveOrderSplit
    
    for(i in 1:waveOrderSplit){
      limit = floor(i*divider)      
      olderOrderSlice = slice[slice[["order"]] %in% 1:limit,]
      betterOrderSlice = slice[slice[["betterOrder"]] %in% 1:limit,]
    }
  }
  return(df)
}
df = scorer(df = merged, waveOrderSplit = 2, signRow = -1, signCol = -1, maxOrMin = "max", frac = 0.5)
for(x in c(0.35,0.4,0.45,0.5,0.55,0.6, 0.65, 0.7,0.75,0.8)){
  df = scorer(df = merged, waveOrderSplit = 2, signRow = -1, signCol = -1, maxOrMin = "max", frac = x)
  slice = df[df["betterOrderQuart"] == 1,]
  print(x)
  print(sum(slice[["matchGuess"]]))
}
scoreFrame = makeScoreFrame(df, 2)



scoreFrame = makeScoreFrame(df, 1 )
colSums(scoreFrame[,])
ordered = df[order(df["iidM"],-df["matchGuess"]),]
orderedWave = ordered[ordered["wave"] == 11,]
orderedWave
orderedWave[c("numMatchesM", "numMatchesW")] = 0
for(iidM in unique(orderedWave[["iidM"]])){
  orderedWave[orderedWave["iidM"] == iidM,][["numMatchesM"]] = sum(orderedWave[orderedWave["iidM"] == iidM,][["match"]])
}
for(iidW in unique(orderedWave[["iidW"]])){
  orderedWave[orderedWave["iidW"] == iidW,][["numMatchesW"]] = sum(orderedWave[orderedWave["iidW"] == iidW,][["match"]])
}

slice = orderedWave[order(-orderedWave["matchGuess"]),]
slice[1:floor(nrow(slice)/2),]
slice[slice["iidM"] == 289 | slice["iidW"] == 268,]
sum(slice[["match"]])
sum(orderedWave[["match"]])
colSums(scoreFrame)
newSlice= orderedWave[orderedWave["iidW"] == 268,]
newSlice[order(-newSlice["matchGuess"]),]
newSlice= orderedWave[orderedWave["iidM"] == 293,]
newSlice[order(-newSlice["matchGuess"]),]
