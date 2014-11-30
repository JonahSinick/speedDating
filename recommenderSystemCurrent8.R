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
  scoreFrame = data.frame()
  waves = unique(df[["wave"]])
  for(wave in waves){
    scoreFrame[toString(wave),] = 0
  }
  scoreFrame[c("numDates", "numPartners", "numMatches")] = 0
  
  
  colNames = c("baseNum_", "betterNum_", "baseNumM_", "betterNumM_", "baseNumW_", "betterNumW_")
  print("here")
  
  for(name in colNames){
    for(i in 1:waveOrderSplit){
      scoreFrame[gsub("$", toString(i), colNames)] = 0
    }
  }
  for(wave in waves){
    scoreFrame[toString(wave),c("numDates", "numPartners", "numMatches")] = c(nrow(waveC), len, sum(waveC[["match"]]))
    
    slice = df[df["wave"] == wave,]
    for(i in 1:waveOrderSplit){
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





scorer = function(df, rankingSplit, waveOrderSplit, signRow, signCol, maxOrMin, reverse){
  for(wave in unique(df[["wave"]])){
    waveC = df[df["wave"] == wave,]
    sorted = waveC[order(signRow*waveC["matchGuess"]),]
    orderFrame = data.frame()
    for(iidW in unique(sorted[["iidW"]])){
      orderFrame[toString(iidW),] = 0
    }
    for(iidM in unique(sorted[["iidM"]])){
      orderFrame[toString(iidM)] = 0
    }
    len = nrow(orderFrame)
    rankingSplitDivider = nrow(waveC)/rankingSplit
    waveOrderDivider = len/waveOrderSplit
    for(quantile in 1:waveOrderSplit){
      conditions = (df["wave"] == wave) & (df["order"] <= quantile*waveOrderDivider) & (df["oldOrderQuart"] == 0)
      df[conditions,][["oldOrderQuart"]] = quantile
    }
    for(quantile in 1:floor(rankingSplit)){
      topPortion = sorted[1:floor(quantile*rankingSplitDivider),]
      topPortion = topPortion[order(signCol*topPortion["matchGuess"]),]
      iidWs = unique(topPortion[["iidW"]])
      iidMs = unique(topPortion[["iidM"]])  

      for(i in 1:nrow(topPortion)){
        iidM = toString(topPortion[i,"iidM"])
        iidW = toString(topPortion[i,"iidW"])
        womanTakens = orderFrame[iidW,]
        menTakens = orderFrame[,iidM]
        sequence = 1:len
        frees = sequence[!(sequence %in% c(manTakens, womanTakens))]
        if(reverse == FALSE){
          eligibleFrees = frees[frees <= quantile*waveOrderDivider]
        }
        if(reverse == TRUE){
          eligibleFrees = frees[frees >= quantile*waveOrderDivider]
        }
        
        if(length(eligibleFrees) > 0 & orderFrame[iidW,iidM] == 0){
          if(maxOrMin == "min"){
            orderFrame[iidW,iidM] = min(eligibleFrees)
          }
          if(maxOrMin == "max"){
            orderFrame[iidW,iidM] = max(eligibleFrees)
          }          
          df[df["iidM"] == as.integer(iidM) & df["iidW"] == as.integer(iidW), "betterOrder"] = orderFrame[iidW,iidM]
          df[df["iidM"] == as.integer(iidM) & df["iidW"] == as.integer(iidW), "betterOrderQuart"] = quantile
        }
      }
    }
  }
  return(df)
}

df = scorer(df = merged, rankingSplit = 2, waveOrderSplit = 2, signRow = 1, signCol = -1, maxOrMin = "min", reverse = TRUE)

scoreFrame = makeScoreFrame(df, 2 )
scoreFrame
colSums(scoreFrame)
colSums(scoreFrame[3,])
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
sum(slice[["match"]])
sum(orderedWave[["match"]])
colSums(scoreFrame)
newSlice= orderedWave[orderedWave["iidW"] == 268,]
newSlice[order(-newSlice["matchGuess"]),]
newSlice= orderedWave[orderedWave["iidM"] == 293,]
newSlice[order(-newSlice["matchGuess"]),]
