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
  df[c("betterOrder", "expectedScore")] = 0
  for(wave in unique(df[["wave"]])){
    currentWave = df[df["wave"] == wave,]
    processed = processWave(currentWave, waveOrderSplit, signRow, signCol, maxOrMin, frac)
    df[df["wave"] == wave,][c()]
  }
}

processWave = function(wave, waveOrderSplit, signRow, signCol, maxOrMin, frac){
  sorted = wave[order(-wave["matchGuess"]),]
  orderFrame = data.frame()
  for(iidW in unique(sorted[["iidW"]])){
    orderFrame[toString(iidW),] = 0
  }
  for(iidM in unique(sorted[["iidM"]])){
    orderFrame[toString(iidM)] = 0
  }
  for(iidW in unique(sorted[["iidW"]])){
    for(iidM in unique(sorted[["iidM"]])){
      orderFrame[toString(iidW),toString(iidM)] = 0
    }
  }
  numPeople = nrow(orderFrame)
  divider = nrow(sorted)*frac
  maxQuantile = ceiling(1/frac)
  for(quantile in 1:maxQuantile){
    newData = data.frame()
    limit = min(ceiling(quantile*divider), nrow(sorted))
    topPortion = sorted[1:limit,]
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
        sequence = 1:numPeople
        frees = sequence[!(sequence %in% c(manTakens, womanTakens))]
        frees = frees[frees <= quantile*floor(numPeople/waveOrderSplit)]
        if(length(frees) > 0 & orderFrame[iidW,iidM] == 0){
          orderFrame[iidW,iidM]  = ifelse(maxOrMin == "max",  max(frees),  min(frees))
          wave[wave["iidM"] == as.integer(iidM) & wave["iidW"] == as.integer(iidW), "betterOrder"] = orderFrame[iidW,iidM]
        }
      }
    }
  }
  sliceOld = wave[wave["order"] <= floor(numPeople/waveOrderSplit),]
  sliceNew = wave[wave["betterOrder"] <= floor(numPeople/waveOrderSplit) & wave["betterOrder"] > 0,]
  wave["oldNum"] = sum(sliceOld[["match"]])
  wave["expNum"] = sum(sliceNew[["matchGuess"]])
  wave["num"] = sum(sliceNew[["match"]])
  wave[c("oldNumM", "oldNumW", "expNumM", "expNumW", "numM", "numW")] = 0
  wave["sliceOldSize"] = nrow(sliceOld)
  wave["sliceNewSize"] = nrow(sliceNew)
  for(iidM in unique(sliceOld[["iidM"]])){
    wave[wave["iidM"] == iidM,][["oldNumM"]] = sum(sliceOld[sliceOld["iidM"] == iidM,][["match"]])
  }
  for(iidW in unique(sliceOld[["iidW"]])){
    wave[wave["iidW"] == iidW,][["oldNumW"]] = sum(sliceOld[sliceOld["iidW"] == iidW,][["match"]])
  }
  for(iidM in unique(sliceNew[["iidM"]])){
    wave[wave["iidM"] == iidM,][["numM"]] = sum(sliceNew[sliceNew["iidM"] == iidM,][["match"]])
    wave[wave["iidM"] == iidM,][["expNumM"]] = sum(sliceNew[sliceNew["iidM"] == iidM,][["matchGuess"]])
  }
  for(iidW in unique(sliceNew[["iidW"]])){
    wave[wave["iidW"] == iidW,][["numW"]] = sum(sliceNew[sliceNew["iidW"] == iidW,][["match"]])
    wave[wave["iidW"] == iidW,][["expNumW"]] = sum(sliceNew[sliceNew["iidW"] == iidW,][["matchGuess"]])
  }
  return(wave)
}
orderedWave = merged[merged["wave"] == 19,]
printMetricsNew = function(x, waves, split, type){
  expNum = 0
  expNumMen = 0
  expNumWomen = 0
  for(w in waves){
    newWave = processWave(wave=merged[merged["wave"] == w,], waveOrderSplit = split, signRow = -1, signCol = -1, maxOrMin = type, frac = x)
    len = length(unique(newWave[["iidM"]]))
    expNum = expNum + newWave[["expNum"]][1]
    happyMen = length(unique(newWave[newWave["expNumM"] >= 1,][["iidM"]]))
    happyWomen = length(unique(newWave[newWave["expNumW"] >= 1,][["iidW"]]))
    expNumMen = expNumMen +  happyMen
    expNumWomen = expNumWomen + happyWomen
  }  
  return(c(expNum,expNumMen,expNumWomen))
}
newDF = data.frame()
for(wave in waves){
  newDF[toString(wave),] = 0
}
newDF[c("expNum", "expNumMen", "expNumWomen", "bestNumFrac", "bestMenFrac", "bestWomenFrac")] = 0
for(w in waves){
  for(frac in seq(0.25,1,by=0.05)){
    sequence = printMetricsNew(x = frac, c(w), split = 2, type="max")
    print(c(frac,w))
    print(sequence)
    print(round(newDF[toString(w),],2))
    if(sequence[1] > newDF[toString(w),"expNum"]){
      newDF[toString(w),"expNum"] = sequence[1]
      newDF[toString(w),"bestNumFrac"] = frac
    }
    if(sequence[2] > newDF[toString(w),"expNumMen"]){
      newDF[toString(w),"expNumMen"] = sequence[2]
      newDF[toString(w),"bestMenFrac"] = frac
    }
    if(sequence[3] > newDF[toString(w),"expNumWomen"]){
      newDF[toString(w),"expNumWomen"] = sequence[3]
      newDF[toString(w),"bestWomenFrac"] = frac
    }
  }
}


printMetricsNew(x = 0.25, waves= waves, split = 2, type="max")
waveHash = hash()
waveHash[["7"]] = processWave(wave=merged[merged["wave"] == 7,], waveOrderSplit = 2, signRow = -1, signCol = -1, maxOrMin = "max", frac = 0.5)  
waveHash[["9"]] = processWave(wave=merged[merged["wave"] == 9,], waveOrderSplit = 2, signRow = -1, signCol = -1, maxOrMin = "max", frac = 0.5)  
waveHash[["11"]] = processWave(wave=merged[merged["wave"] == 11,], waveOrderSplit = 2, signRow = -1, signCol = -1, maxOrMin = "max", frac = 0.6)  
waveHash[["12"]] = processWave(wave=merged[merged["wave"] == 12,], waveOrderSplit = 2, signRow = -1, signCol = -1, maxOrMin = "max", frac = 0.6)  
waveHash[["19"]] = processWave(wave=merged[merged["wave"] == 19,], waveOrderSplit = 2, signRow = -1, signCol = -1, maxOrMin = "max", frac = 0.6)  

waveHash[["21"]] = processWave(wave=merged[merged["wave"] == 21,], waveOrderSplit = 2, signRow = -1, signCol = -1, maxOrMin = "max", frac = 0.6)  
waveHash[["4"]] = processWave(wave=merged[merged["wave"] == 4,], waveOrderSplit = 2, signRow = -1, signCol = -1, maxOrMin = "max", frac = 0.6)  
df = data.frame()
df["Alpha",] = 0
n = names(waveHash[["4"]])
df[n] = 0
for(wave in keys(waveHash)){
  df = rbind(df,waveHash[[wave]])
}

matches = df[df["match"]  ==1,]

oldNumMPos = df[df["oldNumM"] > 0,]
oldNumWPos = df[df["oldNumW"] > 0,]
numMPos = df[df["numM"] > 0,]
numWPos = df[df["numW"] > 0,]
length(unique(oldNumMPos[["iidM"]]))
length(unique(numMPos[["iidM"]]))
length(unique(oldNumWPos[["iidW"]]))
length(unique(numWPos[["iidW"]]))

sequence = c("oldNum","num", "oldNumM", "numM", "oldNumW", "numW")
df[sequence] =0

df = data.frame()
for(w in keys(waveHash)){
  df[toString(w),] = 0
}
for(w in keys(waveHash)){
  df[toString(w),sequence[1]] = waveHash[[w]][1,sequence[1]]
  df[toString(w),sequence[2]] = waveHash[[w]][1,sequence[2]]
}




df = data.frame()
for(w in keys(waveHash)){
  df[toString(w),] = 0
}
for(w in keys(waveHash)){
  df[toString(w),sequence[1]] = waveHash[[w]][1,sequence[1]]
  df[toString(w),sequence[2]] = waveHash[[w]][1,sequence[2]]
}





newDF[c("expNum", "expNumMen", "expNumWomen", "bestNumFrac", "bestMenFrac", "bestWomenFrac")] = 0
for(frac in seq(0.1,1,by=0.05)){
  for(w in waves){
    sequence = printMetricsNew(x = frac, waves= c(w))
    print(round(newDF,2))
    if(sequence[1] > newDF[toString(w),"expNum"]){
      newDF[toString(w),"expNum"] = sequence[1]
      newDF[toString(w),"bestNumFrac"] = frac
    }
    if(sequence[2] > newDF[toString(w),"expNumMen"]){
      newDF[toString(w),"expNumMen"] = sequence[2]
      newDF[toString(w),"bestMenFrac"] = frac
    }
    if(sequence[3] > newDF[toString(w),"expNumWomen"]){
      newDF[toString(w),"expNumWomen"] = sequence[3]
      newDF[toString(w),"bestWomenFrac"] = frac
    }
  }
}


type = "max"

waveHash = hash()
waveHash[["7"]] = processWave(wave=merged[merged["wave"] == 7,], waveOrderSplit = 3, signRow = -1, signCol = -1, maxOrMin = type, frac = 0.15)  
waveHash[["9"]] = processWave(wave=merged[merged["wave"] == 9,], waveOrderSplit = 3, signRow = -1, signCol = -1, maxOrMin = type, frac = 0.65)  
waveHash[["11"]] = processWave(wave=merged[merged["wave"] == 11,], waveOrderSplit = 3, signRow = -1, signCol = -1, maxOrMin = type, frac = 0.2)  
waveHash[["12"]] = processWave(wave=merged[merged["wave"] == 12,], waveOrderSplit = 3, signRow = -1, signCol = -1, maxOrMin = type, frac = 0.95)  
waveHash[["19"]] = processWave(wave=merged[merged["wave"] == 19,], waveOrderSplit = 3, signRow = -1, signCol = -1, maxOrMin = type, frac = 0.1)  

waveHash[["21"]] = processWave(wave=merged[merged["wave"] == 21,], waveOrderSplit = 3, signRow = -1, signCol = -1, maxOrMin = type, frac = 0.95)  
waveHash[["4"]] = processWave(wave=merged[merged["wave"] == 4,], waveOrderSplit = 3, signRow = -1, signCol = -1, maxOrMin = type, frac = 0.1)  
df = data.frame()
df["Alpha",] = 0
n = names(waveHash[["4"]])
df[n] = 0
for(wave in keys(waveHash)){
  df = rbind(df,waveHash[[wave]])
}


oldNumMPos = df[df["oldNumM"] > 0,]
oldNumWPos = df[df["oldNumW"] > 0,]
numMPos = df[df["numM"] > 0,]
numWPos = df[df["numW"] > 0,]
length(unique(oldNumMPos[["iidM"]]))
length(unique(numMPos[["iidM"]]))
length(unique(oldNumWPos[["iidW"]]))
length(unique(numWPos[["iidW"]]))


sequence = c("oldNum","num", "oldNumM", "numM", "oldNumW", "numW")
df[sequence] =0

df = data.frame()
for(w in keys(waveHash)){
  df[toString(w),] = 0
}
for(w in keys(waveHash)){
  df[toString(w),sequence[1]] = waveHash[[w]][1,sequence[1]]
  df[toString(w),sequence[2]] = waveHash[[w]][1,sequence[2]]
}
