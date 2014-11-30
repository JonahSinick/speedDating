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

merged = merged[merged[["wave"]] %in% waves,c("wave", "iidM", "iidW", "match", "genericConjProbCal" , "matchGuess")]
df = df[df[["wave"]] %in% waves & df["gender"] == 0, c("wave", "iid", "pid", "order")]

merged[c("order", "betterOrder")] = 0

for(i in 1:nrow(merged)){
  iidM = merged[i,"iidM"]
  iidW = merged[i,"iidW"]
  merged[i,"order"] = df[df[["iid"]] == iidW & df[["pid"]] == iidM, "order"]
}

processWave = function(wave, waveOrderSplit, maxOrMin, frac, probType){
  sorted = wave[order(-wave[probType]),]

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
    newData = newData[names(sort(-rowSums(newData))),]
    newData = newData[,names(sort(-colSums(newData)))]
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
  wave["expNum"] = sum(sliceNew[[probType]])
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
    wave[wave["iidM"] == iidM,][["expNumM"]] = sum(sliceNew[sliceNew["iidM"] == iidM,][[probType]])
  }
  for(iidW in unique(sliceNew[["iidW"]])){
    wave[wave["iidW"] == iidW,][["numW"]] = sum(sliceNew[sliceNew["iidW"] == iidW,][["match"]])
    wave[wave["iidW"] == iidW,][["expNumW"]] = sum(sliceNew[sliceNew["iidW"] == iidW,][[probType]])
  }
  wave["oldNumComb"] = wave["oldNumM"] + wave["oldNumW"]
  wave["numComb"] = wave["numM"] + wave["numW"]
  
  return(wave)
}
printMetricsNew = function(df, frac, waves, waveOrderSplit, maxOrMin, probType){
  expNum = 0
  expNumMen = 0
  expNumWomen = 0
  expNumPeople = 0
  
  for(w in waves){
    wave = df[df["wave"] == w,]
    currentWave = processWave(wave, waveOrderSplit, maxOrMin, frac,  probType)
    len = length(unique(currentWave[["iidM"]]))
    expNum = expNum + currentWave[["expNum"]][1]
    happyMen = length(unique(currentWave[currentWave["expNumM"] >= 1,][["iidM"]]))
    happyWomen = length(unique(currentWave[currentWave["expNumW"] >= 1,][["iidW"]]))
    expNumMen = expNumMen +  happyMen
    expNumWomen = expNumWomen + happyWomen
    expNumPeople = expNumMen + expNumWomen
  }  
  return(c(expNum,expNumMen,expNumWomen, expNumPeople))
}

optimizeParameter = function(df, waves, waveOrderSplit, probType){
  optimizedDF = data.frame()
  for(wave in waves){
    optimizedDF[toString(wave),] = 0
  }
  optimizedDF[c("expNum", "expNumMen", "expNumWomen", "expNumPeople",
                "bestNumFrac", "bestMenFrac", "bestWomenFrac", "bestPeopleFrac")] = 0
  for(w in waves){
    for(frac in seq(0.25,1,by=0.05)){
      currentWaves = c(w)
      sequence = printMetricsNew(df, frac, currentWaves, waveOrderSplit, maxOrMin="max", probType)
      if(sequence[1] >= optimizedDF[toString(w),"expNum"]){
        optimizedDF[toString(w),"expNum"] = sequence[1]
        optimizedDF[toString(w),"bestNumFrac"] = frac
      }
      if(sequence[2] >= optimizedDF[toString(w),"expNumMen"]){
        optimizedDF[toString(w),"expNumMen"] = sequence[2]
        optimizedDF[toString(w),"bestMenFrac"] = frac
      }
      if(sequence[3] >= optimizedDF[toString(w),"expNumWomen"]){
        optimizedDF[toString(w),"expNumWomen"] = sequence[3]
        optimizedDF[toString(w),"bestWomenFrac"] = frac
      }
      if(sequence[4] >= optimizedDF[toString(w),"expNumPeople"]){
        optimizedDF[toString(w),"expNumPeople"] = sequence[4]
        optimizedDF[toString(w),"bestPeopleFrac"] = frac
      }
      print(round(c(frac,w),2))
      print(round(sequence,2))
      print(round(optimizedDF[toString(w),],2))
    }
  }  
  return(optimizedDF)
}

optimized = optimizeParameter(merged, waves, 2,  "matchGuess")
optimized2 = optimizeParameter(merged, waves, 2, "genericConjProbCal")