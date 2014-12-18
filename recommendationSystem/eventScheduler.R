
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
printMetricsTemp = function(df, frac, waves, waveOrderSplit, maxOrMin, probType){
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

optimizeParameter = function(df, waves, waveOrderSplit, probType, tries){
  optimizedDF = data.frame()
  for(wave in waves){
    optimizedDF[toString(wave),] = 0
  }
  optimizedDF[c("expNum", "expNumMen", "expNumWomen", "expNumPeople",
                "bestNumFrac", "bestMenFrac", "bestWomenFrac", "bestPeopleFrac")] = 0
  for(w in waves){
    for(frac in tries){
      currentWaves = c(w)
      sequence = printMetricsTemp(df, frac, currentWaves, waveOrderSplit, maxOrMin="max", probType)
      cat("Wave ", currentWaves,
          " frac ", frac,
          " ExpNum ", sequence[1], 
          " ExpMen ", sequence[2],
          " ExpWomen ", sequence[3], 
          " ExpPeople ", sequence[4], 
          "\n", sep="")
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
    }
  }  
  return(optimizedDF)
}
makeWaveHash = function(df, waves, waveOrderSplit, frac, probType){
  waveHash = hash()
  for(wave in waves){
    slice = df[df["wave"] == wave,]
    waveHash[[toString(wave)]] = processWave(slice, waveOrderSplit, "max", frac,  probType)
  }
  return(waveHash)
}

printScheduleMetrics = function(waveHash){
  df = data.frame()
  df["placeHolder",] = 0
  oldNum = 0
  newNum = 0
  for(wave in keys(waveHash)){
    currentWave = waveHash[[wave]]
    oldNum = oldNum + mean(currentWave[["oldNum"]])
    newNum = newNum + mean(currentWave[["num"]])
    df = rbind(df,currentWave)
  }
  oldNumMPos = length(unique(df[df["oldNumM"] > 0,][["iidM"]]))
  oldNumWPos = length(unique(df[df["oldNumW"] > 0,][["iidW"]]))
  oldPeoplePos = oldNumMPos + oldNumWPos
  numMPos = length(unique(df[df["numM"] > 0,][["iidM"]]))
  numWPos = length(unique(df[df["numW"] > 0,][["iidW"]]))
  newPeoplePos = numMPos + numWPos
  matches = df[df["match"] == 1 & df["betterOrder"] > 0,]
  fracNumMEnd = round((length(unique(matches[["iidM"]]))/109),2)
  fracNumWEnd = round((length(unique(matches[["iidW"]]))/100),2)
  fracNumPeopleEnd = round((length(unique(matches[["iidW"]])) + (length(unique(matches[["iidM"]]))))/209,2)
  fracNumMatchesEnd = round(nrow(matches)/364,2)
  cat("Men with >= 1 match: Old ", oldNumMPos, " New ", numMPos, " Ratio ", round((numMPos/oldNumMPos),2), " Percent End ", fracNumMEnd, "\n", sep="")
  cat("Women with >= 1 match: Old ", oldNumWPos, " New ", numWPos, " Ratio ", round((numWPos/oldNumWPos),2), " Percent End ", fracNumWEnd, "\n", sep="")
  cat("People with >= 1 match: Old ", oldPeoplePos, " New ", newPeoplePos, " Ratio ", round((newPeoplePos/oldPeoplePos),2), " Percent End ", fracNumPeopleEnd, "\n", sep="")
  cat("Number of matches: Old:", oldNum, " New: ", newNum, " Ratio ", round((newNum/oldNum),2), " Percent End ", fracNumMatchesEnd, "\n", sep="")
  return(df)
}
