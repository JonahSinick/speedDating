makeRatingSums = function(df, ratings){
  for(rating in ratings){
    ratingStub = gsub("Rating$", "", rating)
    raterSum = paste(ratingStub,"RaterSum",sep="")
    rateeSum = paste(ratingStub,"RateeSum",sep="")
    waveSum = paste(ratingStub,"WaveSum",sep="")
    df[c(raterSum,rateeSum, waveSum)] = 0
    for(iid in unique(df[["iid"]])){
      wave = df[df["iid"] == iid,"wave"][1]
      gender = df[df["iid"] == iid,"gender"][1]
      s = sum(df[df["iid"] == iid,rating])
      df[df["iid"] == iid,raterSum] = s
      df[df["wave"] == wave,waveSum] = df[df["wave"] == wave,waveSum] + s  
    }
    for(pid in unique(df[["pid"]])){
      s = sum(df[df["pid"] == pid,rating])
      df[df["pid"] == pid,rateeSum] = df[df["pid"] == pid,rateeSum] + s
    }
  }
  return(df)
}

makeSurrogates = function(df){
  df[c("surIID", "surPID")] = 0
  for(wave in unique(df[["wave"]])){
    slice = df[df["wave"] == wave,]
    iids = unique(slice[["iid"]])
    pids = unique(slice[["pid"]])
    for(i in 1:nrow(slice)){
      eligibleIIDs = iids[iids != slice[i,"iid"]]
      eligiblePIDs = pids[pids != slice[i,"pid"]]
      surIID = sample(eligibleIIDs, 1)
      surPID = sample(eligiblePIDs, 1)
      slice[i,"surIID"] = surIID
      slice[i,"surPID"] = surPID
    }
    df[df["wave"] == wave,][["surIID"]] = slice[["surIID"]]
    df[df["wave"] == wave,][["surPID"]] = slice[["surPID"]]
  }
  return(df)
}


makeAvgs = function(df, ratings){
  waves = unique(df[["wave"]])
  for(rating in ratings){
    raterSum = gsub("Rating", "RaterSum", rating)
    rateeSum = gsub("Rating", "RateeSum", rating)
    waveSum = gsub("Rating", "WaveSum", rating)
    raterAvg = gsub("Rating", "RaterAvg", rating)
    rateeAvg = gsub("Rating", "Avg", rating)
    waveAvg = gsub("Rating", "WaveAvg", rating)
    df[c(raterAvg,rateeAvg,waveAvg)] = 0
    names = c("iid","pid","surIID", "surPID", rating, raterSum,rateeSum,waveSum, raterAvg,rateeAvg,waveAvg)
    for(wave in waves){
      print(c(rating,wave))
      waveSlice = df[df["wave"] == wave,][names]
      numIIDs = length(unique(waveSlice[["iid"]]))
      numPIDs = length(unique(waveSlice[["pid"]]))
      for(i in 1:nrow(waveSlice)){
        iid = waveSlice[i,"iid"]
        pid = waveSlice[i,"pid"]
        surPID = waveSlice[i,"surPID"]
        surIID = waveSlice[i,"surIID"]
        surPIDRating = waveSlice[waveSlice["iid"] == iid & waveSlice["pid"] == surPID, rating]
        surIIDRating = waveSlice[waveSlice["iid"] == surIID & waveSlice["pid"] == pid, rating]
        surPIDRatingSum = waveSlice[waveSlice[["pid"]] == surPID, rateeSum][1]
        surIIDRatingSum = waveSlice[waveSlice[["iid"]] == surIID, raterSum][1]
        waveSlice[i,raterAvg] = (waveSlice[i,raterSum] - waveSlice[i,rating] + surPIDRating)/numPIDs
        waveSlice[i,rateeAvg] = (waveSlice[i,rateeSum] - waveSlice[i,rating] + surIIDRating)/numIIDs
        waveSlice[i,waveAvg] = (waveSlice[i,waveSum] - waveSlice[i,raterSum] - waveSlice[i, rateeSum] + surPIDRatingSum + surIIDRatingSum)/nrow(waveSlice)
      }
      df[df["wave"] == wave,][names] = waveSlice[names]
    }
    if(rating == "decRating"){
      df = probColsToLORs(df, c(raterAvg, rateeAvg, waveAvg))
    }
    df[rateeAvg] = df[rateeAvg] - df[waveAvg]
  }
  n = names(df)
  bads = n[grep("WaveAvg|Sum",n)]
  df = df[!(n %in% bads)]
  return(df)
}
