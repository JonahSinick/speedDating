makeRatingSums = function(df, rating){
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
  return(df)
}

makeSurrogates = function(df){
  df[c("surIID", "surPID")] = 0
  waves = unique(df[["wave"]])
  for(wave in waves){
    slice = df[df["wave"] == wave,]
    iids = unique(slice[["iid"]])
    pids = unique(slice[["pid"]])
    for(i in 1:nrow(slice)){
      eligibleIIDs = iids[iids != slice[i,"iid"]]
      eligiblePIDs = pids[pids != slice[i,"pid"]]
      set.seed(i)
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


makeAvgs = function(df, rating){
  waves = unique(df[["wave"]])
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
  n = names(df)
  if(rating == "decRating"){
    df = probColsToLORs(df, c(raterAvg, rateeAvg, waveAvg))
  }
  else{
    df = df[!(n == raterAvg)]
  }
  df[rateeAvg] = df[rateeAvg] - df[waveAvg]
  bads = n[grep("WaveAvg|Sum",n)]
  df = df[!(n %in% bads)]
  return(df)
}

makeAvgCor = function(df, rating){
  corStr = paste(rating,"Cor",sep="")
  df[corStr] = 0
  avg = gsub("Rating", "Avg", rating)
  for(i in 1:nrow(df)){
    print(c(i,rating))
    iid = df[i,"iid"]
    pid = df[i,"pid"]
    surPID = df[i,"surPID"]
    slice = df[df["iid"] == iid & df["pid"] != pid,]
    avgs = slice[[avg]]
    decs = slice[["dec"]]
    newAvg = df[df["iid"] == iid & df["pid"] == surPID,avg]
    newDec = df[df["iid"] == iid & df["pid"] == surPID,"dec"]
    avgs = c(avgs,newAvg)
    decs = c(decs, newDec)
    c = cor(avgs, decs)  
    df[i,corStr] = ifelse(is.na(c), 0, c)
  }
  corSDStr = paste(rating,"CorSD",sep="")
  
  for(i in 1:nrow(df)){
    df[i,corSDStr] = 1 - length(df[df[corStr] > df[i,corStr],"iid"])/nrow(df)
    print(df[i,corSDStr])
  }
  df[corSDStr] = scale(d[corSDStr])
  
  
  return(df)
}





makeRatingMetrics = function(df){
  n = names(df)
  ratings = n[grep("Rating$",n)]
  df = makeSurrogates(df)
  for(rating in ratings){
    df = makeRatingSums(df, rating)
    df = makeAvgs(df, rating)
    if(!(rating %in% c("decRating", "likeRating"))){
      df = makeAvgCor(df, rating)      
    }
  }
  return(df)
}
