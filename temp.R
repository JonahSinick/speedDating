n = names(df)
ratings = n[grep("Rating",n)]
df = df[df["gender"] == 1,]

makeRatingMetrics = function(df){
	ratingsExcDec = ratings[2:9]


	for(rating in ratingsExcDec){
	  df[rating] = (df[rating] - mean(df[[rating]]))/sd(df[[rating]])
	}
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


	n = names(df)
	ratings =  n[grep("Rating",n)]

	for(rating in ratings){
	  print(rating)
	  raterSum = gsub("Rating", "RaterSum", rating)
	  rateeSum = gsub("Rating", "RateeSum", rating)
	  waveSum = gsub("Rating", "WaveSum", rating)
	  raterAvg = gsub("Rating", "RaterAvg", rating)
	  rateeAvg = gsub("Rating", "RateeAvg", rating)
	  waveAvg = gsub("Rating", "WaveAvg", rating)
	  df[c(raterAvg,rateeAvg,waveAvg)] = 0
	  names = c("iid","pid","surIID", "surPID","round", rating, raterSum,rateeSum,waveSum, raterAvg,rateeAvg,waveAvg)
	  for(wave in unique(df[["wave"]])){
	    print(wave)
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
	  df[paste(raterAvg,"Adj", sep="")] = df[raterAvg] - df[waveAvg]
	  df[paste(rateeAvg,"Adj", sep="")] = df[rateeAvg] - df[waveAvg]
	}	
	return(df)
}
