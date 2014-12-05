source("libraries.R")
df = read.csv('~/Desktop/speedDating/handledBinaries.csv')

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


makeAvgs = function(df,ratings){
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
    df[rateeAvg] = df[rateeAvg] - df[waveAvg]
  }
  n = names(df)
  bads = n[grep("WaveAvg|Guess|RaterAvg|Sum",n)]
  bads = bads[!(bads %in% bads[grep("dec", bads)])]
  df = df[!(n %in% bads)]
  df = probColsToLORs(df, c("decRaterAvg", "decRateeAvg" ))
  return(df)
}

addCombs = function(df){
  df["dec"] = df["decRating"]
  df[c("combRateeAvg", "combRatingExcGuess", "combRatingIncGuess")] = 0
  names = c("decRating", "likeRating", "attrRating", "decRateeAvg", "attrRateeAvg", "likeRateeAvg")
  df[names] = scale(df[names])
  df["combRatingInc"] = (df["decRating"] + df["likeRating"] + df["attrRating"])/3
  df["combRatingExc"] = (df["likeRating"] + df["attrRating"])/2
  df[["combRateeAvg"]] = rowSums(df[c("decRateeAvg", "attrRateeAvg", "likeRateeAvg")])/3
  waves = unique(df[["wave"]])
  for(wave in waves){
    slice = df[df["wave"] == wave,]
    raters = unique(slice[["iid"]])
    ratees = unique(slice[["pid"]])
    slice = slice[order(slice["pid"]),]
    for(i in 1:nrow(slice)){
      rater = slice[i,"iid"]
      ratee = slice[i,"pid"]
      slice[-i,"temp"] = slice[-i,"combRatingInc"]
      slice[i,"temp"] = NA
      ratingMatrix = matrix(slice[["temp"]], nrow = length(raters), ncol = length(ratees))
      r <- as(ratingMatrix, "realRatingMatrix")
      recommender = Recommender(r, method = "UBCF")
      recom <- predict(recommender, r, type="ratings") 
      slice[["temp"]] = c(as(recom, "matrix"))
      slice[i,"combRatingIncGuess"] = slice[i,"temp"]
    }
    for(i in 1:nrow(slice)){
      rater = slice[i,"iid"]
      ratee = slice[i,"pid"]
      slice[-i,"temp"] = slice[-i,"combRatingExc"]
      slice[i,"temp"] = NA
      ratingMatrix = matrix(slice[["temp"]], nrow = length(raters), ncol = length(ratees))
      r <- as(ratingMatrix, "realRatingMatrix")
      recommender = Recommender(r, method = "UBCF")
      recom <- predict(recommender, r, type="ratings") 
      slice[["temp"]] = c(as(recom, "matrix"))
      slice[i,"combRatingExcGuess"] = slice[i,"temp"]
    }
    slice = slice[order(slice["iid"]),] 
    df[df["wave"] == wave,][["combRatingIncGuess"]] = slice[["combRatingIncGuess"]]
    df[df["wave"] == wave,][["combRatingExcGuess"]] = slice[["combRatingExcGuess"]]
  }
  return(df)
}


makeRatingMetrics = function(df){
  n = names(df)
  ratings = n[grep("Rating",n)]
  print('startingSur')
  df = makeSurrogates(df)
  print('startingSums')
  df = makeRatingSums(df, ratings)
  print('startingAvgs')
  df = makeAvgs(df, ratings)
  print('addComb')
  df = addCombs(df)
  return(df)
}

names = c( "attrRating", "likeRating", "decRateeAvg", "likeRateeAvg", "attrRateeAvg", "combRateeAvg", "combRatingInc" , "combRatingExc", "combRatingIncGuess", "combRatingExcGuess")
targets = c("decRating", "combRatingInc", "combRatingExc")
niceCors(slice, names, targets)

men = df[df["gender"] == 1,]
women = df[df["gender"] == 0,]
men = makeRatingMetrics(men)
write.csv(men , '~/Desktop/speedDating/ratingMetricsAddedMen.csv')

women = makeRatingMetrics(women)
write.csv(women , '~/Desktop/speedDating/ratingMetricsAddedWomen.csv')


men = men[, !(names(men) %in% c("gender", "X"))]
women = women[, !(names(women) %in% c("gender", "X"))]
men = men[-1:0]
women = women[-1:0]
colnames(men)= gsub("$", "M", names(men))
colnames(women) = gsub("$", "W", names(women))
x_merges = c("iidW", "idW", "waveW", "partnerW", "pidW", "matchW", "sameRaceW")
y_merges = c("pidM", "partnerM", "waveM", "idM", "iidM", "matchM", "sameRaceM")
merged = merge(women, men, by.x = x_merges, by.y = y_merges)


colnames(merged)[c(1,2,3,4,5,6,7)] = c("iidW", "idW", "wave", "idM", "iidM", "match", "sameRace")

mean(merged[["combRateeAvgM"]])
mean(merged[["combRateeAvgW"]])
n = names(merged)

features = n[grep("imprace|imprelig|happy|expnum|Pref|date$|goOut$|RaterAvgM$|RaterAvgW$|RateeAvgW$|RateeAvgM$",n)]
featuresW = features[grep("W$", features)]
featuresM = features[grep("M$", features)]
niceCors(merged, featuresM, "decRatingM")
niceCors(merged, featuresW, "decRatingW")
colNamesDiffs =  gsub("M", "HighWLowMDiff" ,featuresM)
colNamesAbsDiffs =  gsub("M", "AbsDiff" ,featuresM)
for(i in 1:length(featuresW)){
  merged[colNamesDiffs[i]] = merged[featuresW[i]] - merged[featuresM[i]]
  merged[colNamesAbsDiffs[i]] = abs(merged[featuresW[i]] - merged[featuresM[i]])
}

n = names(merged)
combs = n[grep("comb",n)]
diffs = n[grep("Diff", n)]
niceCors(merged, diffs, c("decRatingM", "decRatingW"))
write.csv(merged , '~/Desktop/speedDating/ratingMetricsAdded.csv')
