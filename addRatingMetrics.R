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


makeAvgsAndGuesses = function(df,ratings){
  for(rating in ratings){
    print(rating)
    ratingGuess = gsub("$", "Guess", rating)
    raterSum = gsub("Rating", "RaterSum", rating)
    rateeSum = gsub("Rating", "RateeSum", rating)
    waveSum = gsub("Rating", "WaveSum", rating)
    raterAvg = gsub("Rating", "RaterAvg", rating)
    rateeAvg = gsub("Rating", "RateeAvg", rating)
    waveAvg = gsub("Rating", "WaveAvg", rating)
    df[c(ratingGuess, raterAvg,rateeAvg,waveAvg)] = 0
    names = c("iid","pid","surIID", "surPID","round", rating,ratingGuess, raterSum,rateeSum,waveSum, raterAvg,rateeAvg,waveAvg)
    for(wave in unique(df[["wave"]])){
      print(wave)
      waveSlice = df[df["wave"] == wave,][names]
      numIIDs = length(unique(waveSlice[["iid"]]))
      numPIDs = length(unique(waveSlice[["pid"]]))
      waveSlice = addGuesses(waveSlice, rating)
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
  n = names(df)
  df = df[!(n %in% n[grep("Sum",n)])]
  df[["decRatingGuess"]] = ifelse(df[["decRatingGuess"]] < 0.01, 0.01, df[["decRatingGuess"]])
  return(df)
}

addGuesses = function(df, rating){
  num_ratees = length(unique(df[["pid"]]))
  num_raters = length(unique(df[["iid"]]))
  rating_matrix = t(matrix(df[[rating]], nrow = num_ratees, ncol = num_raters))
  guesses = t(matrix(nrow = num_ratees, ncol = num_raters))
  for(i in 1:num_raters){
    print(c(rating, i))
    for(j in 1:num_ratees){
      temp_matrix = rating_matrix
      temp_matrix[i,j] = NA
      r <- as(temp_matrix, "realRatingMatrix")
      recommender = Recommender(r, method = "UBCF")
      recom <- predict(recommender, r, type="ratings")        
      guesses[i, j] = as(recom, "matrix")[i,j]
    }
  }
  guessName = gsub("$", "Guess", rating)
  df[[guessName]] =  matrix(t(guesses), nrow = num_raters*num_ratees, ncol = 1)[,1]
  df[guessName] = ifelse(df[[guessName]] > 10, 10, df[[guessName]] )    
  df[guessName] = ifelse(df[[guessName]] < 0, 0, df[[guessName]] )
  return(df)
}


makeRatingMetrics = function(df){
  n = names(df)
  ratings = n[grep("Rating",n)]
  ratingsExcDec = ratings[2:9]
  for(rating in ratingsExcDec){
    df[rating] = (df[rating] - mean(df[[rating]]))/sd(df[[rating]])
  }
  print('startingSur')
  df = makeSurrogates(df)
  print('startingSums')
  df = makeRatingSums(df, ratings)
  print('startingAvgsAndGuesses')
  df = makeAvgsAndGuesses(df, ratings)
  return(df)
}


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

n = names(merged)

features = n[grep("imprace|imprelig|happy|expnum|Pref|date$|goOut$|Wave|Rater|Ratee",n)]
featuresW = features[grep("W$", features)]
featuresM = features[grep("M$", features)]
colNamesDiffs =  gsub("M", "HighWLowMDiff" ,featuresM)
colNamesAbsDiffs =  gsub("M", "AbsDiff" ,featuresM)
toBeFixed = n[grep("decRatingGuess|decRateeAvgW|decRaterAvgW|decWaveAvgM|decWaveAvgW|decRaterAvgM|decRateeAvgM",n)]
merged = probsColsToLORS(merged, toBeFixed)
for(i in 1:length(featuresW)){
  merged[colNamesDiffs[i]] = merged[featuresW[i]] - merged[featuresM[i]]
  merged[colNamesAbsDiffs[i]] = abs(merged[featuresW[i]] - merged[featuresM[i]])
}

write.csv(merged , '~/Desktop/speedDating/ratingMetricsAdded.csv')
