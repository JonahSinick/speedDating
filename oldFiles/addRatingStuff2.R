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
library(arules)

df = read.csv('~/Desktop/speedDating/cleanedData.csv')

n = names(df)
ratings = n[grep("Rating",n)]
ratingsExcDec = ratings[2:9]
menSlice = df[df["gender"] == 1,]
womenSlice = df[df["gender"] == 0,]
for(rating in ratingsExcDec){
  df[df["gender"] == 1,][rating] = (menSlice[rating] - mean(menSlice[[rating]]))/sd(menSlice[[rating]])
  df[df["gender"] == 0,][rating] = (womenSlice[rating] - mean(womenSlice[[rating]]))/sd(womenSlice[[rating]])
}
for(rating in ratings){
  ratingStub = gsub("Rating$", "", rating)
  raterSum = paste(ratingStub,"RaterSum",sep="")
  rateeSum = paste(ratingStub,"RateeSum",sep="")
  waveSumMen = paste(ratingStub,"WaveSumMen",sep="")
  waveSumWomen = paste(ratingStub,"WaveSumWomen",sep="")
  df[c(raterSum,rateeSum, waveSumMen,waveSumWomen)] = 0
  for(iid in unique(df[["iid"]])){
    print(iid)
    wave = df[df["iid"] == iid,"wave"][1]
    gender = df[df["iid"] == iid,"gender"][1]
    s = sum(df[df["iid"] == iid,rating])
    df[df["iid"] == iid,raterSum] = s
    waveSumName = ifelse(gender == 1,waveSumMen, waveSumWomen )
    df[df["wave"] == wave,waveSumName] = df[df["wave"] == wave,waveSumName] + s  
  }
  for(pid in unique(df[["pid"]])){
    s = sum(df[df["pid"] == pid,rating])
    df[df["pid"] == pid,rateeSum] = s
  }
}

df[c("surIID", "surPID")] = 0

for(wave in unique(df[["wave"]])){
  slice = df[df["wave"] == wave,]
  menSlice = slice[slice["gender"] == 1,]
  womenSlice = slice[slice["gender"] == 0,]
  iidMs = unique(menSlice[["iid"]])
  iidWs = unique(womenSlice[["iid"]])
  for(i in 1:nrow(slice)){
    if(slice[i,"gender"] == 1){
      eligibleIIDs = iidMs[iidMs != slice[i,"iid"]]
      eligiblePIDs = iidWs[iidWs != slice[i,"pid"]]
    }
    else{
      eligibleIIDs = iidWs[iidWs != slice[i,"iid"]]
      eligiblePIDs = iidMs[iidMs != slice[i,"pid"]]
    }
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
  waveSumMen = gsub("Rating", "WaveSumMen", rating)
  waveSumWomen = gsub("Rating", "WaveSumWomen", rating)
  raterAvg = gsub("Rating", "RaterAvg", rating)
  rateeAvg = gsub("Rating", "RateeAvg", rating)
  waveAvgMen = gsub("Rating", "WaveAvgMen", rating)
  waveAvgWomen = gsub("Rating", "WaveAvgWomen", rating)
  df[c(raterAvg,rateeAvg,waveAvgMen, waveAvgWomen)] = 0
  names = c("iid","pid","gender", "surIID", "surPID","round", rating, raterSum,rateeSum,waveSumMen,waveSumWomen, raterAvg,rateeAvg,waveAvgMen, waveAvgWomen)
  for(wave in unique(df[["wave"]])){
    print(wave)
    waveSlice = df[df["wave"] == wave,][names]
    for(i in 1:nrow(waveSlice)){
      iid = waveSlice[i,"iid"]
      pid = waveSlice[i,"pid"]
      gender = waveSlice[i,"gender"]
      numIIDs = length(unique(waveSlice[waveSlice["gender"] == gender,][["iid"]]))
      numPIDs = length(unique(waveSlice[waveSlice["gender"] == gender,][["pid"]]))
      surPID = waveSlice[i,"surPID"]
      surIID = waveSlice[i,"surIID"]
      surPIDRating = waveSlice[waveSlice["iid"] == iid & waveSlice["pid"] == surPID, rating]
      surIIDRating = waveSlice[waveSlice["iid"] == surIID & waveSlice["pid"] == pid, rating]
      surPIDRatingSum = waveSlice[waveSlice[["pid"]] == surPID, rateeSum][1]
      surIIDRatingSum = waveSlice[waveSlice[["iid"]] == surIID, raterSum][1]
      waveSlice[i,raterAvg] = (waveSlice[i,raterSum] - waveSlice[i,rating] + surPIDRating)/numPIDs
      waveSlice[i,rateeAvg] = (waveSlice[i,rateeSum] - waveSlice[i,rating] + surIIDRating)/numIIDs
      if(waveSlice[i,"gender"] == 1){
        waveSlice[i,waveAvgMen] = (waveSlice[i,waveSumMen] - waveSlice[i,raterSum] - waveSlice[i, rateeSum] + surPIDRatingSum + surIIDRatingSum)/nrow(waveSlice)
      }
      else{
        waveSlice[i,waveAvgWomen] = (waveSlice[i,waveSumWomen] - waveSlice[i,raterSum] - waveSlice[i, rateeSum] + surPIDRatingSum + surIIDRatingSum)/nrow(waveSlice)    
      }
    }
    df[df["wave"] == wave,][names] = waveSlice[names]
  }
}


