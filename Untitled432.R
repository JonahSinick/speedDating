source("libraries.R")
merged = read.csv(merged , '~/Desktop/speedDating/ratingMetricsAdded.csv')

n = names(merged)

merged["interDecW"] = (0.5)*(merged["decRaterAvgW"] + merged["decRateeAvgW"])
merged["interDecM"] = (0.5)*(merged["decRaterAvgM"] + merged["decRateeAvgM"])
merged["interMatch"] = (0.5)*(merged["interDecW"] + merged["interDecM"])


binaries = n[grep("career|goOut|field|date|goal|race", n)]
menBinaries = binaries[grep("M$", binaries)][-8:0]
womenBinaries = binaries[grep("W$", binaries)][-8:0]

createCrosses = function(df,menBinaries,womenBinaries, codeWords){
  for(codeWord in codeWords){
    mens = menBinaries[grep(codeWord,menBinaries)]
    womens = womenBinaries[grep(codeWord,womenBinaries)]
    for(m in mens){
      for(w in womens){
        newName = paste(paste(m,w,sep=""),"Cross",sep="")
        df[newName] = 0 
        df[df[w] == 1 & df[m] == 1,newName] = 1
      }
    }
  }
  return(df)
}

merged = createCrosses(merged,menBinaries,womenBinaries, c("field","goOut","race", "career", "date", "goal"))

n = names(merged)
crosses= n[grep("Cross",n)]
badCrosses = crosses[colSums(merged[crosses]) < 100]
merged = merged[!(n %in% badCrosses)]
goodCrosses = crosses[!(crosses %in% badCrosses)]
crossRatingNames = c("fieldCross", "raceCross", "goOutCross", "dateCross", "careerCross", "goalCross")

h = hash()
h[["careerCross"]] = goodCrosses[grep("career", goodCrosses)]
h[["fieldCross"]] = goodCrosses[grep("field", goodCrosses)]
h[["raceCross"]] = goodCrosses[grep("race", goodCrosses)]
h[["goalCross"]] = goodCrosses[grep("goal", goodCrosses)]
h[["dateCross"]] = goodCrosses[grep("date", goodCrosses)]
h[["goOutCross"]] = goodCrosses[grep("goOut", goodCrosses)]

processCrossKey = function(df,key,crosses){
  mRating = paste(key,"DecM",sep="")
  wRating = paste(key,"DecW",sep="")
  matchRating = paste(key,"Match",sep="")
  df[c(mRating,wRating,matchRating)] = NA
  for(cross in crosses){
    slice = df[df[cross] == 1,][c("iidM", "iidW",  "decRatingW", "decRatingM", "match")]
    for(i in 1:nrow(slice)){
      iidM = slice[i,"iidM"]
      iidW = slice[i,"iidW"]
      subSlice = slice[!(slice["iidM"] == iidM |slice["iidW"] == iidW),]
      menAvg = mean(subSlice[["decRatingM"]])
      womenAvg = mean(subSlice[["decRatingW"]])
      matchAvg = mean(subSlice[["match"]])
      df[df["iidM"] == iidM &  df["iidW"] == iidW, c(mRating,wRating,matchRating)] = c(menAvg,womenAvg,matchAvg)
    }
    print(c(key,cross))
    print(round(100*c(menAvg - 0.48,womenAvg - 0.335 ,matchAvg - 0.15 )))
  }
  slice = df[is.na(df[[mRating]]),][c("iidM", "iidW",  "decRatingW", "decRatingM", "match")]    
  for(i in 1:nrow(slice)){
    iidM = slice[i,"iidM"]
    iidW = slice[i,"iidW"]
    subSlice = slice[!(slice["iidM"] == iidM |slice["iidW"] == iidW),]
    menAvg = mean(subSlice[["decRatingM"]])
    womenAvg = mean(subSlice[["decRatingW"]])
    matchAvg = mean(subSlice[["match"]])
    df[df["iidM"] == iidM &  df["iidW"] == iidW, c(mRating,wRating,matchRating)] = c(menAvg,womenAvg,matchAvg)
  }
  df = probColsToLORs(df, c(mRating,wRating,matchRating))
  
  return(df)
}

for(key in keys(h)){
  merged = processCrossKey(merged, key, crosses)
}


write.csv(merged , '~/Desktop/speedDating/binaryCrossesHandled.csv')
