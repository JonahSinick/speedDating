merged = read.csv('~/Desktop/speedDating/ratingMetricsAdded.csv')
# 

n = names(merged)
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
colSums(merged[crosses])
badCrosses = crosses[colSums(merged[crosses]) < 100]
merged = merged[!(n %in% badCrosses)]
n = names(merged)

goodCrosses = n[grep("Cross",n)]
crossRatingNames = c("fieldCross", "raceCross", "goOutCross", "dateCross", "careerCross", "goalCross")
h = hash()
h[["careerCross"]] = goodCrosses[grep("career", goodCrosses)]
h[["fieldCross"]] = goodCrosses[grep("field", goodCrosses)]
h[["raceCross"]] = goodCrosses[grep("race", goodCrosses)]
h[["goalCross"]] = goodCrosses[grep("goal", goodCrosses)]
h[["dateCross"]] = goodCrosses[grep("date", goodCrosses)]
h[["goOutCross"]] = goodCrosses[grep("goOut", goodCrosses)]
merged["interDecW"] = (0.5)*(merged["decRaterAvgW"] + merged["decRateeAvgW"])
merged["interDecM"] = (0.5)*(merged["decRaterAvgM"] + merged["decRateeAvgM"])
merged["interMatch"] = (0.5)*(merged["interDecW"] + merged["interDecM"])

n = names(merged)
womenGuesses = n[grep("Guess",n)][1:8]
menGuesses = n[grep("Guess",n)][9:16]
womenWaves = n[grep("Wave",n)][1:8]
menWaves = n[grep("Wave",n)][9:16]
for(i in 1:8){
  merged[gsub("W$", "AdjW", womenGuesses[i])] = merged[womenGuesses[i]] - merged[womenWaves[i]]
  merged[gsub("M$", "AdjM", menGuesses[i])] = merged[menGuesses[i]] - merged[menWaves[i]]
  merged[gsub("W$", "HighWLowMDiff", womenGuesses[i])] = merged[womenGuesses[i]] - merged[menGuesses[i]]
  merged[gsub("W$", "AbsDiff", womenGuesses[i])] = abs(merged[womenGuesses[i]] - merged[menGuesses[i]])
  merged[gsub("W$", "AdjHighWLowMDiff", womenGuesses[i])] = merged[gsub("W$", "AdjW", womenGuesses[i])] - merged[gsub("M$", "AdjM", menGuesses[i])]
  merged[gsub("W$", "AdjAbsDiff", womenGuesses[i])] = abs(merged[gsub("W$", "AdjHighWLowMDiff", womenGuesses[i])])
}
n = names(merged)
menRateeAvgs = n[grep("RateeAvgM$",n)]
menRateeAvgsAdjs = n[grep("RateeAvgAdjM$",n)]
menRaterAvgs = n[grep("RaterAvgM$",n)]
menRaterAvgsAdjs = n[grep("RaterAvgAdjM$",n)]
menGuesses = n[grep("GuessM$",n)]
menAdjGuesses = n[grep("GuessAdjM$",n)]
decPredictors = data.frame()
decPredictors[c("raterAvgs", "raterAvgsAdjs", "rateeAvgs","rateeAvgsAdjs", "guesses","guessesAdjs"),]  = 0
decPredictors[r] = 0
r = gsub("RatingM$", "", n[grep("RatingM$",n)])
decPredictors["guesses",r]  = c(matrix(niceCors(merged, menGuesses, c("decRaterAvgW"))))
decPredictors["raterAvgs",r]  = c(matrix(niceCors(merged, menRaterAvgs, c("decRaterAvgW"))))
decPredictors["rateeAvgs",r]  = c(matrix(niceCors(merged, menRateeAvgs, c("decRaterAvgW"))))
decPredictors["raterAvgsAdjs",r]  = c(matrix(niceCors(merged, menRaterAvgsAdjs, c("decRaterAvgW"))))
decPredictors["rateeAvgsAdjs",r]  = c(matrix(niceCors(merged, menRateeAvgsAdjs, c("decRaterAvgW"))))
decPredictors["guessesAdjs",r[1:8]]  = c(matrix(niceCors(merged, menAdjGuesses, c("decRaterAvgW"))))
decPredictors
df2 = matrix(niceCors(merged, menGuesses, c("decRatingM")))
niceCors(merged, menAvgs, c("decRatingM"))
for(key in keys(h)){
  mRating = paste(key,"DecM",sep="")
  wRating = paste(key,"DecW",sep="")
  matchRating = paste(key,"Match",sep="")
  inters = c("interDecM","interDecM","interMatch")
  merged[c(mRating,wRating,matchRating)] = NA
  for(cross in h[[key]]){
    slice = merged[merged[cross] == 1,][c("iidM", "iidW",  "decRatingW", "decRatingM", "match")]
    for(i in 1:nrow(slice)){
      iidM = slice[i,"iidM"]
      iidW = slice[i,"iidW"]
      subSlice = slice[!(slice["iidM"] == iidM |slice["iidW"] == iidW),]
      menAvg = mean(subSlice[["decRatingM"]])
      womenAvg = mean(subSlice[["decRatingW"]])
      matchAvg = mean(subSlice[["match"]])
      merged[merged["iidM"] == iidM &  merged["iidW"] == iidW, c(mRating,wRating,matchRating)] = c(menAvg,womenAvg,matchAvg)
    }
    print(c(key,cross))
    print(round(100*c(menAvg - 0.48,womenAvg - 0.335 ,matchAvg - 0.15 )))
  }
  slice = merged[is.na(merged[[mRating]]),][c("iidM", "iidW",  "decRatingW", "decRatingM", "match")]    
  for(i in 1:nrow(slice)){
    iidM = slice[i,"iidM"]
    iidW = slice[i,"iidW"]
    subSlice = slice[!(slice["iidM"] == iidM |slice["iidW"] == iidW),]
    menAvg = mean(subSlice[["decRatingM"]])
    womenAvg = mean(subSlice[["decRatingW"]])
    matchAvg = mean(subSlice[["match"]])
    merged[merged["iidM"] == iidM &  merged["iidW"] == iidW, c(mRating,wRating,matchRating)] = c(menAvg,womenAvg,matchAvg)
  }
  merged = probColsToLORs(merged, c(mRating,wRating,matchRating))
}

n = names(merged)
menCrosses = n[grep("CrossDecM", n)]
womenCrosses = n[grep("CrossDecW", n)]
matchCrosses = n[grep("CrossMatch", n)]
niceCors(merged, c(menCrosses), c("decRatingM"))
niceCors(merged, c(womenCrosses), c("decRatingM"))





n = names(merged)
waves = unique(merged[["wave"]])
features = n[grep("Wave|Rater|Ratee|CrossDec|Guess|Diff",n)]
menFeatures = features
womenFeatures = features
menBase = c("decRaterAvgM", "attrRateeAvgAdjM")
womenBase = c("decRaterAvgW", "attrRateeAvgAdjW", "sharRateeAvgAdjW", "likeRateeAvgAdjW", "decRateeAvgW", "decRateeAvgHighWLowMDiff", "decRaterAvgAdjHighWLowMDiff")
menBaseHash = hash()
womenBaseHash = hash()
for(wave in waves){
  menBaseHash[[toString(wave)]] = menBase
  womenBaseHash[[toString(wave)]] = womenBase
}
menHash = hash()
womenHash = hash()
for(wave in waves){  
  train = merged[merged["wave"] != wave,]  
  test = merged[merged["wave"] == wave,]
  men= featureSelector(train, test, menBase, menFeatures, "decRatingM",0.5, 10)  
  womenHash[[toString(wave)]] = featureSelector(train, test, women, womenFeatures, "decRatingW",0.5, 10)  
  menFeatures = 
}


newNames = c()
for(wave in waves){
  print(wave)
  print(length(names(waveHash[[toString(wave)]])))
  newNames = c(newNames, names(waveHash[[toString(wave)]]))
}
unique(newNames[table(newNames) > 7])
newNames = unique(newNames[table(newNames) > 8])

waves = unique(merged[["wave"]])
s2 = waveHash[["2"]][1:75]
waveHash[["4"]][1:75]
waveHash[["7"]][1:75]
waveHash[["9"]]
waveHash[["11"]][1:5]
waveHash[["12"]][1:10]
waveHash[["15"]][1:5]
waveHash[["19"]][1:5]
waveHash[["21"]][1:5]
w7
w9
w11
w12
w15
w
