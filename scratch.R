source("libraries.R")
source("machineLearning2.R")

merged = read.csv('~/Desktop/speedDating/binaryCrossesHandled.csv')
merged[["combRateeAvgM"]] = rowSums(scale(merged[c("decRateeAvgM", "attrRateeAvgM", "likeRateeAvgM")]))/3
merged[["combRateeAvgW"]] = rowSums(scale(merged[c("decRateeAvgW", "attrRateeAvgW", "likeRateeAvgW")]))/3
n = names(merged)
features = n[grep("Diff|Cross|comb|dec",n)]
features = features[-(c(1:4,10:13))]


mBase = c("combRateeAvgM", "decRaterAvgM")
merged["combWHighMLowDiff"] = rowSums(scale(merged[c("decRaterAvgHighWLowMDiff", "attrRateeAvgHighWLowMDiff", "likeRateeAvgHighWLowMDiff")]))

wBase = c("decRaterAvgW", "combRateeAvgW", "combWHighMLowDiff")
womenHash = hash()

waves =unique(merged[["wave"]])
for(wave in waves){  
  print(c(wave))
  train = merged[merged["wave"] != wave,]  
  test = merged[merged["wave"] == wave,] 
  womenHash[[toString(wave)]]  = featureSelector(train, test, wBase, c(possibilities), "decRatingW", 0.5, 10, thres= 0 )    
}
oks = c()
for(key in keys(womenHash)){
  print(key)
  print(womenHash[[key]][1:10])
  oks = c(oks, names(womenHash[[key]]))
}
possibilities = unique(oks[table(oks)  > 8])
newOKs = unique(names(oks))
length(unique(names(oks[table(oks) > 8])))
okFeatures = unique(oks)[table(oks) > 8]

unique(names(oks[(oks > 8) == TRUE]))
unique(okFeatures) == 
for(key in keys(womenHash)){
  oks = c(oks, names(womenHash[[key]]))
}
unique(oks[table(oks) > 7])
for(i in 1:1){
  for(wave in waves){  
    print(c(i,wave))
    train = merged[merged["wave"] != wave,]  
    test = merged[merged["wave"] == wave,]
    menFeatures = featuresSelector(train, test, menBaseHash[[toString(wave)]], oks, "decRatingM", 1, 1, 1)
    menHash[[paste(toString(wave),toString(i),sep="_")]] = menFeatures
    menFeatures = names(menFeatures)
    oks = unique(c(oks, menFeatures[1]))
    menBaseHash[[toString(wave)]] = c(menBaseHash[[toString(wave)]], menFeatures[1])
  }
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
slice = merged[merged["wave"] == 4,]
scale(slice[c("attrRatingGuessW", "likeRatingGuessW", "funRatingGuessW")])
scale(merged["attr"])
for(w in waves){
  print(niceCors(merged[merged["wave"] == w,], c("decRatingM"), c("attrRateeAvgAdjM", "likeRateeAvgAdjM", "decRateeAvgAdjM", "combRateeAvgAdjM")))
}

for(w in waves){
  print(niceCors(merged[merged["wave"] == w,], c("decRatingW"), c("attrRateeAvgAdjW", "likeRateeAvgAdjW", "decRateeAvgAdjW", "combRateeAvgAdjW")))
}


for(w in waves){
  #   print(niceCors(merged[merged["wave"] == w,], c("decRatingM"), c("likeRatingGuessM", "attrRatingGuessM", "funRatingGuessM")))
  print(niceCors(merged[merged["wave"] == w,], c("decRatingW"), c("likeRateeAvgAdjW", "attrRateeAvgAdjW", "funRateeAvgAdjW", "attrRatingGuessW")))  
}

print(niceCors(merged[merged[["wave"]] %in% waves ,], c("decRatingW"), c("attrRateeAvgAdjW", "likeRateeAvgAdjW", "combRateeAvgAdjW")))
print(niceCors(merged[merged[["wave"]] %in% waves ,], c("decRatingM"), c("attrRateeAvgAdjM", "likeRateeAvgAdjM", "combRateeAvgAdjM")))


merged[["normedDecRateeAvgAdjM"]] = (merged[["decRateeAvgAdjM"]] - mean(merged[["decRateeAvgAdjM"]]))/sd(merged[["decRateeAvgAdjM"]])
merged[["normedDecRateeAvgAdjW"]] = (merged[["decRateeAvgAdjW"]] - mean(merged[["decRateeAvgAdjW"]]))/sd(merged[["decRateeAvgAdjW"]])

merged[["normedDecRatingGuessW"]] = (merged[["decRatingGuessW"]] - mean(merged[["decRatingGuessW"]]))/sd(merged[["decRatingGuessW"]])
merged[["normedDecRatingGuessW"]] = (merged[["decRatingGuessW"]] - mean(merged[["decRatingGuessW"]]))/sd(merged[["decRatingGuessW"]])
merged["combRateeAvgAdjW"] = rowSums(merged[c("normedDecRateeAvgAdjW", "attrRateeAvgAdjW", "likeRateeAvgAdjW")])
merged["combRateeAvgAdjM"] = rowSums(merged[c("normedDecRateeAvgAdjM", "attrRateeAvgAdjM", "likeRateeAvgAdjM")])


niceCors(merged, c("decRatingW"), c("attrRateeAvgAdjW", "likeRateeAvgAdjW", "decRateeAvgAdjW", "combRateeAvgAdjW"))
niceCors(merged, c("decRatingM"), c("attrRateeAvgAdjM", "likeRateeAvgAdjM", "decRateeAvgAdjM", "combRateeAvgAdjM"))
n = names(merged)

bads = c(n[grep("WaveAvg|RaterAvgAdjHighW|RaterAvgAdjAbsDiff|RaterAvgAbsDiff|RaterAvgHighW", n)])
slice = merged
slice = slice[!(n %in% bads)]
slice[grep()]