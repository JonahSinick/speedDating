womenKeys = keys(waveHash)[grep("W$",keys(waveHash))]
for(key in womenKeys){
  print(key)
  cat("\n")
  print(waveHash[[key]])
  cat("\n")
}
"11M" "11W" "12M" "12W" "15W" "19W" "21W" "2M"  "2W"  "4W"  "7M"  "7W"  "9M"  "9W"

womenBase = c("decRaterAvgW", "attrRateeAvgAdjW", "sharRateeAvgAdjW", "likeRateeAvgAdjW", "decRateeAvgW", 
              "decRateeAvgHighWLowMDiff", "decRaterAvgAdjHighWLowMDiff")

merged["decWPrediction"] = 0
for(i in 1:length(womenBase)){
  for(wave in unique(merged[["wave"]])){
    train = merged[merged["wave"] != wave,]  
    test = merged[merged["wave"] == wave,]  
    merged[merged["wave"] == wave,][["decWPrediction"]] = getProbs(train, test, womenBase[1:i], "decRatingW")  
  }
  print(womenBase[1:i])
  printMetrics(merged[["decRatingW"]], merged[["decWPrediction"]])
}