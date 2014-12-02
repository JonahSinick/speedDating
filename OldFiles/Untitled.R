for(wave in waves){
  slice = merged[merged["wave"] == wave,]
  for(i in 1:4){
    olderOrderSlice = slice[slice[["oldOrderQuart"]] %in% 1:i,]
    betterOrderSlice = slice[slice[["betterOrderQuart"]] %in% 1:i,]
    scoreFrame[toString(wave), paste("baseNum",toString(i),sep="_")] = sum(olderOrderSlice[["match"]])
    scoreFrame[toString(wave), paste("betterNum",toString(i),sep="_")] = sum(betterOrderSlice[["match"]])
    scoreFrame[toString(wave), paste("baseNumM",toString(i),sep="_")] = length(unique(olderOrderSlice[["iidM"]]))
    scoreFrame[toString(wave), paste("betterNumM",toString(i),sep="_")] = length(unique(betterOrderSlice[["iidM"]]))
    scoreFrame[toString(wave), paste("baseNumW",toString(i),sep="_")] = length(unique(olderOrderSlice[["iidW"]]))
    scoreFrame[toString(wave), paste("betterNumW",toString(i),sep="_")] = length(unique(betterOrderSlice[["iidW"]]))
  }
}