
merged = read.csv('~/Desktop/speedDating/mergedCrossFeaturesAdded.csv')
formTrainAndTest = function(df){
  waves = unique(df[["wave"]])
  idxs = sample(waves, length(waves))
  trainIdxs = idxs[1:8]
  testIdxs = idxs[9:16]
  train = df[df[["wave"]] %in% trainIdxs ,]
  test = df[df[["wave"]] %in% testIdxs ,]
  write.csv(train, '~/Desktop/speedDating/currentTrain.csv')
  write.csv(test, '~/Desktop/speedDating/currentTest.csv')

}


formTrainAndTest(merged)
