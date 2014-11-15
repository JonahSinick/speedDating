merged = read.csv('~/Desktop/speedDating/mergedCrossFeaturesAdded.csv')

formTrainAndTest = function(df){
  waves = unique(df[["wave_W"]])
  idxs = sample(waves, length(waves)/2)
  train = df[(df[["wave_W"]] %in% idxs),]
  test = df[!(df[["wave_W"]] %in% idxs),]
  answer = hash()
  answer[["train"]] = train
  answer[["test"]] = test
  return(answer)
}


trainAndTest = formTrainAndTest(merged)
train = trainAndTest[["train"]]
test = trainAndTest[["test"]]
write.csv(train, '~/Desktop/speedDating/currentTrain.csv')
write.csv(test, '~/Desktop/speedDating/currentTest.csv')