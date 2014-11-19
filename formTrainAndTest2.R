
merged = read.csv('~/Desktop/speedDating/mergedCrossFeaturesAdded.csv')
formTrainAndTest = function(df){
  idxs = sample(nrow(df))
  train = df[idxs[1:nrow(df)/2],]
  test = df[idxs[(nrow(df)/2):nrow(df)],]
  write.csv(train, '~/Desktop/speedDating/currentTrain.csv')
  write.csv(test, '~/Desktop/speedDating/currentTest.csv')

}


trainAndTest = formTrainAndTest(merged)
