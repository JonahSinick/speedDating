merged = read.csv('~/Desktop/speedDating/mergedCrossFeaturesAdded.csv')
n = names(merged)
adjusted_acts = n[grep("ActAdj",n)]
merged = merged[,!(n %in% adjusted_acts)]
n = names(merged)
codes = n[grep("cd_W|cd_M|c_M_|c_W_|_c_M|_c_W",n)]
merged = merged[,!(n %in% codes)]
colnames(merged)[9] = c("match")

formTrainAndTest = function(df){
  waves = unique(df[["wave_W"]])
  idxs = sample(waves, length(waves))
  train_idxs_1 = idxs[1:3]
  train_idxs_2 = idxs[4:6]
  test_idxs_1 = idxs[7:9]
  test_idxs_2 = idxs[10:11]
  cv_idxs_1 = idxs[12:14]
  cv_idxs_2 = idxs[15:16]
  train_1 = df[df[["wave_W"]] %in% train_idxs_1 ,]
  train_2 = df[df[["wave_W"]] %in% train_idxs_2 ,]
  test_1 = df[df[["wave_W"]] %in% test_idxs_1 ,]
  test_2 = df[df[["wave_W"]] %in% test_idxs_2 ,]
  cv_1 = df[df[["wave_W"]] %in% cv_idxs_1 ,]
  cv_2 = df[df[["wave_W"]] %in% cv_idxs_2 ,]
  train = df[df[["wave_W"]] %in% c(train_idxs_1,train_idxs_2)  ,]
  test = df[df[["wave_W"]] %in% c(test_idxs_1,test_idxs_2)  ,]
  cv = df[df[["wave_W"]] %in% c(cv_idxs_1,cv_idxs_2)  ,]
  answer = hash()
  answer[["train_1"]] = train_1
  answer[["train_2"]] = train_2
  answer[["test_1"]] = test_1
  answer[["test_2"]] = test_2
  answer[["cv_1"]] = cv_1
  answer[["cv_2"]] = cv_2
  answer[["train"]] = train
  answer[["test"]] = test
  answer[["cv"]] = cv
  return(answer)
}


trainAndTest = formTrainAndTest(merged)
cross_validation = trainAndTest[["crossValidation"]]
write.csv(trainAndTest[["train_1"]], '~/Desktop/speedDating/currentTrain1.csv')
write.csv(trainAndTest[["test_1"]], '~/Desktop/speedDating/currentTest1.csv')
write.csv(trainAndTest[["train_2"]], '~/Desktop/speedDating/currentTrain2.csv')
write.csv(trainAndTest[["test_2"]], '~/Desktop/speedDating/currentTest2.csv')
write.csv(trainAndTest[["cv_1"]], '~/Desktop/speedDating/currentCV1.csv')
write.csv(trainAndTest[["cv_2"]], '~/Desktop/speedDating/currentCV1.csv')
write.csv(trainAndTest[["train"]], '~/Desktop/speedDating/currentTrain.csv')
write.csv(trainAndTest[["test"]], '~/Desktop/speedDating/currentTest.csv')
write.csv(trainAndTest[["cv"]], '~/Desktop/speedDating/currentCV.csv')