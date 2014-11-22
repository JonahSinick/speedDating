

for(i in 1:2){
  tar = targets[i]
  RFsuffix = RFsuffixes[i]
  LinSuffix = linearSuffixes[i]
  comboSuffix = comboSuffixes[i]
  for(nameIdx in 1:length(names)){
    train = h[["train"]]
    print(nameIdx)
    print(names[nameIdx])
    menName = menNames[nameIdx]
    womenName = womenNames[nameIdx]
    features = c(menName, womenName)
    newColNameRF = paste(names[nameIdx],RFsuffix,sep="")
    newColNameLin = paste(names[nameIdx],LinSuffix,sep="")
    newColNameCombo = paste(names[nameIdx],comboSuffix,sep="")
    print(features)
    rf_fit <- randomForest(y=as.factor(train[,tar]), x=train[features], importance=TRUE, ntree=200, m_try = 2)
    print("PastForest")
    for(key in keys(h)){
      df = h[[key]]
      df[[newColNameLin]] = getProbs(train, df, features, tar, tryCosts)
      predictions = predict(rf_fit, df, type="prob")
      df[[newColNameRF]] = predictions[,"1"]
      h[[key]] = df
    }
  }
}


RFsuffixes = c("RFDecM", "RFDecW")
linearSuffixes = c("LinDecM", "LinDecW")
comboSuffixes = c("ScoreDecM", "ScoreDecW")
guessScores = c("guessRatingM", "guessRatingW")
avgScores = c("avgRatingM", "avgRatingW")
targets = c("decM", "decW")
for(i in 1:2){
  tar = targets[i]
  RFsuffix = RFsuffixes[i]
  LinSuffix = linearSuffixes[i]
  comboSuffix = comboSuffixes[i]
  for(nameIdx in c(1:40,72:73)){
    print(names[nameIdx])
    menName = menNames[nameIdx]
    womenName = womenNames[nameIdx]
    features = c(menName, womenName)
    print(features)
    newColNameRF = paste(names[nameIdx],RFsuffix,sep="")
    newColNameLin = paste(names[nameIdx],LinSuffix,sep="")
    newColNameCombo = paste(names[nameIdx],comboSuffix,sep="")
    for(key in keys(h)){
      df = h[[key]]
      df[[newColNameCombo]] = getProbs(h[["futureTrain"]], df, c(features, newColNameRF), tar, tryCosts)
      h[[key]] = df
    }
  }
}
train = h[["train"]]
futureTrain = h[["futureTrain"]]
futureTest = h[["futureTest"]]
n = names(train)
mens = n[grep("DecM",n)]
menScores = mens[grep("Score",mens)]
menLins = mens[grep("Lin",mens)][c(1:40,72:73)]
womens = n[grep("DecW",n)]
womenScores = womens[grep("Score",womens)]
womenLins = womens[grep("Lin",womens)][c(1:40,72:73)]
round(100*cor(futureTrain[c(menScores,lins)], futureTrain["decM"]))
for(i in 1:length(menScores)){
  logLoss1 = round(logLoss(futureTest[["decM"]],futureTest[[menScores[i]]]),3)
  logLoss2 = round(logLoss(futureTest[["decM"]],futureTest[[menLins[i]]]),3)
  cat(menScores[i],": ",logLoss1," ", menLins[i],": ", logLoss2,"\n",sep="")
}
tryCosts = 10^(seq(-4,4,by=0.05))
baseFeatures = c( "raterDecAvgM", "avgWaveDecM", "decAvgbyMofW")
features = c(baseFeatures, "avgRatingLinDecM", "impraceScoreDecM", "funRatingGuessbyWofScoreDecM",
             "artArtActScoreDecM", "museumsArtActScoreDecM", "goOutScoreDecM")
futureTest[["decMGuess"]] = getProbs(futureTrain, futureTest, features, "decM", tryCosts)
futureTest[["decMGuessSolid"]] = ifelse(futureTest[["decMGuess"]] > 0.5, 1, 0)


round(logLoss(futureTest[["decM"]],futureTest[["decMGuess"]]),3)
t = table(futureTest[["decMGuessSolid"]], futureTest[["decM"]])
(t[1,2] + t[2,1])/(nrow(futureTest))