library(gdata)
library(ggplot2)
library(miscTools)
library(xlsx)
library(hash)
library(aod)
library(randomForest)
library(LiblineaR)
library(Matrix)
library(plyr)
library(recommenderlab)
library(scatterplot3d)
library(Metrics)


merged = read.csv( '~/Desktop/speedDating/mergedGuessesAdded.csv')

n = names(merged)
avgRatings = n[grep("AvgRating", n)]
guesses = n[grep("RatingGuess",n)]
crosses = n[grep("Cross",n)]
probs = n[grep("ProbDec",n)]
prefs = n[grep("Pref",n)]
wealthInds = n[grep("WealthInd",n)]
baselineDecM = c("raterDecAvgM", "decAvgW","avgWaveDecM")
baselineDecW = c("raterDecAvgW", "decAvgM","avgWaveDecW")

menTraits = n[grep("M$",n)][c(11:28,30:52,54:59,61:70,73:75,77:96)]
menTraits = menTraits[!(menTraits %in% guesses)]
womenTraits = gsub("M$", "W", menTraits)


probsToLORs = function(probs){
  probs = ifelse(probs == 0, 0.01, probs)
  probs = ifelse(probs == 1, 0.99, probs)
  ORs = probs/(1 - probs)
  return(log(ORs))
}

LORsToProbs = function(LORs){
  ORs = exp(LORs)
  probs = ORs/(1 + ORs)
  return(probs)
}
boostedRFPredictor = function(df, features, tar, numTimes){
  newColName = paste(tar,"PredictedProbs",sep="")
  df[newColName] = 0 
  for(i in 1:numTimes){
    idxs = sample(1:nrow(df))
    startIdx = 1
    midIdx = floor(1*(nrow(df)/2))
    trainIdxs = idxs[startIdx:midIdx]
    testIdxs = idxs[(midIdx + 1):nrow(df)]
    train = df[trainIdxs,]
    test = df[testIdxs,]
    testProbs = getProbs(df[trainIdxs,], df[testIdxs,], features, tar,"heuristic")
    df[testIdxs,][[newColName]] =   df[testIdxs,][[newColName]] + probsToLORs(testProbs)
    trainProbs = getProbs(df[testIdxs,], df[trainIdxs,], features, tar,"heuristic")
    df[trainIdxs,][[newColName]]  = df[trainIdxs,][[newColName]] + probsToLORs(trainProbs)
  }
  df[[newColName]] = LORsToProbs((df[[newColName]]/numTimes))
  printMetrics(df[[tar]],df[[newColName]])
  return(df)
}


idxs = sample(1:nrow(merged))
startIdx = 1
midIdx = floor(1*(nrow(merged)/2))
trainIdxs = idxs[startIdx:midIdx]
testIdxs = idxs[(midIdx + 1):nrow(merged)]
train = merged[trainIdxs,]
test = merged[testIdxs,]

model = lm(sincRatingM~sincRatingGuessM + sincAvgRatingM, data=train)

test["sincRatingGuessNewM"] = predict(model,test)
cor(test[c("sincRatingM","sincAvgRatingM", "sincRatingGuessM", "sincRatingGuessNewM")])
features = c(menTraits, womenTraits, crosses)


model = lm(intelRatingM~intelRatingGuessM + intelAvgRatingM, data=train)

test["intelRatingGuessNewM"] = predict(model,test)
cor(test[c("intelRatingM","intelAvgRatingM", "intelRatingGuessM", "intelRatingGuessNewM")])

model = lm(probRatingW~attrRatingM + probAvgRatingW, data=train)

test["probRatingGuessNewW"] = predict(model,test)
cor(test[c("probRatingW","probAvgRatingW", "probRatingGuessW", "probRatingGuessNewW")])

model = lm(funRatingM~funRatingGuessM + funAvgRatingM, data=train)

test["funRatingGuessNewM"] = predict(model,test)
cor(test[c("funRatingM","funAvgRatingM", "funRatingGuessM", "funRatingGuessNewM")])


rfForTrain = randomForest(raterDecAvgW ~., importance=TRUE, ntree=2000, data = train)
rfForTest = randomForest(decAvgM ~., importance=TRUE, ntree=2000, data = slice2)

predictedStuff = predict(rfForTrain, test)
test["raterDecAvgMGuess"] = predictedStuff
cor(test[c("raterDecAvgM", "raterDecAvgMGuess")])
mean(abs(test[["raterDecAvgM"]] - test[["raterDecAvgMGuess"]]))
wave17 = merged[merged["wave"] == 17,]

rating_matrix = t(matrix(wave17[["attrRatingM"]], nrow = 14, ncol = 10))
r <- as(rating_matrix, "realRatingMatrix")
r_m <- normalize(r)
image(r_m, main = "Raw Ratings")
slice = wave17[(wave17[["idW"]] %in% c(4,6)) & (wave17[["idM"]] %in% c(12,13,14)),]
slice = as.data.frame(t(slice))
round(slice)
row2 = wave17[wave17["idW"] == 11 & wave17["idM"] == 2 ,]
row9 = wave17[wave17["idW"] == 11 & wave17["idM"] == 9 ,]

image(r_m, main = "Normalized Ratings")
m = round(100*cor(rating_matrix))
corMatrix  = as(m, "realRatingMatrix")
corMatrix <- normalize(corMatrix)

image(corMatrix, main = "Rating Correlations")


  num_ratees = length(unique(df[["pid"]]))
  num_raters = length(unique(df[["iid"]]))

first = as.data.frame(t(row1))
second = as.data.frame(t(row2))
round(cbind(second,first),3)
