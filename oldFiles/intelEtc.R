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
library(MASS)
library(relaimpo)
merged = read.csv( '~/Desktop/speedDating/mergedCrossFeaturesAdded.csv')

n = names(merged)
toBeLogged = n[grep("dec|Dec|match|Match",n)][c(3,4,6,7,8,9,10,11,12)]
for(name in toBeLogged){
  newName = paste(name,"LOR",sep="")
  merged[[newName]] = ifelse(merged[[name]] == 0, 0.01, merged[[name]])
  merged[[newName]] = ifelse(merged[[newName]] == 1, 0.99, merged[[newName]])
  merged[newName] = log((merged[newName])/(1 - merged[newName]))
}
ratings = n[grep("Rating",n)]
mRatings = ratings[grep("M$", ratings)]
wRatings = ratings[grep("W$", ratings)]
mAvgs = mRatings[9:15]
mGuesses = mRatings[16:24]
wAvgs = wRatings[9:15]
wGuesses = wRatings[16:24]

mSlice = merged[c("decAvgMLOR",mAvgs)]

fitted  = lm(decAvgMLOR ~ ., data = mSlice)

merged["decAvgMLORFit"] = fitted$fitted.values

step <- stepAIC(fitted, direction="both")
calc.relimp(fitted,type=c("lmg","last","first","pratt"),rela=TRUE)

names(merged)


baselineDecM = c("raterDecAvgM", "decAvgW","avgWaveDecM")





evenBetterBooster = function(df, base, tries, tar, numTimes,type){
  totalLogLoss = 0
  votes = hash()
  for(feature in tries){
    votes[[feature]] = 0
  }
  for(i in 1:numTimes){
    print(i)
    idxs = sample(1:nrow(df))
    startIdx = 1
    midIdx = floor(2*(nrow(df)/3))
    trainIdxs = idxs[startIdx:midIdx]
    testIdxs = idxs[(midIdx + 1):nrow(df)]
    train = df[trainIdxs,]
    test = df[testIdxs,]
    baseLogLoss = logLoss(test[[tar]], getProbs(train, test, base,tar,type))
    totalLogLoss = baseLogLoss + totalLogLoss
    for(feature in tries){
      newLogLoss = logLoss(test[[tar]], getProbs(train, test, c(base,feature),tar,type))
      votes[[feature]] = votes[[feature]] + baseLogLoss -  newLogLoss 
    }
  }
  print((totalLogLoss/numTimes))
  for(feature in tries){
    votes[[feature]] = (votes[[feature]]/numTimes)
  }
  return(votes)
}


x = evenBetterBooster(merged,c("calBetterProb"), features, "match", 10, "heuristic")
rfForTrain = randomForest(y=as.factor(merged[,"match"]), x=merged[c("calBetterLORs")], importance=TRUE, ntree=2000)
df = data.frame()

s = sample(seq(100,200,by=0.01), 500)
df[1:500,"yValues"] = 0
df[1:500,"xValues"] = 0
df[["xValues"]] = s
df[["yValues"]] = s^2
rfForTrain = randomForest(y=df[100:200,][["yValues"]], x=df[100:200,]["xValues"], importance=TRUE, ntree=3)

df[200:300,"predictions"] = predict(rfForTrain, df[200:300,])
df[200:300,"shuffled"] = sample(df[200:300,"yValues"])


toBeLogged = n[grep("dec|Dec|match|Match",n)][c(3,4,6,7,8,9,10,11,12)]
for(name in toBeLogged){
  newName = paste(name,"LOR",sep="")
  merged[[newName]] = ifelse(merged[[name]] == 0, 0.01, merged[[name]])
  merged[[newName]] = ifelse(merged[[newName]] == 1, 0.99, merged[[newName]])
  merged[newName] = log((merged[newName])/(1 - merged[newName]))
}
avgRatings[1:8]
rfForTrain = randomForest(y=merged[["decAvgMLOR"]], x=merged[c(avgRatings[1:8], "avgWaveDecW")], importance=TRUE, ntree=2000)
rfForTrain$prediction
rfForTrain$importance
mslice = merged[c("decAvgMLOR",avgRatings[1:8])]
fitted  = lm(decAvgMLOR ~ ., data = mSlice)
fitted  = lm(decAvgMLOR ~ ., data = mSlice)
cor(merged[["decAvgMLOR"]], c(fitted$fitted.values, merged[avgRatings[1:8]]))

mRatings = n[grep("Rating",n)][1:8]
mAvgs = n[grep("Rating",n)][9:16]
mGuesses = n[grep("Rating",n)][17:24]

wRatings = n[grep("Rating",n)][25:32]
wAvgs = n[grep("Rating",n)][33:40]
wGuesses = n[grep("Rating",n)][41:48]

for(i in 1:8){
  mSlice = merged[c(mRatings[i],mAvgs[i],mGuesses[i])]
  wSlice = merged[c(wRatings[i],wAvgs[i],wGuesses[i])]
  mName = paste(mRatings[i],"Est",sep="")
  wName = paste(mRatings[i],"Est",sep="")
  fitted  = lm(y=merged[[mRatings[i]]],x=merged[c(mAvgs[i],mGuesses[i])])
  merged[mName] = fitted$fitted.values
  #   fitted  = lm(wRatings[i] ~ ., data = wSlice)
  #   merged[wName] = fitted$fitted.values
}

wAvgs = n[grep("Rating",n)][9:16]
wGuesses = n[grep("Rating",n)][24:32]
mGuesses = n[grep("Rating",n)][24:32]
mslice = merged[c("decAvgMLOR",avgRatings[1:8])]
fitted  = lm(decAvgMLOR ~ ., data = mSlice)
fitted  = lm(decAvgMLOR ~ ., data = mSlice)
