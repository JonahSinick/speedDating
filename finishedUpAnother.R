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

myTrain = read.csv('~/Desktop/speedDating/currentTrain.csv')
myTest = read.csv('~/Desktop/speedDating/currentTest.csv')
myCV = read.csv('~/Desktop/speedDating/currentCV.csv')


myTrainTemp = myTrain
myTestTemp = myTest

myTrain = myTrain[,colSums(myTrain^2) != 0 & colSums(myTest^2) != 0 & colSums(myCV^2) != 0]
myTest = myTest[,colSums(myTrainTemp^2) != 0 & colSums(myTest^2) != 0 & colSums(myCV^2) != 0]
myCV = myCV[,colSums(myTrainTemp^2) != 0 & colSums(myTestTemp^2) != 0 & colSums(myCV^2) != 0]
getProbs = function(train, test, features, tar){
  s=scale(train[features],center=TRUE,scale=TRUE)
  co=heuristicC(s)
  m=LiblineaR(data=s,labels=factor(train[,tar]),type=0,cost=co,bias=TRUE,verbose=FALSE)
  s2= scale(test[features],attr(s,"scaled:center"),attr(s,"scaled:scale"))
  p=predict(m,s2,prob=TRUE)
  probs = p$probabilities[,"1"]
  return(probs)
}



addProbColsGender = function(genderHash){
  train = genderHash[["train"]]
  test = genderHash[["test"]]
  tars = c("dec_M","dec_W")
  colNames = genderHash[["colNames"]]
  menFeatures = genderHash[["featuresMen"]]
  womenFeatures = genderHash[["featuresWomen"]]
  train[[colNames[1]]]  = getProbs(train, train, menFeatures, tars[1])
  test[[colNames[1]]]  = getProbs(train, test, menFeatures, tars[1])
  train[[colNames[2]]]  = getProbs(train, train, womenFeatures, tars[2])
  test[[colNames[2]]]  = getProbs(train, test, womenFeatures, tars[2])
  genderHash[["train"]] = train
  genderHash[["test"]] = test
  return(genderHash)
}



Names = names(myTrain)
guessNames = Names[grep("Guess_", Names)]
avgNames = Names[grep("Avg", Names)]
avgNames = c(avgNames[2:8],avgNames[12:18])
crossNames = Names[grep("Cross", Names)]
menNames = Names[grep("W_of_M|_M$", Names)]
womenNames = Names[grep("M_of_W|_W$", Names)]

menDecAvgs = c("decAvg_M_of_W", "raterDecAvg_M") 
womenDecAvgs = c("decAvg_W_of_M", "raterDecAvg_W") 

genderHash = hash()
genderHash[["train"]] = myTrain
genderHash[["test"]] = myTest


# 

for(i in 1:7){
  string = paste("mergedRatingWofM",toString(i),sep="_")
  stringM = paste(string,"decM",sep="_")
  stringW = paste(string,"decW",sep="_")
  genderHash[["colNames"]] = c(stringM, stringW)
  genderHash[["featuresMen"]] = c(guessNames[i], avgNames[i])
  genderHash[["featuresWomen"]] = c(guessNames[i], avgNames[i])
  genderHash = addProbColsGender(genderHash)
}

for(i in 7:14){
  string = paste("mergedRatingMofW",toString(i),sep="_")
  stringM = paste(string,"decM",sep="_")
  stringW = paste(string,"decW",sep="_")
  genderHash[["colNames"]] = c(stringM, stringW)
  genderHash[["featuresMen"]] = c(guessNames[i], avgNames[i])
  genderHash[["featuresWomen"]] = c(guessNames[i], avgNames[i])
  genderHash = addProbColsGender(genderHash)
}



myTrain = genderHash[["train"]]
myTest = genderHash[["test"]]


h = function(tar,probs){
  tab = table(ifelse(probs > 0.5, 1, 0),tar)
  logloss = round(logLoss(tar, probs),2)
  print(c("logLoss:", logloss))
  totalError = round((tab[2,1] + tab[1,2])/length(tar), 2)
  print(c("totalError:", totalError))
  fracYesFound = round(tab[2,2]/(tab[1,2] + tab[2,2]), 2)
  print(c("fracYesFound", fracYesFound))
  return(tab)
}
n = names(myTrain)
mergedRatings = n[grep("mergedRatingWofM",n)]
mergedRatings = mergedRatings[grep("decW",mergedRatings)]

for(i in 1:7){
  print("NEXT ROUND")
  features1 = c("decAvg_W_of_M", "raterDecAvg_W", avgNames[i + 7])
  features2 = c("decAvg_W_of_M", "raterDecAvg_W", guessNames[i + 7])
  features3 = c("decAvg_W_of_M", "raterDecAvg_W", mergedRatings[i])
  print(avgNames[i])
  probs = getProbs(myTrain, myTest, features1, "dec_W")
  h(myTest[["dec_W"]], probs)
  print(guessNames[i])
  probs = getProbs(myTrain, myTest, features2, "dec_W")
  h(myTest[["dec_W"]], probs)
  print(mergedRatings[i])
  probs = getProbs(myTrain, myTest, features3, "dec_W")
  h(myTest[["dec_W"]], probs)
}
features = c("decAvg_W_of_M", "raterDecAvg_W", "mergedRatingWofM_1_decW")

