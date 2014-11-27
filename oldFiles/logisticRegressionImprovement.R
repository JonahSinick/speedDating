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

myTrain = myTrain[,(colSums(myTrain^2) >= 20) & (colSums(myTest^2) >=20)]
myTest = myTest[,(colSums(myTrainTemp^2)) >= 20 & (colSums(myTest^2) >= 20)]

manTraitsDecW = c("attrAvgRatingbyWofM", 
                           "funAvgRatingbyWofM", 
                           "attrRatingGuessbyWofM",
                           "likeRatingGuessbyWofM",
                           "datesManTraitDecW",
                           "decAvgbyWofM", 
                           "probAvgRatingbyWofM", 
                           "fieldsManTraitDecW",
                           "careersManTraitDecW")
womanTraitsDecW = c("raterDecAvgW", 
                "probRatingbyMofW", 
                "datesWomanTraitDecW")

crossTraitsDecW = c("careersCrossTraitDecW", 
                    "datesCrossTraitDecW",
                    "fieldsCrossTraitDecW", 
                    "racesCrossTraitDecW",
                    "raceAsianM_raceWhiteW", 
                    "raceWhiteM_raceWhiteW")


n = names(train)
features = c(manTraitsDecW, womanTraitsDecW, crossTraitsDecW)

tar = "decW"
s=scale(train[features],center=TRUE,scale=TRUE)
tryCosts=c(100)
bestCost=NA
bestLogLoss=1000
bestType=NA
t=0
for(ty in tryTypes){
  for(co in tryCosts){
    s=scale(train[features],center=TRUE,scale=TRUE)
    m=LiblineaR(data=s,labels=train[,tar],type=0,cost=co,bias=TRUE,verbose=FALSE)
    s2= scale(test[features],attr(s,"scaled:center"),attr(s,"scaled:scale"))
    p=predict(m,s2,prob=TRUE)
    probs = p$probabilities[,"1"]
    preds = p$predictions
    bothYes = sum(ifelse(preds == 1 & test[[tar]] == 1, 1, 0))
    bothNo = sum(ifelse(preds == 0 & test[[tar]] == 0, 1, 0))
    falseNeg = sum(ifelse(preds == 0 & test[[tar]] == 1, 1, 0))
    falsePos = sum(ifelse(preds == 1 & test[[tar]] == 0, 1, 0))
    num = nrow(test)
    logLoss = round(logLoss(test[[tar]], probs),3)
    totalErr = (falseNeg + falsePos)/num
    falseNegRate = falseNeg/(falseNeg + bothNo)
    falsePosRate = falsePos/(falsePos + bothYes)
    cat("LogLoss for C=",co," : ",logLoss, "\n",sep="")
    cat("totalErr for C=",co," : ",totalErr,"\n",sep="")
    cat("falseNegRate for C=",co," : ",falseNegRate,"\n",sep="")
    cat("falsePosRate for C=",co," : ",falsePosRate,"\n",sep="")
    if(logLoss < bestLogLoss){
      bestCost=co
      bestLogLoss=logLoss
      bestType=ty
    }
  }
}
cat("Best cost is: ",bestCost,"\n")
cat("Best logLoss is: ",bestLogLoss,"\n")
