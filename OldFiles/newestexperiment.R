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

train = read.csv('~/Desktop/speedDating/currentTrain.csv')
test = read.csv('~/Desktop/speedDating/currentTest.csv')


# Center and scale data
s=scale(xTrain,center=TRUE,scale=TRUE)

# Logistic Regression
t=0



getProbs = function(train, test, features, tar){
  s=scale(train[features],center=TRUE,scale=TRUE)
  co=heuristicC(s)
  m=LiblineaR(data=s,labels=factor(train[,tar]),type=0,cost=co,bias=TRUE,verbose=FALSE)
  s2= scale(test[features],attr(s,"scaled:center"),attr(s,"scaled:scale"))
  p=predict(m,s2,prob=TRUE)
  probs = p$probabilities[,"1"]
  return(probs)
}

womanTraitsDecManNames = c("decAvgbyMofW", "attrAvgRatingbyMofW", "likeRatingGuessbyMofW", 
                           "raceAsianM_raceWhiteW", "careersWomanTraitDecM", "fieldsWomanTraitDecM",
                           "goalsWomanTraitDecM", "racesWomanTraitDecM")
makeErrorFrame = function(scoreFrame, tar){
  scoreNames = names(scoreFrame)
  errorFrame = scoreFrame[c(1),c()]
  errorFrame["logLoss"] = 0
  errorFrame["totalError"] = 0
  errorFrame["positivesFound"] = 0
  tar = scoreFrame[[tar]]
  for(name in scoreNames){
    probs = scoreFrame[[name]]
    errorFrame[name,] = 0
    errorFrame[name,"logLoss"] = logloss = round(logLoss(tar, probs),3)
    tab = table(ifelse(probs > 0.5, 1, 0),tar)
    print(name)
    print(tab)
    errorFrame[name, "totalError"] = round((tab[1,2] + tab[2,1])/length(tar), 3)
    errorFrame[name,"positivesFound"] = round(tab[2,2]/(tab[1,2] + tab[2,2]), 3)
    errorFrame[name, "guessNoNo"] = tab[1,1]
    errorFrame[name, "guessNoYes"] = tab[1,2]
    errorFrame[name, "totalNum"] = length(tar)
  }
  return(errorFrame)
}



# Find the best model with the best cost parameter via 10-fold cross-validations
tryTypes=c(0:7)
tryCosts=c(1000,1,0.001)
bestCost=NA
bestAcc=0
bestType=NA

for(ty in tryTypes){
  for(co in tryCosts){
    acc=LiblineaR(data=s,labels=yTrain,type=ty,cost=co,bias=TRUE,cross=5,verbose=FALSE)
    cat("Results for C=",co," : ",acc," accuracy.\n",sep="")
    if(acc>bestAcc){
      bestCost=co
      bestAcc=acc
      bestType=ty
    }
  }
}

cat("Best model type is:",bestType,"\n")
cat("Best cost is:",bestCost,"\n")
cat("Best accuracy is:",bestAcc,"\n")

# Re-train best model with best cost value.
m=LiblineaR(data=s,labels=yTrain,type=bestType,cost=bestCost,bias=TRUE,verbose=FALSE)

# Scale the test data
s2=scale(xTest,attr(s,"scaled:center"),attr(s,"scaled:scale"))

# Make prediction
pr=FALSE
if(bestType==0 | bestType==7) pr=TRUE

p=predict(m,s2,proba=pr,decisionValues=TRUE)

# Display confusion matrix
res=table(p$predictions,yTest)
print(res)

# Compute Balanced Classification Rate
BCR=mean(c(res[1,1]/sum(res[,1]),res[2,2]/sum(res[,2]),res[3,3]/sum(res[,3])))
print(BCR)

