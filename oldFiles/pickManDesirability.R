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
myTrain = myTrain[,colSums(myTrain^2) != 0 & colSums(myTest^2) != 0]
myTest = myTest[,colSums(myTrain^2) != 0 & colSums(myTest^2) != 0]


getProbs = function(train, test, features, tar){
  s=scale(train[features],center=TRUE,scale=TRUE)
  co=heuristicC(s)
  m=LiblineaR(data=s,labels=factor(train[,tar]),type=0,cost=co,bias=TRUE,verbose=FALSE)
  s2= scale(test[features],attr(s,"scaled:center"),attr(s,"scaled:scale"))
  p=predict(m,s2,prob=TRUE)
  probs = p$probabilities[,"1"]
  return(probs)
}



addProbCol = function(train, test, colName, features, tar){
  print(colName)
  outputHash = hash()
  len = length(features)
  currentFeatures = c()
  bestLogLoss = 100
  for(i in 1:len){
    lenNew = length(currentFeatures) + 1
    for(f in features){
      tempFeatures =  c(currentFeatures, f)
      probs = getProbs(train, test, tempFeatures, tar)
      newLogLoss = logLoss(test[[tar]],probs)
      if(newLogLoss < bestLogLoss){
        bestLogLoss = newLogLoss
        test[[colName]] = probs
        currentFeatures = tempFeatures
      }
    }
    if(len + 1 >= lenNew | i == len){
      outputHash[["newTest"]] = test
      outputHash[["features"]] = currentFeatures
      return(outputHash)
    }
  }
}

createProbs = function(train, test, featureHash, base, tar){
  TCBHash = hash()
  for(colName in keys(featureHash)){
    features = c(base, featureHash[[colName]])
    outputHash = addProbCol(train, test, colName, features, tar)
    test = outputHash[["newTest"]]
    outputHash = del("newTest", outputHash)
    TCBHash[[colName]] = outputHash
  }
  TCBHash[["newTest"]] = test
  return(TBCHash)
}




decAvgOnMan = hash()


Names = names(myTrain)
Names = Names[!(Names %in% Names[grep("Sum|Guess|Rating_W_of_M|gender_M|Rating_M_of_W|genderRating_W_of_M|order|goal_M|goal_W|prob_M|prob_W|Cross|iid_W|id_W|wave_W|dec_W|dec_M",Names)])]
menNames = Names[grep("W_of_M|_M$", Names)]
womenNames = Names[grep("M_of_W|_W$", Names)]

setScore = function(train,test,colName,features,target){
  answerHash = hash()
  answerHash[["newTrain"]] = addProbCol(train, train,  colName , features, target)[["newTest"]]
  answerHash[["newTest"]] = addProbCol(train, test,  colName , features, target)[["newTest"]]
  return(answerHash)
}




myTest =  addProbCol(myTrain, myTest,  "manRateeScore" , menNames, "dec_W")[["newTest"]]
myTest = addProbCol(myTrain, myTest,  "womanRateeScore" , womenNames, "dec_M")[["newTest"]]
myTest = addProbCol(myTrain, myTest,  "manRaterScore" , menNames, "dec_M")[["newTest"]]
myTest =  addProbCol(myTrain, myTest,  "womanRaterScore" , womenNames, "dec_W")[["newTest"]]

myTest =  addProbCol(myTrain, myTest,  "simpleManRateeScore" ,c("decAvg_M_of_W"), "dec_W")[["newTest"]]
myTest =  addProbCol(myTrain, myTest,  "simpleWomanRateeScore" , c("decAvg_W_of_M"), "dec_M")[["newTest"]]
myTest =  addProbCol(myTrain, myTest,  "simpleManRaterScore" ,c("raterDecAvg_M"), "dec_M")[["newTest"]]
myTest =  addProbCol(myTrain, myTest,  "simpleWomanRaterScore" , c("raterDecAvg_W"), "dec_W")[["newTest"]]

myTrain =  addProbCol(myTrain, myTrain,  "manRateeScore" , menNames, "dec_W")[["newTest"]]
myTrain = addProbCol(myTrain, myTrain,  "womanRateeScore" , womenNames, "dec_M")[["newTest"]]
myTrain = addProbCol(myTrain, myTrain,  "manRaterScore" , menNames, "dec_M")[["newTest"]]
myTrain =  addProbCol(myTrain, myTrain,  "womanRaterScore" , womenNames, "dec_W")[["newTest"]]

myTrain =  addProbCol(myTrain, myTrain,  "simpleManRateeScore" ,c("decAvg_M_of_W"), "dec_W")[["newTest"]]
myTrain =  addProbCol(myTrain, myTrain,  "simpleWomanRateeScore" , c("decAvg_W_of_M"), "dec_M")[["newTest"]]
myTrain =  addProbCol(myTrain, myTrain,  "simpleManRaterScore" ,c("raterDecAvg_M"), "dec_M")[["newTest"]]
myTrain =  addProbCol(myTrain, myTrain,  "simpleWomanRaterScore" , c("raterDecAvg_W"), "dec_W")[["newTest"]]

myTest =  addProbCol(myTrain, myTest,  "simpleDec_M_Score" ,c("decAvg_M_of_W", "raterDecAvg_M"), "dec_M")[["newTest"]]
myTest =  addProbCol(myTrain, myTest,  "simpleDec_W_Score" ,c("decAvg_W_of_M", "raterDecAvg_W"), "dec_W")[["newTest"]]

myTrain =  addProbCol(myTrain, myTrain,  "simpleDec_M_Score" ,c("decAvg_M_of_W", "raterDecAvg_M"), "dec_M")[["newTest"]]
myTrain =  addProbCol(myTrain, myTrain,  "simpleDec_W_Score" ,c("decAvg_W_of_M", "raterDecAvg_W"), "dec_W")[["newTest"]]

myTest =  addProbCol(myTrain, myTest,  "enhancedDec_M_Score" ,c("decAvg_M_of_W", "raterDecAvg_M", "womanRateeScore","manRaterScore"), "dec_M")[["newTest"]]
myTest =  addProbCol(myTrain, myTest,  "enhancedDec_W_Score" ,c("decAvg_W_of_M", "raterDecAvg_W", "manRateeScore","womanRaterScore"), "dec_W")[["newTest"]]

myTrain =  addProbCol(myTrain, myTrain,  "enhancedDec_M_Score" ,c("decAvg_M_of_W", "raterDecAvg_M", "womanRateeScore","manRaterScore"), "dec_M")[["newTest"]]
myTrain =  addProbCol(myTrain, myTrain,  "enhancedDec_W_Score" ,c("decAvg_W_of_M", "raterDecAvg_W", "manRateeScore","womanRaterScore"), "dec_W")[["newTest"]]



myTrain = myTrain[1:284]

myTest["matchUsingSimpleRateeScores"] = myTest["decAvg_W_of_M"]*myTest["raterDecAvg_W"]
myTest["dec_M_UsingDecAvgs"] = myTest["decAvg_M_of_W"]*myTest["raterDecAvg_M"]




h = function(df, tar,probs){
  tar = df[[tar]]
  probs = df[[probs]]
  tab = table(ifelse(probs > 0.5, 1, 0),tar)
  logloss = round(logLoss(tar, probs),2)
  print(c("logLoss:", logloss))
  frac_yes = round((tab[1,2] + tab[2,2])/length(tar), 2)
  print(c("fracYes:", frac_yes))
  totalError = round((tab[2,1] + tab[1,2])/length(tar), 2)
  print(c("totalError:", totalError))
  fracYesFound = round(tab[2,2]/(tab[1,2] + tab[2,2]), 2)
  print(c("fracYesFound", fracYesFound))
  return(tab)
}

for(n in c("simpleDec_M_Score", "simpleManRateeScore","womanRaterScore","simpleWomanRaterScore")){
  print(n)
  h(myTrain, "dec_W", n)
}


for(n in c("womanRateeScore", "simpleWomanRateeScore","manRaterScore","simpleManRaterScore")){
  print(n)
  h(myTrain, "dec_M", n)
}

