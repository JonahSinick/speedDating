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
myTrainTemp = myTrain
myTrain = myTrain[,colSums(myTrain^2) != 0 & colSums(myTest^2) != 0]
myTest = myTest[,colSums(myTrainTemp^2) != 0 & colSums(myTest^2) != 0]


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
        print(newLogLoss)
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


myTest =  addProbCol(myTrain, myTest,  "superEnhancedDec_M_Score" ,c("decAvg_M_of_W", "raterDecAvg_M", "womanRateeScore","manRaterScore", menNames, womenNames), "dec_M")[["newTest"]]
myTest =  addProbCol(myTrain, myTest,  "superEnhancedDec_W_Score" ,c("decAvg_W_of_M", "raterDecAvg_W", "manRateeScore","womanRaterScore", menNames, womenNames), "dec_W")[["newTest"]]

myTrain =  addProbCol(myTrain, myTrain,  "superEnhancedDec_M_Score" ,c("decAvg_M_of_W", "raterDecAvg_M", "womanRateeScore","manRaterScore", menNames, womenNames), "dec_M")[["newTest"]]
myTrain =  addProbCol(myTrain, myTrain,  "superEnhancedDec_W_Score" ,c("decAvg_W_of_M", "raterDecAvg_W", "manRateeScore","womanRaterScore", menNames, womenNames), "dec_W")[["newTest"]]


myTest["simpleMatchScore"] = myTest["simpleDec_M_Score"]*myTest["simpleDec_W_Score"]
myTest["enhancedMatchScore"] = myTest["enhancedDec_M_Score"]*myTest["enhancedDec_W_Score"]
myTrain["simpleMatchScore"] = myTrain["simpleDec_M_Score"]*myTrain["simpleDec_W_Score"]
myTrain["enhancedMatchScore"] = myTrain["enhancedDec_M_Score"]*myTrain["enhancedDec_W_Score"]

myTrain["superEnhancedMatchScore"] = myTrain["superEnhancedDec_M_Score"]*myTrain["superEnhancedDec_W_Score"]
myTest["superEnhancedMatchScore"] = myTest["superEnhancedDec_M_Score"]*myTest["superEnhancedDec_W_Score"]

superDuperEnhancedSeq = c("superEnhancedDec_M_Score", "superEnhancedDec_W_Score", "superEnhancedMatchScore")


myTest =  addProbCol(myTrain, myTest,  "superDuperMatchScore" , superDuperEnhancedSeq, "match")[["newTest"]]
myTrain =  addProbCol(myTrain, myTrain,  "superDuperMatchScore" , superDuperEnhancedSeq, "match")[["newTest"]]


enhancedSeq= c(c("decAvg_M_of_W", "raterDecAvg_M", "womanRateeScore","manRaterScore") ,
               c("decAvg_W_of_M", "raterDecAvg_W", "manRateeScore","womanRaterScore"), 
               "enhancedMatchScore", "simpleMatchScore", "enhancedDec_M_Score", "enhancedDec_W_Score")
simpleSeq= c("decAvg_M_of_W", "raterDecAvg_M", "decAvg_W_of_M", "raterDecAvg_W", "simpleMatchScore")

myTest =  addProbCol(myTrain, myTest,  "betterSimpleMatchScore" ,simpleSeq, "match")[["newTest"]]
myTest =  addProbCol(myTrain, myTest,  "betterEnhancedMatchScore" ,enhancedSeq, "match")[["newTest"]]

myTrain =  addProbCol(myTrain, myTrain,  "betterSimpleMatchScore" ,simpleSeq, "match")[["newTest"]]
myTrain =  addProbCol(myTrain, myTrain,  "betterEnhancedMatchScore" ,enhancedSeq, "match")[["newTest"]]


superSeq = c(enhancedSeq, "betterEnhancedMatchScore","betterSimpleMatchScore",  menNames, womenNames)
myTrain =  addProbCol(myTrain, myTrain,  "combinedMatchScore" ,superSeq, "match")[["newTest"]]
myTest =  addProbCol(myTrain, myTest,  "combinedMatchScore" ,superSeq, "match")[["newTest"]]

myTrain =  addProbCol(myTrain, myTrain,  "betterCombinedMatchScore" ,c("superEnhancedDec_M_Score", "superEnhancedDec_W_Score", superSeq), "match")[["newTest"]]
myTest =  addProbCol(myTrain, myTest,  "betterCombinedMatchScore" ,c("superEnhancedDec_M_Score", "superEnhancedDec_W_Score", superSeq), "match")[["newTest"]]

superDuperSeq = c(enhancedSeq, "betterEnhancedMatchScore","betterSimpleMatchScore", "superEnhancedDec_M_Score", "superEnhancedDec_W_Score", "superEnhancedMatchScore", "superDuperMatchScore", "combinedMatchScore", "betterCombinedMatchScore",  menNames, womenNames)
myTrain =  addProbCol(myTrain, myTrain,  "finalMatchScore" ,superDuperSeq, "match")[["newTest"]]
myTest =  addProbCol(myTrain, myTest,  "finalMatchScore" , superDuperSeq, "match")[["newTest"]]



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

for(n in c("simpleMatchScore", "enhancedMatchScore","betterSimpleMatchScore", "betterEnhancedMatchScore", "combinedMatchScore" ,"betterCombinedMatchScore")){
  print(n)
  h(myTrain, "match", n)
}

