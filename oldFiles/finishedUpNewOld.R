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


Names = names(myTrain)
Names = Names[!(Names %in% Names[grep("Sum|Rating_W_of_M|gender_M|Rating_M_of_W|genderRating_W_of_M|order|goal_M|goal_W|prob_M|prob_W|Cross|iid_W|id_W|wave_W|dec_W|dec_M",Names)])]
menNames = Names[grep("W_of_M|_M$", Names)]
womenNames = Names[grep("M_of_W|_W$", Names)]



addProbCol = function(train, test, colName, features, tar){
  outputHash = hash()
  len = length(features)
  currentFeatures = c()
  bestLogLoss = 100
  best_p = 0
  bestModel = NA
  for(i in 1:len){
    newFeature = FALSE
    for(f in features){
      tempFeatures =  c(currentFeatures, f)
      s=scale(train[tempFeatures],center=TRUE,scale=TRUE)
      co=heuristicC(s)
      m=LiblineaR(data=s,labels=factor(train[,tar]),type=0,cost=co,bias=TRUE,verbose=FALSE)
      s2= scale(test[tempFeatures],attr(s,"scaled:center"),attr(s,"scaled:scale"))
      p=predict(m,s2,prob=TRUE)
      probs = p$probabilities[,"1"]
      newLogLoss = logLoss(test[[tar]],probs)
      if(newLogLoss < bestLogLoss){
        print(currentFeatures)
        print(newLogLoss)
        bestModel = m
        bestLogLoss = newLogLoss
        replacementFeatures = tempFeatures
        newFeature = TRUE
      }
    }
    if(newFeature == FALSE | i == len){
      print(currentFeatures)
      return(currentFeatures)
    }
    else{
      currentFeatures = replacementFeatures
    }
  }
}

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
basic_features = c("decAvg_W_of_M", "raterDecAvg_W")
features =  c(menNames, basic_features)
m = addProbCol(myTrain, myTest, "haha",features, "dec_W")
m2 = addProbCol(myTrain, myTest, "haha", basic_features, "dec_W")

crazyFeatures = c("attrRatingAvg_W_of_M", "funRatingAvg_W_of_M",  "ambRatingAvg_W_of_M",  "sharRatingAvg_W_of_M", "likeRatingAvg_W_of_M", "decAvg_W_of_M",        "age_M",               
                  "tuition_M",            "race_M",               "imprace_M",            "date_M",               "exerciseAct_M",        "diningAct_M",          "museumsAct_M",        
                  "artAct_M",             "clubbingAct_M",        "moviesAct_M",          "concertsAct_M",        "shoppingAct_M",        "yogaAct_M",            "attrPref_M",          
                  "intelPref_M",          "goalFunNight_M",       "raceAsian_M",          "raceWhite_M",          "raceLatino_M",         "raceBlack_M",          "go_out2_M",           
                  "fieldLaw_M",           "fieldMath_M",          "fieldBusiness_M",      "fieldEngin_M",         "fieldSocialSci_M",     "careerAcademic_M",     "careerMedicine_M",    
                  "raterDecAvg_M",        "raterDecAvg_W")
s=scale(myTrain[crazyFeatures],center=TRUE,scale=TRUE)
s2= scale(myTest[crazyFeatures],attr(s,"scaled:center"),attr(s,"scaled:scale"))
p=predict(m,s2,prob=TRUE)


s=scale(myTrain[basic_features],center=TRUE,scale=TRUE)
s2= scale(myTest[basic_features],attr(s,"scaled:center"),attr(s,"scaled:scale"))
p2=predict(m2,s2,prob=TRUE)

myTest[["enhancedDecWomenScore"]] = p$probabilities[,"1"]

myTest[["weakWomenScore"]] = p2$probabilities[,"1"]


h(myTest, "dec_W", "enhancedDecWomenScore")
h(myTest, "dec_W", "weakWomenScore")

s=scale(myTrain[crazyFeatures],center=TRUE,scale=TRUE)
s2= scale(myCV[crazyFeatures],attr(s,"scaled:center"),attr(s,"scaled:scale"))
p=predict(m,s2,prob=TRUE)


s=scale(myTrain[basic_features],center=TRUE,scale=TRUE)
s2= scale(myCV[basic_features],attr(s,"scaled:center"),attr(s,"scaled:scale"))
p2=predict(m2,s2,prob=TRUE)

myCV[["enhancedDecWomenScore"]] = p$probabilities[,"1"]

myCV[["weakWomenScore"]] = p2$probabilities[,"1"]


h(myCV, "dec_W", "enhancedDecWomenScore")
h(myCV, "dec_W", "weakWomenScore")
