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
myCV = read.csv('~/Desktop/speedDating/currentTest.csv')
myTest = read.csv('~/Desktop/speedDating/currentCV.csv')

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


Names = names(myTrain)
Names = Names[!(Names %in% Names[grep("Sum|Rating_W_of_M|gender_M|Rating_M_of_W|genderRating_W_of_M|order|goal_M|goal_W|prob_M|prob_W|Cross|iid_W|id_W|wave_W|dec_W|dec_M|partner_W",Names)])]
menNames = Names[grep("W_of_M|_M$", Names)]
womenNames = Names[grep("M_of_W|_W$", Names)]
crossNames = Names[grep("Cross$", Names)]


sportsWoman = c("exerciseAct_W", "hikingAct_W", "yogaAct_W", "sportsAct_W")
myTest[["womanSportsScoreDecW"]] = getProbs(myTrain, myTest,sportsWoman, "dec_W")
myTrain[["womanSportsScoreDecW"]] = getProbs(myTrain, myTrain,sportsWoman, "dec_W")
myTest[["womanSportsScoreDecM"]] = getProbs(myTrain, myTest,sportsWoman, "dec_M")
myTrain[["womanSportsScoreDecM"]] = getProbs(myTrain, myTrain,sportsWoman, "dec_M")

sportsMan = c("exerciseAct_M", "hikingAct_M", "yogaAct_M", "sportsAct_M")
myTest[["manSportsScoreDecW"]] = getProbs(myTrain, myTest,sportsMan, "dec_W")
myTrain[["manSportsScoreDecW"]] = getProbs(myTrain, myTrain,sportsMan, "dec_W")
myTest[["manSportsScoreDecM"]] = getProbs(myTrain, myTest,sportsMan, "dec_M")
myTrain[["manSportsScoreDecM"]] = getProbs(myTrain, myTrain,sportsMan, "dec_M")


attrLikeFunWofM = c("attrRatingAvg_W_of_M", "likeRatingAvg_W_of_M", "funRatingAvg_W_of_M")
attrLikeFunMofW = c("attrRatingAvg_M_of_W", "likeRatingAvg_M_of_W", "funRatingAvg_M_of_W")

myTest[["manAttrLikeFunScoreDecW"]] = getProbs(myTrain, myTest,attrLikeFunMofW, "dec_W")
myTrain[["manAttrLikeFunScoreDecW"]] = getProbs(myTrain, myTrain,attrLikeFunMofW, "dec_W")
myTest[["manAttrLikeFunScoreDecM"]] = getProbs(myTrain, myTest,attrLikeFunMofW, "dec_M")
myTrain[["manAttrLikeFunScoreDecM"]] = getProbs(myTrain, myTrain,attrLikeFunMofW, "dec_M")


myTest[["womanAttrLikeFunScoreDecW"]] = getProbs(myTrain, myTest,attrLikeFunWofM, "dec_W")
myTrain[["womanAttrLikeFunScoreDecW"]] = getProbs(myTrain, myTrain,attrLikeFunWofM, "dec_W")
myTest[["womanAttrLikeFunScoreDecM"]] = getProbs(myTrain, myTest,attrLikeFunWofM, "dec_M")
myTrain[["womanAttrLikeFunScoreDecM"]] = getProbs(myTrain, myTrain,attrLikeFunWofM, "dec_M")




attrLikeFunGuessWofM = c("attrRatingGuess_W_of_M", "likeRatingGuess_W_of_M", "funRatingGuess_W_of_M")
attrLikeFunGuessMofW = c("attrRatingGuess_M_of_W", "likeRatingGuess_M_of_W", "funRatingGuess_M_of_W")

myTest[["manAttrLikeFunGuessScoreDecW"]] = getProbs(myTrain, myTest,attrLikeFunGuessMofW, "dec_W")
myTrain[["manAttrLikeFunGuessScoreDecW"]] = getProbs(myTrain, myTrain,attrLikeFunGuessMofW, "dec_W")
myTest[["manAttrLikeFunGuessScoreDecM"]] = getProbs(myTrain, myTest,attrLikeFunGuessMofW, "dec_M")
myTrain[["manAttrLikeFunGuessScoreDecM"]] = getProbs(myTrain, myTrain,attrLikeFunGuessMofW, "dec_M")


myTest[["womanAttrLikeFunGuessScoreDecW"]] = getProbs(myTrain, myTest,attrLikeFunGuessWofM, "dec_W")
myTrain[["womanAttrLikeFunGuessScoreDecW"]] = getProbs(myTrain, myTrain,attrLikeFunGuessWofM, "dec_W")
myTest[["womanAttrLikeFunGuessScoreDecM"]] = getProbs(myTrain, myTest,attrLikeFunGuessWofM, "dec_M")
myTrain[["womanAttrLikeFunGuessScoreDecM"]] = getProbs(myTrain, myTrain,attrLikeFunGuessWofM, "dec_M")



# Adding more stuff

IntelAmbSincWofM = c("intelRatingAvg_W_of_M", "ambRatingAvg_W_of_M", "sincRatingAvg_W_of_M")
IntelAmbSincMofW = c("intelRatingAvg_M_of_W", "ambRatingAvg_M_of_W", "sincRatingAvg_M_of_W")

myTest[["manIntelAmbSincScoreDecW"]] = getProbs(myTrain, myTest,IntelAmbSincMofW, "dec_W")
myTrain[["manIntelAmbSincScoreDecW"]] = getProbs(myTrain, myTrain,IntelAmbSincMofW, "dec_W")
myTest[["manIntelAmbSincScoreDecM"]] = getProbs(myTrain, myTest,IntelAmbSincMofW, "dec_M")
myTrain[["manIntelAmbSincScoreDecM"]] = getProbs(myTrain, myTrain,IntelAmbSincMofW, "dec_M")


myTest[["womanIntelAmbSincScoreDecW"]] = getProbs(myTrain, myTest,IntelAmbSincWofM, "dec_W")
myTrain[["womanIntelAmbSincScoreDecW"]] = getProbs(myTrain, myTrain,IntelAmbSincWofM, "dec_W")
myTest[["womanIntelAmbSincScoreDecM"]] = getProbs(myTrain, myTest,IntelAmbSincWofM, "dec_M")
myTrain[["womanIntelAmbSincScoreDecM"]] = getProbs(myTrain, myTrain,IntelAmbSincWofM, "dec_M")

IntelAmbSincGuessWofM = c("intelRatingGuess_W_of_M", "ambRatingGuess_W_of_M", "sincRatingGuess_W_of_M")
IntelAmbSincGuessMofW = c("intelRatingGuess_M_of_W", "ambRatingGuess_M_of_W", "sincRatingGuess_M_of_W")

myTest[["manIntelAmbSincGuessScoreDecW"]] = getProbs(myTrain, myTest,IntelAmbSincGuessMofW, "dec_W")
myTrain[["manIntelAmbSincGuessScoreDecW"]] = getProbs(myTrain, myTrain,IntelAmbSincGuessMofW, "dec_W")
myTest[["manIntelAmbSincGuessScoreDecM"]] = getProbs(myTrain, myTest,IntelAmbSincGuessMofW, "dec_M")
myTrain[["manIntelAmbSincGuessScoreDecM"]] = getProbs(myTrain, myTrain,IntelAmbSincGuessMofW, "dec_M")


myTest[["womanIntelAmbSincGuessScoreDecW"]] = getProbs(myTrain, myTest,IntelAmbSincGuessWofM, "dec_W")
myTrain[["womanIntelAmbSincGuessScoreDecW"]] = getProbs(myTrain, myTrain,IntelAmbSincGuessWofM, "dec_W")
myTest[["womanIntelAmbSincGuessScoreDecM"]] = getProbs(myTrain, myTest,IntelAmbSincGuessWofM, "dec_M")
myTrain[["womanIntelAmbSincGuessScoreDecM"]] = getProbs(myTrain, myTrain,IntelAmbSincGuessWofM, "dec_M")


manOfWomanDecWoman = c("manAttrLikeFunScoreDecW", "manAttrLikeFunGuessScoreDecW", "manIntelAmbSincScoreDecW", "manIntelAmbSincGuessScoreDecW")
manOfWomanDecMan = c("manAttrLikeFunScoreDecM", "manAttrLikeFunGuessScoreDecM", "manIntelAmbSincScoreDecM", "manIntelAmbSincGuessScoreDecM")

womanOfManDecWoman = c("womanAttrLikeFunScoreDecW", "womanAttrLikeFunGuessScoreDecW", "womanIntelAmbSincScoreDecW", "womanIntelAmbSincGuessScoreDecW")
womanOfManDecMan = c("womanAttrLikeFunScoreDecM", "womanAttrLikeFunGuessScoreDecM", "womanIntelAmbSincScoreDecM", "womanIntelAmbSincGuessScoreDecM")

myTest[["WofMqualityDecWomanScore"]] = getProbs(myTrain, myTest,c(womanOfManDecWoman), "dec_W")
myTest[["WofMqualityDecManScore"]] = getProbs(myTrain, myTest,c(womanOfManDecMan), "dec_M")

myTrain[["WofMqualityDecWomanScore"]] = getProbs(myTrain, myTrain,c(womanOfManDecWoman), "dec_W")
myTrain[["WofMqualityDecManScore"]] = getProbs(myTrain, myTrain,c(womanOfManDecMan), "dec_M")

myTest[["MofWqualityDecWomanScore"]] = getProbs(myTrain, myTest,c(manOfWomanDecWoman), "dec_W")
myTest[["MofWqualityDecManScore"]] = getProbs(myTrain, myTest,c(manOfWomanDecMan), "dec_M")

myTrain[["MofWqualityDecWomanScore"]] = getProbs(myTrain, myTrain,c(manOfWomanDecWoman), "dec_W")
myTrain[["MofWqualityDecManScore"]] = getProbs(myTrain, myTrain,c(manOfWomanDecMan), "dec_M")


myTest[["decWomanGuess"]] = getProbs(myTrain, myTest,c("MofWqualityDecWomanScore", "WofMqualityDecWomanScore", "decAvg_W_of_M", "raterDecAvg_W"), "dec_W")
myTest[["decManGuess"]] = getProbs(myTrain, myTest,c("MofWqualityDecManScore", "WofMqualityDecManScore", "decAvg_M_of_W", "raterDecAvg_M"), "dec_M")

myTest[["decWomanBasicGuess"]] = getProbs(myTrain, myTest,c("decAvg_W_of_M", "raterDecAvg_W"), "dec_W")
myTest[["decManBasicGuess"]] = getProbs(myTrain, myTest,c("decAvg_M_of_W", "raterDecAvg_M"), "dec_M")



myCV[["manAttrLikeFunScoreDecW"]] = getProbs(myTrain, myCV,attrLikeFunMofW, "dec_W")
myCV[["manAttrLikeFunScoreDecM"]] = getProbs(myTrain, myCV,attrLikeFunMofW, "dec_M")


myCV[["womanAttrLikeFunScoreDecW"]] = getProbs(myTrain, myCV,attrLikeFunWofM, "dec_W")
myCV[["womanAttrLikeFunScoreDecM"]] = getProbs(myTrain, myCV,attrLikeFunWofM, "dec_M")


myCV[["manAttrLikeFunGuessScoreDecW"]] = getProbs(myTrain, myCV,attrLikeFunGuessMofW, "dec_W")
myCV[["manAttrLikeFunGuessScoreDecM"]] = getProbs(myTrain, myCV,attrLikeFunGuessMofW, "dec_M")


myCV[["womanAttrLikeFunGuessScoreDecW"]] = getProbs(myTrain, myCV,attrLikeFunGuessWofM, "dec_W")
myCV[["womanAttrLikeFunGuessScoreDecM"]] = getProbs(myTrain, myCV,attrLikeFunGuessWofM, "dec_M")



# Adding more stuff

IntelAmbSincWofM = c("intelRatingAvg_W_of_M", "ambRatingAvg_W_of_M", "sincRatingAvg_W_of_M")
IntelAmbSincMofW = c("intelRatingAvg_M_of_W", "ambRatingAvg_M_of_W", "sincRatingAvg_M_of_W")

myCV[["manIntelAmbSincScoreDecW"]] = getProbs(myTrain, myCV,IntelAmbSincMofW, "dec_W")
myCV[["manIntelAmbSincScoreDecM"]] = getProbs(myTrain, myCV,IntelAmbSincMofW, "dec_M")


myCV[["womanIntelAmbSincScoreDecW"]] = getProbs(myTrain, myCV,IntelAmbSincWofM, "dec_W")
myCV[["womanIntelAmbSincScoreDecM"]] = getProbs(myTrain, myCV,IntelAmbSincWofM, "dec_M")


myCV[["manIntelAmbSincGuessScoreDecW"]] = getProbs(myTrain, myCV,IntelAmbSincGuessMofW, "dec_W")
myCV[["manIntelAmbSincGuessScoreDecM"]] = getProbs(myTrain, myCV,IntelAmbSincGuessMofW, "dec_M")


myCV[["womanIntelAmbSincGuessScoreDecW"]] = getProbs(myTrain, myCV,IntelAmbSincGuessWofM, "dec_W")
myCV[["womanIntelAmbSincGuessScoreDecM"]] = getProbs(myTrain, myCV,IntelAmbSincGuessWofM, "dec_M")



myCV[["WofMqualityDecWomanScore"]] = getProbs(myTrain, myCV,c(womanOfManDecWoman), "dec_W")
myCV[["WofMqualityDecManScore"]] = getProbs(myTrain, myCV,c(womanOfManDecMan), "dec_M")


myCV[["MofWqualityDecWomanScore"]] = getProbs(myTrain, myCV,c(manOfWomanDecWoman), "dec_W")
myCV[["MofWqualityDecManScore"]] = getProbs(myTrain, myCV,c(manOfWomanDecMan), "dec_M")



myCV[["decWomanGuess"]] = getProbs(myTrain, myCV,c("MofWqualityDecWomanScore", "WofMqualityDecWomanScore", "decAvg_W_of_M", "raterDecAvg_W"), "dec_W")
myCV[["decManGuess"]] = getProbs(myTrain, myCV,c("MofWqualityDecManScore", "WofMqualityDecManScore", "decAvg_M_of_W", "raterDecAvg_M"), "dec_M")

myCV[["decWomanBasicGuess"]] = getProbs(myTrain, myCV,c("decAvg_W_of_M", "raterDecAvg_W"), "dec_W")
myCV[["decManBasicGuess"]] = getProbs(myTrain, myCV,c("decAvg_M_of_W", "raterDecAvg_M"), "dec_M")

h(myTest, "dec_W", "decWomanGuess")

addProbCol = function(train, test, colName, features, tar){
  outputHash = hash()
  len = length(features)
  currentFeatures = c()
  bestLogLoss = 100
  best_p = 0
  bestModel = NA
  for(i in 1:len){
    featureAdded = FALSE
    for(f in features[i:len]){
      tempFeatures =  c(currentFeatures, f)
      s=scale(train[tempFeatures],center=TRUE,scale=TRUE)
      co=heuristicC(s)
      m=LiblineaR(data=s,labels=factor(train[,tar]),type=0,cost=co,bias=TRUE,verbose=FALSE)
      s2= scale(test[tempFeatures],attr(s,"scaled:center"),attr(s,"scaled:scale"))
      p=predict(m,s2,prob=TRUE)
      probs = p$probabilities[,"1"]
      newLogLoss = logLoss(test[[tar]],probs)
      if(newLogLoss < bestLogLoss){
        bestModel = m
        bestLogLoss = newLogLoss
        newFeature = f
        featureAdded = TRUE
      }
    }
    if(featureAdded == FALSE | i == len){
      return(probs)
    }
    else{
      print(newFeature)
      print(newLogLoss)
      currentFeatures = c(currentFeatures, newFeature)
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
# basic_features = c("decAvg_W_of_M", "raterDecAvg_W")
features =  c(menNames, womenNames, crossNames)
probs = getProbs(myTrain,myTest,features, "dec_W")
myTest[["predictedWomanRating"]] = probs
h(myTest, "dec_W", "predictedWomanRating")
m = addProbCol(myTrain,myTest, "haha",features, "dec_W")
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


coolFeatures= c("decAvg_W_of_M", "raterDecAvg_W", "funRatingAvg_W_of_M", "raceWhite_M", 
                "likeRatingGuess_W_of_M", "clubbingAct_M", "sharRatingAvg_M_of_W", "date5_M",
                "fieldMath_W", "clubbingAct_W", "goalMeetNew_W", "raterDecAvg_M", "met_W", "attrPref_W",
                "date_M", "go_out5_M", "funPref_M", "go_out4_M", "careerFinance_M", "careerMedicine_M",
                "funRatingAvg_M_of_W", "gamingAct_W", "sharPref_M", "decAvg_M_of_W", "concertsAct_W",
                "date5_W", "go_out3_M", "musicAct_M", "exerciseAct_W", "income_W", "yogaAct_M", "shoppingAct_W",
                "ambPref_M", "met_M", "likeRatingAvg_M_of_W")

len = length(coolFeatures)
for(i in 1:len){
  print(coolFeatures[i])
  features = coolFeatures[1:i]
  s=scale(myTrain[features],center=TRUE,scale=TRUE)
  co=heuristicC(s)
  m=LiblineaR(data=s,labels=factor(myTrain[,"dec_W"]),type=0,cost=co,bias=TRUE,verbose=FALSE)
  s2= scale(myTest[features],attr(s,"scaled:center"),attr(s,"scaled:scale"))
  p=predict(m,s2,prob=TRUE)
  probs = p$probabilities[,"1"]
  myTest[["enhancedDecWomenScore"]] = p$probabilities[,"1"] 
  h(myTest, "dec_W", "enhancedDecWomenScore")
}


