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



addProbColsGender = function(genderHash){
  train = genderHash[["train"]]
  test = genderHash[["test"]]
  tars = c("dec_M","dec_W")
  colNames = genderHash[["colNames"]]
  menFeatures = genderHash[["featuresMen"]]
  womenFeatures = genderHash[["featuresWomen"]]
  outputHash  = addProbCol(genderHash, colNames[1], menFeatures, tars[1], 4)
  outputHash  = addProbCol(genderHash, colNames[2], womenFeatures, tars[2], 4)
  return(outputHash)
}

addProbCol = function(dfHash, colName, features, tar, precision){
  train = dfHash[["train"]]
  test = dfHash[["test"]]
  len = length(features)
  finished = FALSE
  currentFeatures = c()
  bestLogLoss = 100
  for(i in 1:len){
    print("HERE")
    print(i)
    print(currentFeatures)
    betterFound = FALSE
    lenNew = length(currentFeatures) + 1
    tempBestLogLoss = BestLogLoss
    for(f in features){
      tempFeatures =  c(currentFeatures, f)
      probs = getProbs(train, train, tempFeatures, tar)
      newLogLoss = logLoss(train[[tar]],probs)
      if(round(newLogLoss,precision) < round(bestLogLoss),precision){
        bestLogLoss = newLogLoss
        print(newLogLoss)
        currentBestFeatures = tempFeatures
        betterFound = TRUE
      }
    }
    currentFeatures = currentBestFeatures
    if(betterFound == FALSE | i == len){
      train[[colName]] = probs
      test[[colName]] = getProbs(train, test, currentFeatures, tar)
      dfHash[["train"]] = train
      dfHash[["test"]] = test
      dfHash[["features"]] = currentFeatures
      return(dfHash)
    } 
  }
}



Names = names(myTrain)
Names = Names[!(Names %in% Names[grep("Sum|Guess|Rating_W_of_M|gender_M|Rating_M_of_W|genderRating_W_of_M|order|goal_M|goal_W|prob_M|prob_W|Cross|iid_W|id_W|wave_W|dec_W|dec_M|partner_W|pid_W",Names)])]
menNames = Names[grep("W_of_M|_M$", Names)]
womenNames = Names[grep("M_of_W|_W$", Names)]
guessNames = Names[grep("Guess$", Names)]
crossNames = Names[grep("Cross", Names)]
menDecAvgs = c("decAvg_M_of_W", "raterDecAvg_M") 
womenDecAvgs = c("decAvg_W_of_M", "raterDecAvg_W") 

genderHash = hash()
genderHash[["train"]] = myTrain
genderHash[["test"]] = myTest

genderHash[["colNames"]] = c("womanRateeScore","manRateeScore")
genderHash[["featuresMen"]] = womenNames
genderHash[["featuresWomen"]] = menNames
genderHash = addProbColsGender(genderHash)

genderHash[["colNames"]] = c("manRaterScore","womanRaterScore")
genderHash[["featuresMen"]] = menNames
genderHash[["featuresWomen"]] = womenNames

genderHash = addProbColsGender(genderHash)

genderHash[["colNames"]] = c("simpleDecManScore","simpleDecWomanScore")
genderHash[["featuresMen"]] = menDecAvgs
genderHash[["featuresWomen"]] = womenDecAvgs
genderHash = addProbColsGender(genderHash)


currentFeatures = c(menNames, womenNames, "womanRateeScore", "manRateeScore", 
                     "womanRaterScore", "manRaterScore", "simpleDecManScore", "simpleDecWomanScore")
genderHash[["colNames"]] = c("decManScoreNoInters", "decWomanScoreNoInters")
genderHash[["featuresMen"]] = currentFeatures
genderHash[["featuresWomen"]] = currentFeatures

train = genderHash[["train"]]
test = genderHash[["test"]]
train[["naiveMatchScore"]] = train[["simpleDecManScore"]]*train[["simpleDecWomanScore"]]
test[["naiveMatchScore"]] = test[["simpleDecManScore"]]*test[["simpleDecWomanScore"]]
train[["sophMatchScore"]] = train[["decManScoreNoInters"]]*train[["decManScoreNoInters"]]
test[["sophMatchScore"]] = test[["decWomanScoreNoInters"]]*test[["decWomanScoreNoInters"]]
genderHash[["train"]] = train
genderHash[["test"]] = test

genderHash = addProbCol(genderHash, "simpleMatchScore", c("naiveMatchScore", menDecAvgs, womenDecAvgs), "match")
genderHash = addProbCol(genderHash, "matchScoreNoInters", c(currentFeatures, "naiveMatchScore", "sophMatchScore", "simpleMatchScore"), "match")


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

orderedFeatures = c("decAvg_M_of_W",         "attrRatingAvg_M_of_W",  "met_W",                 "fieldScience_W",        "income_W",             
  "careerPsychology_W",    "exerciseAct_W",         "theaterAct_W",          "goalOther_W",           "fieldAcademia_W",      
  "sportsAct_W",           "go_out2_W",             "careerInternational_W", "yogaAct_W",             "sincPref_W",           
  "intelRatingAvg_M_of_W", "likeRatingAvg_M_of_W",  "careerCreative_W",      "goalGetDate_W",         "gamingAct_W",          
  "race_W",                "moviesAct_W",           "tvAct_W",               "shoppingAct_W",         "careerAcademic_W",     
  "date7_W",               "fieldSocialWork_W",     "date2_W",               "go_out1_W",             "careerSocialWork_W",   
  "artAct_W",              "goalSeriousRel_W",      "fieldHistReligPhil_W",  "funRatingAvg_M_of_W",   "fieldMath_W",          
  "careerMedicine_W",      "intelPref_W",           "museumsAct_W",          "funPref_W",             "fieldEngin_W",         
  "hikingAct_W",           "go_out_W",              "raceWhite_W",           "fieldEnglish_W",        "fieldSocialSci_W",     
  "tuition_W",             "exphappy_W",            "goalFunNight_W",        "attrRatingAvg_M_of_W",  "date3_W",              
  "samerace_W",            "readingAct_W",          "activityAvg_W",         "sincRatingAvg_M_of_W",  "fieldLaw_W",           
  "decAvg_M_of_W",         "go_out4_W",             "go_out3_W",             "sharRatingAvg_M_of_W",  "fieldBusiness_W",      
  "concertsAct_W",         "imprace_W",             "raceAsian_W",           "go_out3_W",             "fieldScience_W",       
  "attrRatingAvg_M_of_W",  "income_W",              "go_out_W",              "diningAct_W",           "artAct_W",             
  "go_out_W",              "clubbingAct_W",         "raterDecAvg_W",         "age_W",                 "yogaAct_W",            
  "raceLatino_W",          "fieldScience_W")
for(i in 1:length(orderedFeatures)){
  features = orderedFeatures[1:i]
  probs = getProbs(myTrain, myTest, features, "dec_M")
  print(i)
  print(logLoss(myTest[["dec_M"]], probs),3)
  
}