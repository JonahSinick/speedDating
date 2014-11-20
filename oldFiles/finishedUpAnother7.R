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
myTestTemp = myTest

myTrain = myTrain[,(colSums(myTrain^2) > 1) & (colSums(myTest^2) > 1)]
myTest = myTest[,(colSums(myTrainTemp^2)) > 1 & (colSums(myTest^2) > 1)]

getProbs = function(train, test, features, tar){
  s=scale(train[features],center=TRUE,scale=TRUE)
  co=heuristicC(s)
  m=LiblineaR(data=s,labels=factor(train[,tar]),type=0,cost=co,bias=TRUE,verbose=FALSE)
  s2= scale(test[features],attr(s,"scaled:center"),attr(s,"scaled:scale"))
  p=predict(m,s2,prob=TRUE)
  probs = p$probabilities[,"1"]
  return(probs)
}
groupsHash = hash()

n = names(myTrain)
groupsHash[["races"]] = c("raceAsianW", "raceWhiteW", "raceWhiteM","raceBlackM", "raceLatinoW", "raceAsianM", "raceLatinoM", "raceBlackW")
groupsHash[["raceCrossesImpW"]] = c("raceWhiteMraceWhiteWimpRaceW", "raceAsianMraceWhiteWimpRaceW", "raceAsianMraceWhiteWimpRaceW", "raceAsianMraceAsianWimpRaceW")
groupsHash[["raceCrossesImpM"]] = c("raceWhiteMraceAsianWimpRaceM", "raceWhiteMraceAsianWimpRaceM", "raceWhiteMraceLatinoWimpRaceM", "raceAsianMraceWhiteWimpRaceM")
groupsHash[["careersAndFields"]] = c("careerCreativeW", "careerInternationalW", "careerAcademicW", "careerFinanceW", "careerUndecidedW",
                            "careerMedicineW", "careerSocialWorkW", "careerCreativeM", "careerLawM", "careerFinanceM", "careerAcademicM",
                            "fieldAcademiaW", "fieldSocialSciW", "fieldSocialWorkW", "fieldEnginW", "fieldBusinessW", "fieldPoliSciW", "fieldEnglishW", "fieldScienceW",
                            "fieldBusinessM", "fieldEnginM")
groupsHash[["careerCrosses"]] = c("careerAcademicMcareerAcademicW", 
                                   "careerFinanceMcareerCreativeW", "careerFinanceMcareerAcademicW")

groupsHash[["physActs"]] = n[grep("PhysAct", n)]
groupsHash[["artActs"]] = n[grep("ArtAct", n)]
groupsHash[["wealthInds"]] = n[grep("WealthInd", n)]
groupsHash[["miscActs"]] = n[grep("miscAct", n)]
groupsHash[["dateFreq"]] = n[grep("date", n)]
groupsHash[["goOutFreq"]] = n[grep("goOut", n)]
groupsHash[["goals"]] = n[grep("goal", n)]
groupsHash[["prefs"]] = n[grep("Pref", n)]
groupsHash[["other"]] = c("impraceM", "impraceW", "expnumM", "expnumW", "exphappyM", "exphappyW", "metM", "metW", "impreligM", "impreligW")
groupsHash[["avgRatings"]] = n[grep("AvgRatingOf", n)]
groupsHash[["guessRatings"]] = n[grep("RatingGuess", n)]
groupsHash[["allRatings"]] = c(groupsHash[["avgRatings"]], groupsHash[["guessRatings"]])
ratingTypes = c("attrs", "sincs", "intels", "funs", "ambs", "shars", "likes", "probs")
for(i in 1:length(ratingTypes)){
  avgs = groupsHash[["avgRatings"]]
  guesses = groupsHash[["guessRatings"]]
  groupsHash[[ratingTypes[i]]]  = c(avgs[i], avgs[i + 8], guesses[i], guesses[i + 8])
}
genderHash = hash()
genderHash[["train"]] = myTrain
genderHash[["test"]] = myTest

oldColNum = length(names(myTest))
addProbCols = function(train, test, groupsHash){
  
  test[["raterBaselineDecM"]] = getProbs(train, test, c("raterDecAvgM"), "decM")
  
  test[["raterBaselineDecW"]] = getProbs(train, test, c("raterDecAvgW"), "decW")
  
  test[["rateeBaselineDecM"]] = getProbs(train, test, c("decAvgbyWofM"), "decM")
  
  test[["rateeBaselineDecW"]] = getProbs(train, test, c("decAvgbyMofW"), "decW")
  for(k in keys(groupsHash)){
    print(k)
    features = groupsHash[[k]]
    featuresM = features[grep("M$",features)]
    featuresW = features[grep("W$",features)]
    colName = k
    colnameManTraitsDecMan = paste(colName,"ManTraitsDecM",sep="")
    colnameWomanTraitsDecWoman = paste(colName,"WomanTraitsDecW",sep="")
    colnameManTraitsDecWoman = paste(colName,"ManTraitsDecW",sep="")
    colnameWomanTraitsDecMan = paste(colName,"WomanTraitsDecM",sep="")

    test[[colnameManTraitsDecMan]] = getProbs(train, test, c(featuresM, "decAvgbyMofW"), "decM")
    test[[colnameWomanTraitsDecWoman]] = getProbs(train, test, c(featuresW, "decAvgbyWofM"), "decW")
    test[[colnameManTraitsDecWoman]] = getProbs(train, test, c(featuresM, "raterDecAvgW"), "decW")
    test[[colnameWomanTraitsDecMan]] = getProbs(train, test,  c(featuresW, "raterDecAvgM"), "decM")
  }
  return(test)
}

myTest = addProbCols(myTrain,myTest,groupsHash)
myTrain = addProbCols(myTrain,myTrain,groupsHash)

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
#      errorFrame[name, "totalError"] = round((tab[1,2] + tab[2,1])/length(tar), 3)
#      errorFrame[name,"positivesFound"] = round(tab[2,2]/(tab[1,2] + tab[2,2]), 3)
    errorFrame[name, "guessNoNo"] = tab[1,1]
    errorFrame[name, "guessNoYes"] = tab[1,2]
    errorFrame[name, "totalNum"] = length(tar)
  }
  return(errorFrame)
}


n = c(names(myTest))
menRaterNames = c("rateeBaselineDecM", n[grep("ManTraitsDecM",n)], "decM")
womenRaterNames = c("rateeBaselineDecW", n[grep("WomanTraitsDecW",n)], "decW")
menRateeNames = c("raterBaselineDecM", n[grep("WomanTraitsDecM",n)], "decM")
womenRateeNames = c("raterBaselineDecW", n[grep("ManTraitsDecW",n)], "decW")

menRaterFrame = myTest[menRaterNames]
womenRaterFrame = myTest[womenRaterNames]
menRateeFrame = myTest[menRateeNames]
womenRateeFrame = myTest[womenRateeNames]
menRaterErrorFrame = makeErrorFrame(menRaterFrame, "decM")
womenRaterErrorFrame = makeErrorFrame(womenRaterFrame, "decW")
menRateeErrorFrame = makeErrorFrame(menRateeFrame, "decM")
womenRateeErrorFrame = makeErrorFrame(womenRateeFrame, "decW")

myTest[["menImprovedRatingsdecM"]] = getProbs(myTrain, myTest, mensImprovedRatings, "decM")
myTest[["womenImprovedRatingsdecW"]] = getProbs(myTrain, myTest, womensImprovedRatings, "decW")
myTrain[["menImprovedRatingsdecM"]] = getProbs(myTrain, myTrain, mensImprovedRatings, "decM")
myTrain[["womenImprovedRatingsdecW"]] = getProbs(myTrain, myTrain, womensImprovedRatings, "decW")



women = c("allRatingsMdecM","avgRatingsMdecM", "likesMdecM", "raceCrossesImpMMdecM",  "attrsMdecM", "decMbaseline", "otherMdecM",
                        "goOutFreqMdecM", "goalsMdecM", "dateFreqMdecM", "careerCrossesMdecM", "ambsMdecM", "funsMdecM")
womensImprovedRatings = c("ambsWdecW", "attrsWdecW", "intelsWdecW", "funsWdecW", "sincsWdecW", 
                          "likesWdecW", "sharsWdecW", "probsMdecM", "allRatingsWdecW", "avgRatingsWdecW")

myTest[["menImprovedRatingsdecM"]] = getProbs(myTrain, myTest, mensImprovedRatings, "decM")
myTest[["womenImprovedRatingsdecW"]] = getProbs(myTrain, myTest, womensImprovedRatings, "decW")
myTrain[["menImprovedRatingsdecM"]] = getProbs(myTrain, myTrain, mensImprovedRatings, "decM")
myTrain[["womenImprovedRatingsdecW"]] = getProbs(myTrain, myTrain, womensImprovedRatings, "decW")

mensBestCombo =  c("allRatingsMdecM","avgRatingsMdecM", "likesMdecM", "raceCrossesImpMMdecM",  "attrsMdecM", "decMbaseline", "otherMdecM",
                   "goOutFreqMdecM", "goalsMdecM", "dateFreqMdecM", "careerCrossesMdecM", "ambsMdecM", "funsMdecM")
womensBestCombo = c("decWbaseline", "allRatingsWdecW", "avgRatingsWdecW", "attrsWdecW", "sharsWdecW", "intelsWdecW", "likesWdecW",
                    "funsWdecW", "careersAndFieldsWdecW", "raceCrossesImpWWdecW", "sincsWdecW", "wealthIndsWdecW", "wealthIndsWdecW", "prefsWdecW",
                    "raterDecAvgW", "decAvgOfMbyW","decWbaseline","funsWdecW")

myTest[["mensBestCombodecM"]] = getProbs(myTrain, myTest, mensBestCombo, "decM")
myTrain[["mensBestCombodecM"]] = getProbs(myTrain, myTrain, mensBestCombo, "decM")

myTest[["womensBestCombodecW"]] = getProbs(myTrain, myTest, womensBestCombo, "decW")

myTrain[["womensBestCombodecW"]] = getProbs(myTrain, myTrain, womensBestCombo, "decW")









h = function(tar,probs){
  tab = table(ifelse(probs > 0.5, 1, 0),tar)
  logloss = round(logLoss(tar, probs),2)
  print(c("logLoss:", logloss))
  totalError = round((tab[2,1] + tab[1,2])/length(tar), 2)
  print("")
  print("")
  print(c("totalError:", totalError))
  fracYesFound = round(tab[2,2]/(tab[1,2] + tab[2,2]), 2)
  print("")
  print("")
  print(c("fracYesFound", fracYesFound))
  return(tab)
}

