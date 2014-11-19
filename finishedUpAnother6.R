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

myTrain = myTrain[,(colSums(myTrain^2) > 1) & (colSums(myTest^2) != 1)]
myTest = myTest[,(colSums(myTrainTemp^2)) > 1 & (colSums(myTest^2) != 1)]

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
groupsHash[["races"]] = n[grep("race", n)][c(2:5,7:10)]
groupsHash[["raceCrosses"]] = n[grep("mrace", n)]
groupsHash[["fields"]] = n[grep("field", n)][1:30]
groupsHash[["fieldCrosses"]] = n[grep("MField", n)]
groupsHash[["careers"]] = n[grep("career", n)][1:24]
groupsHash[["careerCrosses"]] = n[grep("Mcareer", n)]
groupsHash[["fieldsAndCareers"]] = c(groupsHash[["fields"]], groupsHash[["careers"]])
groupsHash[["fieldAndCareerCrosses"]] = c(groupsHash[["careerCrosses"]], groupsHash[["fieldCrosses"]])
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
groupsHash[["raterDecs"]] = c("raterDecAvgW", "raterDecAvgM")
groupsHash[["rateeDecs"]] = c("decAvgOfWbyM", "decAvgOfMbyW")
groupsHash[["decs"]] = c(groupsHash[["raterDecs"]], groupsHash[["rateeDecs"]])
genderHash = hash()
genderHash[["train"]] = myTrain
genderHash[["test"]] = myTest

oldColNum = length(names(myTest))
addProbCols = function(train, test, groupsHash){
  menBase = c("raterDecAvgM", "decAvgOfWbyM")
  womenBase = c("raterDecAvgW", "decAvgOfMbyW")
  test[["decMbaseline"]] = getProbs(train, test, menBase, "decM")
  test[["decWbaseline"]] = getProbs(train, test, womenBase, "decW")
  for(k in keys(groupsHash)){
    print(k)
    features = groupsHash[[k]]
    featuresM = features[grep("M$",features)]
    featuresW = features[grep("W$",features)]
    colName = k
    colNameMM = paste(colName,"MdecM",sep="")
    colNameMW= paste(colName,"MdecW",sep="")
    colNameWM = paste(colName,"WdecM",sep="")
    colNameWW= paste(colName,"WdecW",sep="")
    test[[colNameMM]] = getProbs(train, test, c(featuresM, menBase), "decM")
    test[[colNameMW]] = getProbs(train, test, c(featuresM, womenBase), "decW")
    test[[colNameWM]] = getProbs(train, test, c(featuresW, menBase), "decM")
    test[[colNameWW]] = getProbs(train, test,  c(featuresW, womenBase), "decW")
  }
  return(test)
}

myTest = addProbCols(myTrain,myTest,groupsHash)
myTrain = addProbCols(myTrain,myTrain,groupsHash)


justForOwnGender = function(train, test, groupsHash){

  
  for(k in keys(groupsHash)){
    features = groupsHash[[k]]
    featuresM = features[grep("decM",features)]
    featuresW = features[grep("decW",features)]
    colName = k
    colNameM = paste(colName,"decM",sep="")
    colNameW = paste(colName,"decW",sep="")

    test[[colNameM]] = getProbs(train, test, c(featuresM), "decM")
    test[[colNameW]] = getProbs(train, test,  c(featuresW), "decW")
  }
  return(test)
}

newGroupsHash = hash()
mensImprovedRatings = c("ambsMdecM", "attrsMdecM", "intelsMdecM", "funsMdecM", "sincsMdecM", 
                        "likesMdecM", "sharsMdecM", "probsWdecM")
womensImprovedRatings = c("ambsWdecW", "attrsWdecW", "intelsWdecW", "funsWdecW", "sincsWdecW", 
                          "likesWdecW", "sharsWdecW", "probsMdecM")
newGroupsHash[["overallRating"]] = c(mensImprovedRatings, womensImprovedRatings, groupsHash[["avgRating"]])
goOuts = c("goOutFreqMdecM", "goOutFreqWdecM", "goOutFreqMdecM", "goOutFreqWdecM")
dates = c("dateFreqMdecM", "dateFreqWdecM", "dateFreqMdecW", "dateFreqWdecM")
goals = c("goalsMdecW", "goalsWdecW", "goalsWdecM", "goalsMdecM")
newGroupsHash[["dateGoalgoOut"]] = c(goOuts, dates, goals)


physActs = c("physActsMdecW", "physActsWdecM")
artActs = c("artActsMdecW", "artActsWdecM")
miscActs = c("miscActsMdecW", "miscActsWdecM")
wealthInds = c("wealthIndsMdecW", "wealthIndsWdecM")
dates = c("dateFreqWdecM", "dateFreqMdecW")
goals = c("goalsMdecM", "goalsWdecW")

newGroupsHash[["activities"]] = c(physActs, artActs, miscActs, wealthInds)


features = ("allRatingsWdecW", "dateFreqWdecW", "avgRatingsWdecW", "sameRace", "impraceW", "likesWdecW", "attrsWdecW", "goalsWdecW", "funsWdecW")
myTrain[["womanDecGuess"]] = 

myTest = justForOwnGender(myTrain,myTest,newGroupsHash)
myTrain = justForOwnGender(myTrain,myTrain,newGroupsHash)
n = c(names(myTest))
menNames = n[grep("decM",n)]
womenNames = n[grep("decW",n)]
menFrame = myTest[menNames]
womenFrame = myTest[womenNames]
menErrorFrame = makeErrorFrame(menFrame, "decM")
womenErrorFrame = makeErrorFrame(womenFrame, "decW")

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
    errorFrame[name, "totalError"] = round((tab[1,2] + tab[2,1])/length(tar), 3)
    errorFrame[name,"positivesFound"] = round(tab[2,2]/(tab[1,2] + tab[2,2]), 3)
  }
  return(errorFrame)
}



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

