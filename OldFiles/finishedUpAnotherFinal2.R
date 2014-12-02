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

myTrain = myTrain[,(colSums(myTrain^2) >= 20) & (colSums(myTest^2) >=20)]
myTest = myTest[,(colSums(myTrainTemp^2)) >= 20 & (colSums(myTest^2) >= 20)]

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
groupsHash[["physActs"]] = n[grep("PhysAct", n)]
groupsHash[["artActs"]] = n[grep("ArtAct", n)]
groupsHash[["wealthInds"]] = n[grep("WealthInd", n)]
groupsHash[["miscActs"]] = n[grep("miscAct", n)]
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



addProbCols = function(train, test, groupsHash){
  
  test[["ManTraitsDecMBaseline"]] = getProbs(train, test, c("raterDecAvgM"), "decM")
  
  test[["WomanTraitsDecWBaseline"]] = getProbs(train, test, c("raterDecAvgW"), "decW")
  
  test[["WomanTraitsDecMBaseline"]] = getProbs(train, test, c("decAvgbyWofM"), "decM")
  
  test[["ManTraitsDecWBaseline"]] = getProbs(train, test, c("decAvgbyMofW"), "decW")
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

    test[[colnameManTraitsDecMan]] = getProbs(train, test, c(featuresM, "raterDecAvgM"), "decM")
    test[[colnameWomanTraitsDecWoman]] = getProbs(train, test, c(featuresW, "raterDecAvgW"), "decW")
    test[[colnameManTraitsDecWoman]] = getProbs(train, test, c(featuresM, "decAvgbyWofM"), "decW")
    test[[colnameWomanTraitsDecMan]] = getProbs(train, test,  c(featuresW, "decAvgbyMofW"), "decM")
  }
  return(test)
}

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

myTest = addProbCols(myTrain,myTest,groupsHash)
myTrain = addProbCols(myTrain,myTrain,groupsHash)
n = names(myTest)
menTraitsDecM = n[grep("ManTraitDecM$|ManTraitsDecM$",n)]
womenTraitsDecM = n[grep("WomanTraitDecM$|WomanTraitsDecM$",n)]
menTraitsDecW = n[grep("ManTraitDecW$|ManTraitsDecW$",n)]
womenTraitsDecW = n[grep("WomanTraitDecW$|WomanTraitsDecW$",n)]
crossTraitsDecM = n[grep("CrossTraitDecM$",n)]
crossTraitsDecW = n[grep("CrossTraitDecW$",n)]

womenTraitsDecW = n[grep("WomanTraitDecW$|WomanTraitsDecW$",n)]
makeErrorFrame(myTest[c("decM", "WomanTraitsDecMBaseline", womenTraitsDecM)], "decM")

myTest[["menTraitsDecMGuess"]] = getProbs(myTrain, myTest,  menTraitsDecM, "decM")
myTrain[["menTraitsDecMGuess"]] = getProbs(myTrain, myTrain,  menTraitsDecM, "decM")

myTest[["womenTraitsDecMGuess"]] = getProbs(myTrain, myTest,  womenTraitsDecM, "decM")
myTrain[["womenTraitsDecMGuess"]] = getProbs(myTrain, myTrain,  womenTraitsDecM, "decM")

myTest[["simpleDecMGuess"]] = getProbs(myTrain, myTest,  c("raterDecAvgM", "decAvgbyMofW"), "decM")
myTrain[["simpleDecMGuess"]] = getProbs(myTrain, myTrain,  c("raterDecAvgM", "decAvgbyMofW"), "decM")


allTogetherDecM = c("allRatingsWomanTraitsDecM", "raterDecAvgM", "decAvgbyMofW")
myTest[["decMGuess"]] = getProbs(myTrain, myTest, allTogetherDecM, "decM")


decMTraits = c("decMGuess")
makeErrorFrame(myTest[c("decM","simpleDecMGuess", decMTraits)], "decM")

