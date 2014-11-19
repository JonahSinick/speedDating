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
corFrame = myTrain[c(1),c()]
# manTraitsDecManNames = c("raterDecAvgM", "goOutM", "matchAvgM","funAvgRatingbyWofM")
# womanTraitsDecManNames = c("decAvgbyMofW", "attrAvgRatingbyMofW", "likeRatingGuessbyMofW", "raceAsianM_raceWhiteW",
#                            "careersWomanTraitDecM", "fieldsWomanTraitDecM","goalsWomanTraitDecM", "racesWomanTraitDecM", )
# 
# manTraitsDecWomanNames = c("attrAvgRatingbyWofM", "funAvgRatingbyWofM", "attrRatingGuessbyWofM", "likeRatingGuessbyWofM",
#                            "datesManTraitDecW", "decAvgbyWofM", "probAvgRatingbyWofM", "fieldsManTraitDecW", "careersManTraitDecW")
# womanTraitsDecWomanNames = c("raterDecAvgW", "probRatingbyMofW", "datesWomanTraitDecW", "attrRatingbyMofW")
# 
# crossTraitsDecWomanNames = c("careersCrossTraitDecW", "datesCrossTraitDecW", "fieldsCrossTraitDecW", "racesCrossTraitDecW",
#                              "raceAsianM_raceWhiteW", "raceWhiteM_raceWhiteW")
# 
# crossTraitsDecManNames = c("careersCrossTraitDecM", "datesCrossTraitDecW", "fieldsCrossTraitDecM", "racesCrossTraitDecM",
#                              "raceAsianM_raceWhiteW", "raceWhiteM_raceWhiteW")
for(name in n[10:337]){
  if(name %in% n[grep("M$",n)]){
    myTrain[[paste(name,"DecM")]] = getProbs(myTrain, myTrain, c(name, manTraitsDecManNames), "decM")
#     myTrain[[paste(name,"DecW")]] = getProbs(myTrain, myTrain, c(name, manTraitsDecWomanNames), "decW")
  }
  if(name %in% n[grep("W$",n)]){
#     myTrain[[paste(name,"DecW")]] = getProbs(myTrain, myTrain, c(name, womanTraitsDecWomanNames), "decW")
#     myTrain[[paste(name,"DecM")]] = getProbs(myTrain, myTrain, c(name, womanTraitsDecManNames), "decM")
  }
}
n = names(myTrain)
M_decM = n[grep("M DecM$",n)]
round(cor(myTrain[M_decM], myTrain["decM"]), 2) 
# 
# W_decM = n[grep("W DecM$",n)]
# 100*(round(cor(myTrain[W_decM], myTrain["decM"]), 2) - 0.41)

W_decW = n[grep("W DecW$",n)]
100*(round(cor(myTrain[W_decW], myTrain["decW"]), 2) - 0.44)

# M_decW = n[grep("M DecW$",n)]
# 100*(round(cor(myTrain[M_decW], myTrain["decW"]), 2) - 0.33)




for(k in keys(groupsHash)){
    test[[colnameManTraitsDecMan]] = getProbs(train, test, c(featuresM, "raterDecAvgM"), "decM")
    test[[colnameWomanTraitsDecWoman]] = getProbs(train, test, c(featuresW, "raterDecAvgW"), "decW")
    test[[colnameManTraitsDecWoman]] = getProbs(train, test, c(featuresM, "decAvgbyWofM"), "decW")
    test[[colnameWomanTraitsDecMan]] = getProbs(train, test,  c(featuresW, "decAvgbyMofW"), "decM")
  }
  return(test)
}

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

guesses = n[grep("Guess", n)]
womensGuesses = guesses[1:8]
myTest = addProbCols(myTrain,myTest,groupsHash)
myTrain = addProbCols(myTrain,myTrain,groupsHash)

myTrain[["simpleDecMGuess"]] = getProbs(myTrain, myTrain,  c("raterDecAvgM", "decAvgbyMofW"), "decM")
myTest[["simpleDecMGuess"]] = getProbs(myTrain, myTest,  c("raterDecAvgM", "decAvgbyMofW"), "decM")


manTraitsDecWomanNames = c("attrAvgRatingbyWofM", "funAvgRatingbyWofM", "attrRatingGuessbyWofM", "likeRatingGuessbyWofM",
                           "datesManTraitDecW", "decAvgbyWofM", "probAvgRatingbyWofM", "fieldsManTraitDecW", "careersManTraitDecW")
womanTraitsDecWomanNames = c("raterDecAvgW", "probRatingbyMofW", "datesWomanTraitDecW")

crossTraitsDecWomanNames = c("careersCrossTraitDecW", "datesCrossTraitDecW", "fieldsCrossTraitDecW", "racesCrossTraitDecW",
                             "raceAsianM_raceWhiteW", "raceWhiteM_raceWhiteW")

makeErrorFrame(myTest[c("decM", "decAvgbyMofW")],"decM")

manTraitsDecManNames = c("goOutM", "matchAvgM","funAvgRatingbyWofM")
womanTraitsDecManNames = c("attrAvgRatingbyMofW", "likeRatingGuessbyMofW",
                           "careersWomanTraitDecM", "fieldsWomanTraitDecM","goalsWomanTraitDecM", "racesWomanTraitDecM" )


crossTraitsDecManNames = c("careersCrossTraitDecM", "datesCrossTraitDecM", "fieldsCrossTraitDecM", "racesCrossTraitDecM",
                           "raceAsianM_raceWhiteW")

features = c(womanTraitsDecManNames)
# avgs = c("decAvgbyMofW", "raterDecAvgM")

features = c("decAvgbyMofW", "raterDecAvgM")
rf_fit <- randomForest(y=as.factor(myTrain[,"decM"]), x=myTrain[features], importance=TRUE, mtry = length(features), ntree=5000)
p = predict(rf_fit, myTest[c("decM", features)])
t = table(p,myTest[["decM"]])
1 - (t[1,1] + t[2,2])/nrow(myTest)


myTrain[["simpleDecMGuess"]] = getProbs(myTrain, myTrain,  c("raterDecAvgM", "decAvgbyMofW"), "decM")
