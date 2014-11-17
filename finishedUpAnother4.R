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
  tars = c("dec_M","dec_W", "match")
  colNames = genderHash[["colNames"]]
  menFeatures = genderHash[["featuresMen"]]
  womenFeatures = genderHash[["featuresWomen"]]
  matchFeatures = genderHash[["featuresMatch"]]
  
  train[[colNames[1]]]  = getProbs(train, train, menFeatures, tars[1])
  test[[colNames[1]]]  = getProbs(train, test, menFeatures, tars[1])
  train[[colNames[2]]]  = getProbs(train, train, womenFeatures, tars[2])
  test[[colNames[2]]]  = getProbs(train, test, womenFeatures, tars[2])
  train[[colNames[3]]]  = getProbs(train, train, match, tars[3])
  test[[colNames[3]]]  = getProbs(train, test, match, tars[3])
  
  genderHash[["train"]] = train
  genderHash[["test"]] = test
  return(genderHash)
}



Names = names(myTrain)

avgRatingNamesWofM = Names[grep("Avg", Names)][c(2:8)]
avgRatingNamesMofW = gsub("W_of_M", "M_of_W", avgRatingNamesWofM)

guessNames = Names[grep("Guess_", Names)]
womenOfMenGuessNames = guessNames[grep("W_of_M", guessNames)]
menOfWomenGuessNames = guessNames[grep("M_of_W", guessNames)]

crossNames = Names[grep("Cross", Names)]
fieldCrossNames = crossNames[grep("field", crossNames)]
careerCrossNames = crossNames[grep("career", crossNames)]

fieldNamesW = Names[grep("field", Names)][1:11]
fieldNamesM = Names[grep("field", Names)][12:20]
careerNamesW = Names[grep("career", Names)][1:10]
careerNamesM = Names[grep("career", Names)][11:17]

raceNames = Names[grep("race", Names)]
menRaces = raceNames[10:13]
womenRaces = raceNames[4:7]
raceCrossNames = raceNames[grep("Cross", raceNames)]

asianManRaceCrossNames = raceCrossNames[grep("raceAsian_M", raceCrossNames)]
whiteManRaceCrossNames = raceCrossNames[grep("raceWhite_M", raceCrossNames)]
latinoManRaceCrossNames = raceCrossNames[grep("raceLatino_M", raceCrossNames)]
blackManRaceCrossNames = raceCrossNames[grep("raceBlack_M", raceCrossNames)]
asianWomanRaceCrossNames = raceCrossNames[grep("raceAsian_W", raceCrossNames)]
whiteWomanRaceCrossNames = raceCrossNames[grep("raceWhite_W", raceCrossNames)]
latinoWomanRaceCrossNames = raceCrossNames[grep("raceLatino_W", raceCrossNames)]
blackWomanRaceCrossNames = raceCrossNames[grep("raceBlack_W", raceCrossNames)]

fieldCrossNamesBusinessMan = fieldCrossNames[grep("fieldBusiness_M", fieldCrossNames)]
fieldCrossNamesLawMan = fieldCrossNames[grep("fieldLaw_M", fieldCrossNames)]
fieldCrossNamesEnginMan = fieldCrossNames[grep("fieldEngin_M", fieldCrossNames)]
fieldCrossNamesScienceMan = fieldCrossNames[grep("fieldScience_M", fieldCrossNames)]
fieldCrossNamesPoliSciMan = fieldCrossNames[grep("PoliSci_M", fieldCrossNames)]

fieldCrossNamesBusinessWoman = fieldCrossNames[grep("fieldBusiness_W", fieldCrossNames)]
fieldCrossNamesLawWoman = fieldCrossNames[grep("fieldLaw_W", fieldCrossNames)]
fieldCrossNamesEnginWoman = fieldCrossNames[grep("fieldEngin_W", fieldCrossNames)]
fieldCrossNamesScienceWoman = fieldCrossNames[grep("fieldScience_W", fieldCrossNames)]
fieldCrossNamesPoliSciWoman = fieldCrossNames[grep("PoliSci_W", fieldCrossNames)]
fieldCrossNamesSocialWorkWoman = fieldCrossNames[grep("SocialWork_W", fieldCrossNames)]
fieldCrossNamesOtherWoman = fieldCrossNames[grep("Academia_W|English_W|HistReligPhil_W|SocialSci_W", fieldCrossNames)]

careerCrossNamesFinanceMan = careerCrossNames[grep("careerFinance_M", careerCrossNames)]
careerCrossNamesCreativeMan = careerCrossNames[grep("careerCreative_M", careerCrossNames)]
careerCrossNamesLawMan = careerCrossNames[grep("careerLaw_M", careerCrossNames)]
careerCrossNamesOtherMan = careerCrossNames[grep("careerMedicine_M|Undecided_M", careerCrossNames)]

careerCrossNamesFinanceWoman = careerCrossNames[grep("careerFinance_W", careerCrossNames)]
careerCrossNamesCreativeWoman = careerCrossNames[grep("careerCreative_W", careerCrossNames)]
careerCrossNamesLawWoman = careerCrossNames[grep("careerLaw_W", careerCrossNames)]
careerCrossNamesAcademicWoman = careerCrossNames[grep("careerAcademic_W", careerCrossNames)]
careerCrossNamesOtherWoman = careerCrossNames[grep("international_W|Psychology_W|SocialWork_W|Undecided_W", careerCrossNames)]

groupsHash = hash()

groupsHash[["avgRatingNamesWofM"]] = avgRatingNamesWofM 
groupsHash[["avgRatingNamesMofW"]] = avgRatingNamesMofW 

groupsHash[["guessNames"]] = guessNames 
groupsHash[["womenOfMenGuessNames"]] = womenOfMenGuessNames 
groupsHash[["menOfWomenGuessNames"]] = menOfWomenGuessNames 

groupsHash[["crossNames"]] = crossNames 
groupsHash[["fieldCrossNames"]] = fieldCrossNames 
groupsHash[["careerCrossNames"]] = careerCrossNames 

groupsHash[["fieldNamesW"]] = fieldNamesW 
groupsHash[["fieldNamesM"]] = fieldNamesM 
groupsHash[["careerNamesW"]] = careerNamesW 
groupsHash[["careerNamesM"]] = careerNamesM 

groupsHash[["raceNames"]] = raceNames 
groupsHash[["menRaces"]] = menRaces 
groupsHash[["womenRaces"]] = womenRaces 
groupsHash[["raceCrossNames"]] = raceCrossNames 

groupsHash[["asianManRaceCrossNames"]] = asianManRaceCrossNames 
groupsHash[["whiteManRaceCrossNames"]] = whiteManRaceCrossNames 
groupsHash[["latinoManRaceCrossNames"]] = latinoManRaceCrossNames 
groupsHash[["blackManRaceCrossNames"]] = blackManRaceCrossNames 
groupsHash[["asianWomanRaceCrossNames"]] = asianWomanRaceCrossNames 
groupsHash[["whiteWomanRaceCrossNames"]] = whiteWomanRaceCrossNames 
groupsHash[["latinoWomanRaceCrossNames"]] = latinoWomanRaceCrossNames 
groupsHash[["blackWomanRaceCrossNames"]] = blackWomanRaceCrossNames 

groupsHash[["fieldCrossNamesBusinessMan"]] = fieldCrossNamesBusinessMan 
groupsHash[["fieldCrossNamesLawMan"]] = fieldCrossNamesLawMan 
groupsHash[["fieldCrossNamesEnginMan"]] = fieldCrossNamesEnginMan 
groupsHash[["fieldCrossNamesScienceMan"]] = fieldCrossNamesScienceMan 
groupsHash[["fieldCrossNamesPoliSciMan"]] = fieldCrossNamesPoliSciMan 

groupsHash[["fieldCrossNamesBusinessWoman"]] = fieldCrossNamesBusinessWoman 
groupsHash[["fieldCrossNamesLawWoman"]] = fieldCrossNamesLawWoman 
groupsHash[["fieldCrossNamesEnginWoman"]] = fieldCrossNamesEnginWoman 
groupsHash[["fieldCrossNamesScienceWoman"]] = fieldCrossNamesScienceWoman 
groupsHash[["fieldCrossNamesPoliSciWoman"]] = fieldCrossNamesPoliSciWoman 
groupsHash[["fieldCrossNamesSocialWorkWoman"]] = fieldCrossNamesSocialWorkWoman 
groupsHash[["fieldCrossNamesOtherWoman"]] = fieldCrossNamesOtherWoman 

groupsHash[["careerCrossNamesFinanceMan"]] = careerCrossNamesFinanceMan 
groupsHash[["careerCrossNamesCreativeMan"]] = careerCrossNamesCreativeMan 
groupsHash[["careerCrossNamesLawMan"]] = careerCrossNamesLawMan 
groupsHash[["careerCrossNamesOtherMan"]] = careerCrossNamesOtherMan 

groupsHash[["careerCrossNamesFinanceWoman"]] = careerCrossNamesFinanceWoman 
groupsHash[["careerCrossNamesCreativeWoman"]] = careerCrossNamesCreativeWoman 
groupsHash[["careerCrossNamesLawWoman"]] = careerCrossNamesLawWoman 
groupsHash[["careerCrossNamesAcademicWoman"]] = careerCrossNamesAcademicWoman 
groupsHash[["careerCrossNamesOtherWoman"]] = careerCrossNamesOtherWoman 


genderHash = hash()
genderHash[["train"]] = myTrain
genderHash[["test"]] = myTest


baselineMofW = c("decAvg_M_of_W", "raterDecAvg_M") 
baselineWofM = c("decAvg_W_of_M", "raterDecAvg_W") 
baselineMatch = c(baselineMofW, baselineWofM)



avgRatingNamesMofW = gsub("W_of_M", "M_of_W", avgRatingNamesWofM)

womenOfMenGuessNames = guessNames[grep("W_of_M", guessNames)]
womenOfMenRatingsMerged = gsub("Guess", "Merged" ,womenOfMenGuessNames)
menOfWomenGuessNames = guessNames[grep("M_of_W", guessNames)]
menOfWomenRatingsMerged = gsub("Guess", "Merged" ,menOfWomenGuessNames)

for(i in 1:7){
  stringWM = paste(womenOfMenRatingsMerged[i],"dec_M",sep="_")
  stringWW = paste(womenOfMenRatingsMerged[i],"dec_W",sep="_")
  stringMM = paste(menOfWomenRatingsMerged[i],"dec_M",sep="_")
  stringMW = paste(menOfWomenRatingsMerged[i],"dec_W",sep="_")
  genderHash[["colNames"]] = c(stringWM, stringWW)
  genderHash[["featuresMen"]] = c(groupsHash[["baselineMofW"]], womenOfMenGuessNames[i], avgRatingNamesWofM[i])
  genderHash[["featuresWomen"]] = c(groupsHash[["baselineWofM"]], womenOfMenGuessNames[i], avgRatingNamesWofM[i])
  genderHash = addProbColsGender(genderHash)
  
  genderHash[["colNames"]] = c(stringMM, stringMW)
  genderHash[["featuresMen"]] = c(groupsHash[["baselineMofW"]], menOfWomenGuessNames[i], avgRatingNamesMofW[i])
  genderHash[["featuresWomen"]] = c(groupsHash[["baselineWofM"]], menOfWomenGuessNames[i], avgRatingNamesMofW[i])
  genderHash = addProbColsGender(genderHash)
  
}


genderHash[["colNames"]] = c("compositeAvgQualityOfW_dec_M", "compositeAvgQualityOf_W_dec_W")
genderHash[["featuresMen"]] = c(groupsHash[["baselineMofW"]], avgRatingNamesMofW)
genderHash[["featuresWomen"]] = c(groupsHash[["baselineWofM"]], avgRatingNamesMofW)
genderHash = addProbColsGender(genderHash)

genderHash[["colNames"]] = c("compositeAvgQualityOfM_dec_M", "compositeAvgQualityOf_M_dec_W")
genderHash[["featuresMen"]] = c(groupsHash[["baselineMofW"]], avgRatingNamesWofM)
genderHash[["featuresWomen"]] = c(groupsHash[["baselineWofM"]], avgRatingNamesWofM)
genderHash = addProbColsGender(genderHash)



myTrain = genderHash[["train"]]
myTest = genderHash[["test"]]


h = function(tar,probs){
  tab = table(ifelse(probs > 0.5, 1, 0),tar)
  logloss = round(logLoss(tar, probs),2)
  print(c("logLoss:", logloss))
  totalError = round((tab[2,1] + tab[1,2])/length(tar), 2)
  print(c("totalError:", totalError))
  fracYesFound = round(tab[2,2]/(tab[1,2] + tab[2,2]), 2)
  print(c("fracYesFound", fracYesFound))
  return(tab)
}
n = names(myTrain)
mergedRatings = n[grep("mergedRatingWofM",n)]
mergedRatings = mergedRatings[grep("decW",mergedRatings)]

features = c("mergedRatingWofM_1_decW", "mergedRatingWofM_4_decW", "mergedRatingWofM_7_decW")
genderHash[["colNames"]] = c("attrLikeFunWofMDecM", "attrLikeFunWofMDecW")
genderHash[["featuresMen"]] = features
genderHash[["featuresWomen"]] = features 
genderHash = addProbColsGender(genderHash)


features = c("mergedRatingWofM_2_decW", "mergedRatingWofM_3_decW", "mergedRatingWofM_5_decW")
genderHash[["colNames"]] = c("sincIntelAmbWofMDecM", "sincIntelAmbWofMDecW")
genderHash[["featuresMen"]] = features 
genderHash[["featuresWomen"]] = features 
genderHash = addProbColsGender(genderHash)

features = c("sincIntelAmbWofMDecW", "attrLikeFunWofMDecW", "mergedRatingWofM_6_decW")
genderHash[["colNames"]] = c("manQualityDecM", "manQualityDecW")
genderHash[["featuresMen"]] = features 
genderHash[["featuresWomen"]] = features 
genderHash = addProbColsGender(genderHash)


myTrain = genderHash[["train"]]
myTest = genderHash[["test"]]


for(i in 1:1){
  print("NEXT ROUND")
  features0 = c("decAvg_W_of_M", "raterDecAvg_W")
  features1 = c("decAvg_W_of_M", "raterDecAvg_W", avgNames[1:7])
  features2 = c("decAvg_W_of_M", "raterDecAvg_W", guessNames[1:7])
  features3 = c("decAvg_W_of_M", "raterDecAvg_W", mergedRatings[1:7])
  features4 = c("decAvg_W_of_M", "raterDecAvg_W", "sincIntelAmbWofMDecW", "attrLikeFunWofMDecW", "mergedRatingWofM_6_decW")
  features5 = c("decAvg_W_of_M", "raterDecAvg_W", "manQualityDecW")
  

  
  
  genderHash[["colNames"]] = c("decPredWomanComposite0", "decPredWomanComposite0")
  genderHash[["featuresMen"]] = features0
  genderHash[["featuresWomen"]] = features0
  genderHash = addProbColsGender(genderHash)
  
  

  
  genderHash[["colNames"]] = c("decPredWomanComposite1", "decPredWomanComposite1")
  genderHash[["featuresMen"]] = features1
  genderHash[["featuresWomen"]] = features1
  genderHash = addProbColsGender(genderHash)
  
  
  genderHash[["colNames"]] = c("decPredWomanComposite2", "decPredWomanComposite2")
  genderHash[["featuresMen"]] = features2
  genderHash[["featuresWomen"]] = features2
  genderHash = addProbColsGender(genderHash)
  
  

  
  genderHash[["colNames"]] = c("decPredWomanComposite3", "decPredWomanComposite3")
  genderHash[["featuresMen"]] = features3
  genderHash[["featuresWomen"]] = features3 
  genderHash = addProbColsGender(genderHash)
  
  
  
  print(features4)
  probs = getProbs(myTrain, myTest, features4, "dec_W")
  h(myTest[["dec_W"]], probs)
  
  
  genderHash[["colNames"]] = c("decPredWomanComposite4", "decPredWomanComposite4")
  genderHash[["featuresMen"]] = features4
  genderHash[["featuresWomen"]] = features4 
  genderHash = addProbColsGender(genderHash)
  

  genderHash[["colNames"]] = c("decPredWomanComposite5", "decPredWomanComposite5")
  genderHash[["featuresMen"]] = features5
  genderHash[["featuresWomen"]] = features5
  genderHash = addProbColsGender(genderHash)
  
  
}
"decPredWomanComposite0", "decPredWomanComposite1", "decPredWomanComposite2", "decPredWomanComposite3", "decPredWomanComposite4", "decPredWomanComposite5", 
c("decAvg_W_of_M", "raterDecAvg_W"), 
myTrain = genderHash[["train"]]
myTest = genderHash[["test"]]
features =   c(c("decPredWomanComposite5", "decPredWomanComposite0", guessNames, avgNames))
probs = getProbs(myTrain, myTest, features, "dec_W")
h(myTest[["dec_W"]], probs)
