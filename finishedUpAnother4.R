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

myTrain = myTrain[,colSums(myTrain^2) != 0 & colSums(myTest^2) != 0 & colSums(myCV^2) != 0]
myTest = myTest[,colSums(myTrainTemp^2) != 0 & colSums(myTest^2) != 0 & colSums(myCV^2) != 0]

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

avgRatingNamesWofM = Names[grep("Avg", Names)][c(2:8)]
avgRatingNamesMofW = gsub("W_of_M", "M_of_W", avgRatingNamesWofM)

guessNames = Names[grep("Guess_", Names)]
womenOfMenGuessNames = guessNames[grep("W_of_M", guessNames)]
menOfWomenGuessNames = guessNames[grep("M_of_W", guessNames)]

goOutNames = Names[grep("go_out", Names)]
womenGoOutNames = goOutNames[grep("_W", goOutNames)]
menGoOutNames = goOutNames[grep("_M", goOutNames)]

dateNames = Names[grep("date", Names)]
womenDateNames = dateNames[grep("_W", dateNames)]
menDateNames = goOutNames[grep("_M", date_Names)]

menPhysActNames =c("sports_M", "yoga_M", "hiking_M", "exercise_M")
menArtActNames =c("art_M", "theater_M", "concerts_M", "moviesAct_M", "museumsAct_M", "music_M", "reading_Act_M")
menMiscActNames = c("diningAct_M", "diningAct_M", "clubbingAct_M", "shoppingAct_M", "tvsportsAct_M", "gamingAct_M")

womenPhysActNames = gsub("_M", "_W", menPhysActNames)
womenArtActNames = gsub("_M", "_W", menArtActNames)
womenMiscActNames = gsub("_M", "_W", womenMiscActNames)

prefNames = Names[grep("Pref", Names)]
womenPrefNames = prefNames[grep("_W", prefNames)]
menPrefNames = prefNames[grep("_M", prefNames)]

womenGoalNames = Names[grep("goal", Names)][2:7]
menGoalNames = Names[grep("goal", Names)][9:14]

menDemoNames = ("imprace_M", "income_M", "imprelig_M", "tuition_M", "age_M", "met_M", "exphappy_M")
menDemoMiscNames = ("imprace_M", "income_M", "imprelig_M", "tuition_M", "age_M", "met_M")
womenDemoMiscNames = gsub("_M", "_W", menDemoMiscNames)

womenDateNames = goOutNames[grep("_W", goOutNames)]
menGoOutNames = goOutNames[grep("_M", goOutNames)]
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

groupsHash[["miscMan"]] = careerCrossNamesOtherWoman 

genderHash = hash()
genderHash[["train"]] = myTrain
genderHash[["test"]] = myTest


baselineMofW = c("decAvg_M_of_W", "raterDecAvg_M") 
baselineWofM = c("decAvg_W_of_M", "raterDecAvg_W") 
baselineMatch = c(baselineMofW, baselineWofM)


addProbColsGender = function(genderHash){
  train = genderHash[["train"]]
  test = genderHash[["test"]]
  tars = c("dec_M","dec_W", "match")
  colNames = genderHash[["colNames"]]
  menFeatures = genderHash[["featuresMen"]]
  womenFeatures = genderHash[["featuresWomen"]]
  matchFeatures = genderHash[["featuresMatch"]]
  print(menFeatures)
  train[[colNames[1]]]  = getProbs(train, train, menFeatures, tars[1])
  test[[colNames[1]]]  = getProbs(train, test, menFeatures, tars[1])
  print(womenFeatures)
  
  train[[colNames[2]]]  = getProbs(train, train, womenFeatures, tars[2])
  test[[colNames[2]]]  = getProbs(train, test, womenFeatures, tars[2])
  print(matchFeatures)
  
  train[[colNames[3]]]  = getProbs(train, train, matchFeatures, tars[3])
  test[[colNames[3]]]  = getProbs(train, test, matchFeatures, tars[3])
  
  
  
  genderHash[["train"]] = train
  genderHash[["test"]] = test
  return(genderHash)
}


genderHash[["colNames"]] = c("baselineDecM", "baselineDecW", "baselineMatch")
genderHash[["featuresMen"]] = baselineMofW
genderHash[["featuresWomen"]] = baselineWofM
genderHash[["featuresMatch"]] = baselineMatch
genderHash = addProbColsGender(genderHash)

myTrain = genderHash[["train"]]
myTest = genderHash[["test"]]


myTrain["simpleMatch"] = myTrain["baselineDecM"]*myTrain["baselineDecW"]
myTest["simpleMatch"] = myTest["baselineDecM"]*myTest["baselineDecW"]

genderHash[["train"]] = myTrain 
genderHash[["test"]] = myTest


genderHash[["colNames"]] = c("baselineDecM", "baselineDecW", "baselineMatch")
genderHash[["featuresMen"]] = baselineMofW
genderHash[["featuresWomen"]] = baselineWofM
genderHash[["featuresMatch"]] = c(baselineMatch, "simpleMatch")
genderHash = addProbColsGender(genderHash)


womenOfMenGuessNames = guessNames[grep("W_of_M", guessNames)]
womenOfMenRatingsMerged = gsub("Guess", "Merged" ,womenOfMenGuessNames)
menOfWomenGuessNames = guessNames[grep("M_of_W", guessNames)]
menOfWomenRatingsMerged = gsub("Guess", "Merged" ,menOfWomenGuessNames)


for(i in 1:7){
  stringWM = paste(womenOfMenRatingsMerged[i],"DecM",sep="_")
  stringWW = paste(womenOfMenRatingsMerged[i],"DecW",sep="_")
  stringWmatch = paste(womenOfMenRatingsMerged[i],"Match",sep="_")
  
  stringMM = paste(menOfWomenRatingsMerged[i],"DecM",sep="_")
  stringMW = paste(menOfWomenRatingsMerged[i],"DecW",sep="_")
  stringMmatch = paste(menOfWomenRatingsMerged[i],"Match",sep="_")
  genderHash[["colNames"]] = c(stringWM, stringWW, stringMmatch )
  genderHash[["featuresMen"]] = c("baselineDecW", womenOfMenGuessNames[i], avgRatingNamesWofM[i])
  genderHash[["featuresWomen"]] = c("baselineDecW", womenOfMenGuessNames[i], avgRatingNamesWofM[i])
  genderHash[["featuresMatch"]] = c("baselineMatch", womenOfMenGuessNames[i], avgRatingNamesWofM[i])
  
  genderHash = addProbColsGender(genderHash)
  
  genderHash[["colNames"]] = c(stringMM, stringMW, stringMmatch)
  genderHash[["featuresMen"]] = c("baselineDecM", menOfWomenGuessNames[i], avgRatingNamesMofW[i])
  genderHash[["featuresWomen"]] = c("baselineDecM", menOfWomenGuessNames[i], avgRatingNamesMofW[i])
  genderHash[["featuresMatch"]] = c("baselineMatch", menOfWomenGuessNames[i], avgRatingNamesMofW[i])
  
  genderHash = addProbColsGender(genderHash)
  
}
# groupsHash[["womenOfMenRatingsMergedDecM"]] = gsub("$", "_DecM" , womenOfMenRatingsMerged)
# groupsHash[["womenOfMenRatingsMergedDecW"]] = gsub("$", "_DecW" , womenOfMenRatingsMerged)
# groupsHash[["womenOfMenRatingsMergedDecW"]] = gsub("$", "_DecW" , womenOfMenRatingsMerged)
# groupsHash[["menOfWomenRatingsMerged"]] = menOfWomenRatingsMerged
names(myTrain)
# groupsHash[["compositeMergedQualityOfW"]] = menOfWomenRatingsMerged
# groupsHash[["compositeMergedQualityOfM"]] = womenOfMenRatingsMerged

for(colName in keys(groupsHash)){
  str1 = paste(colName,"decM",sep="_")
  str2 = paste(colName,"decW",sep="_")
  str3 = paste(colName,"Match",sep="_")
  genderHash[["featuresMen"]] = c("baselineDecM", groupsHash[[colName]])
  genderHash[["featuresWomen"]] = c("baselineDecW", groupsHash[[colName]])
  genderHash[["featuresMatch"]] = c("baselineMatch", groupsHash[[colName]])
  genderHash = addProbColsGender(genderHash)
}

secondGroupsHash = hash()


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