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


merged = read.csv( '~/Desktop/speedDating/mergedCrossFeaturesAdded.csv')

n = names(merged)
avgRatings = n[grep("AvgRating", n)]
guesses = n[grep("RatingGuess",n)]
crosses = n[grep("Cross",n)]
probs = n[grep("ProbDec",n)]
prefs = n[grep("Pref",n)]
wealthInds = n[grep("WealthInd",n)]

menTraits = n[grep("M$",n)][c(11:28,30:52,54:59,61:70,73:75,77:96)]
menTraits = menTraits[!(menTraits %in% guesses)]
womenTraits = gsub("M$", "W", menTraits)

features = c(menTraits, womenTraits, crosses)


getProbs = function(train, test, features, tar, costTryType){
  bestLogLoss = 1000
  bestProbs = NA
  s=scale(train[features],center=TRUE,scale=TRUE)
  if(costTryType == "heuristic"){
    tryCosts = c(heuristicC(s))
  } else{
    tryCosts = 10^(seq(-2,0,by=0.1))
  }
  bestCost = NA
  for(co in tryCosts){
    m=LiblineaR(data=s,labels=factor(train[,tar]),type=0,cost=co,bias=TRUE,verbose=FALSE)
    s2= scale(test[features],attr(s,"scaled:center"),attr(s,"scaled:scale"))
    p=predict(m,s2,prob=TRUE)
    probs = p$probabilities[,"1"]  
    logLoss = round(logLoss(test[[tar]], probs),4)
    if(logLoss < bestLogLoss){
      bestProbs = probs
      bestLogLoss = logLoss
      bestCost = co
    }
  }
  return(bestProbs)
}


features = c("decAvgM",
                "exphappyM",
                "attrAvgRatingW",
                "likeAvgRatingW",
                "careerAcademicM_careerAcademicWCross",
                "raterDecAvgW",
                "impreligW",
                "careersManProbDecM",
                "sincAvgRatingW",
                "yogaPhysActM",
                "shoppingWealthIndW",
                "incomeWealthIndW",
                "goalsManProbDecM",
                "yogaPhysActW",
                "funPrefW",
                "diningWealthIndW",
                "avgWaveDecW",
                "goalFunNightM_goalFunNightWCross",
                "raceWhiteW",
                "tvsportsMiscActW",
                "metM",
                "hikingPhysActW",
                "fieldScienceM",
                "ageW",
                "exphappyW",
                "sincPrefW",
                "sharPrefM",
                "raceAsianM",
                "goOutW",
                "fieldBusinessW",
                "impraceM",
                "goalMeetNewW",
                "goalFunNightM_goalMeetNewWCross",
                "likeAvgRatingM",
                "hikingPhysActM",
                "theaterArtActW",
                "theaterArtActM",
                "careerFinanceM_careerAcademicWCross",
                "raceAsianM_raceWhiteWCross",
                "probAvgRatingM",
                "concertsMiscActM",
                "artArtActM",
                "date7M",
                "goalMeetNewM",
                "fieldsWomanProbDecM",
                "racesWomanProbDecM",
                "impreligM",
                "incomeWealthIndM",
                "dateM",
                "goOutM",
                "sportsPhysActM",
                "clubbingMiscActM",
                "readingArtActM",
                "shoppingWealthIndM",
                "museumsArtActW",
                "gamingMiscActW",
                "moviesArtActW",
                "musicArtActW",
                "raceAsianW",
                "goalMeetNewM_goalMeetNewWCross")

features = c(menTraits, womenTraits, crosses)

evenBetterBooster = function(df, base, tries, tar, numTimes,type){
  totalLogLoss = 0
  votes = hash()
  for(feature in tries){
    votes[[feature]] = 0
  }
  for(i in 1:numTimes){
    print(i)
    idxs = sample(1:nrow(df))
    startIdx = 1
    midIdx = floor(2*(nrow(df)/3))
    trainIdxs = idxs[startIdx:midIdx]
    testIdxs = idxs[(midIdx + 1):nrow(df)]
    train = df[trainIdxs,]
    test = df[testIdxs,]
    baseLogLoss = logLoss(test[[tar]], getProbs(train, test, base,tar,type))
    totalLogLoss = baseLogLoss + totalLogLoss
    for(feature in tries){
      newLogLoss = logLoss(test[[tar]], getProbs(train, test, c(base,feature),tar,type))
      votes[[feature]] = votes[[feature]] + baseLogLoss -  newLogLoss 
    }
  }
  print((totalLogLoss/numTimes))
  for(feature in tries){
    votes[[feature]] = (votes[[feature]]/numTimes)
  }
  return(votes)
}
features = c(menTraits, womenTraits, crosses)

menBase = c("raterDecAvgM", 
         "decAvgW", 
         "avgWaveDecM",
         "attrAvgRatingW", 
         "datesCrossProbDecM", 
         "fieldsCrossProbDecM",
         "avgWaveDecM", 
         "decAvgM",
         "goalsCrossProbDecM",
         "yogaPhysActM",
         "avgWaveDecW",
         "theaterArtActW",
         "goalsManProbDecM",
         "datesWomanProbDecM",
         "careersCrossProbDecM")
bestFeatures = evenBetterBooster(merged,base, c(newNames), "decM", 100,"heuristic")
round(sort(values(bestFeatures), decreasing=TRUE),4)
newNames = names(sort(values(bestFeatures), decreasing=TRUE))[1:8] 




womenBase = c("raterDecAvgW", "decAvgM", "attrAvgRatingM" , "fieldsCrossProbDecW", "datesCrossProbDecW",
         "goalsCrossProbDecW", "avgWaveDecW", "careersCrossProbDecW")
bestFeatures = evenBetterBooster(merged,base, c(newNames), "decW", 300,"heuristic")
10000*round(sort(values(bestFeatures), decreasing=TRUE),4)

newNames = names(sort(values(bestFeatures), decreasing=TRUE))[1:20] 

baselineDecM = c("raterDecAvgM", "decAvgW","avgWaveDecM")
baselineDecW = c("raterDecAvgW", "decAvgM","avgWaveDecW")

for(i in 1:nrow(merged)){
  print(i)
  arr = 1:nrow(merged)
  trainIdxs = arr[arr != i]
  train = merged[trainIdxs,]
  test = merged[i,]
  merged[i,"baselineProbDecM"] =  getProbs(train, test, baselineDecM, "decM", "heuristic")[1]
  merged[i,"betterProbDecM"] =  getProbs(train, test, menBase, "decM", "heuristic")[1]
  merged[i,"baselineProbDecW"] =  getProbs(train, test, baselineDecW, "decW", "heuristic")[1]
  merged[i,"betterProbDecW"] =  getProbs(train, test, womenBase, "decW", "heuristic")[1]
}


accuracyFrame = function(df, tar){
  n = names(df)
  probNames = n[grep("Guess|Conjunction|rob",n)]
  errorFrame = df[c(1),c()]
  errorFrame["logLoss"] = 0
  errorFrame["overallErrorRate"] = 0
  errorFrame["yesGuessErrorRate"] = 0
  errorFrame["noGuessErrorRate"] = 0
  for(name in probNames){
    probs = df[[name]]
    target = df[[tar]]
    errorFrame[name,] = 0
    guesses = ifelse(probs > 0.5, 1, 0)
    correctYesGuessSum = sum(ifelse(guesses == 1 & target == 1, 1,0))
    incorrectYesGuessSum = sum(ifelse(guesses == 1 & target == 0, 1,0))
    correctNoGuessSum = sum(ifelse(guesses == 0 & target == 0, 1,0))
    incorrectNoGuessSum = sum(ifelse(guesses == 0 & target == 1, 1,0))
    errorFrame[name,"logLoss"] = logLoss(target, probs)
    errorFrame[name, "overallErrorRate"] = (incorrectYesGuessSum + incorrectNoGuessSum)/length(target)
    errorFrame[name, "yesGuessErrorRate"] = (incorrectYesGuessSum)/(incorrectYesGuessSum + correctYesGuessSum)    
    errorFrame[name, "noGuessErrorRate"] = (incorrectNoGuessSum)/(incorrectNoGuessSum + correctNoGuessSum)
    errorFrame[name, "fracYesFound"] = (correctYesGuessSum)/(correctYesGuessSum +  incorrectNoGuessSum)
  }
  return(1000*round(errorFrame,3))
} 


accuracyFrame(merged[c("decM", "baselineProbDecM", "betterProbDecM")],"decM")
accuracyFrame(merged[c("decW", "baselineProbDecW", "betterProbDecW")],"decW")


merged["baselineMatchProb"] = merged["baselineProbDecM"]*merged["baselineProbDecW"]
merged["betterMatchProb"] = merged["betterProbDecM"]*merged["betterProbDecW"]
accuracyFrame(merged[c("match", "baselineMatchProb", "betterMatchProb")],"match")
matchBase = c("betterMatchProb", "goalsCrossProbMatch", "fieldsCrossProbMatch", "funAvgRatingM", "ageM")
bestFeatures = evenBetterBooster(merged,base, newNames, "match", 100,"all")
10000*round(sort(values(bestFeatures), decreasing=TRUE),4)
newNames = names(10000*round(sort(values(bestFeatures), decreasing=TRUE),4))[1:20]


for(i in 1:nrow(merged)){
  print(i)
  arr = 1:nrow(merged)
  trainIdxs = arr[arr != i]
  train = merged[trainIdxs,]
  test = merged[i,]
  merged[i,"enhancedProbMatch"] =  getProbs(train, test, matchBase, "match", "heuristic")[1]
}
accuracyFrame(merged[c("match", "baselineMatchProb", "betterMatchProb", "enhancedProbMatch")],"match")

merged["manYesWomanNo"] = merged["decM"]*(1 - merged["decW"])
merged["manNoWomanYes"] = merged["decW"]*(1 - merged["decM"])
merged["manNoWomanNo"] = (1 - merged["decM"])*(1 - merged["decW"])


merged["manYesWomanNoGuess"] = merged["betterProbDecM"]*(1 - merged["betterProbDecW"])
merged["manNoWomanYesGuess"] = merged["betterProbDecW"]*(1 - merged["betterProbDecM"])
merged["manNoWomanNoGuess"] = (1 - merged["betterProbDecM"])*(1 - merged["betterProbDecW"])

noNoBase = c("manNoWomanNoGuess", "goalsManProbDecM", "musicArtActW")
bestFeatures = evenBetterBooster(merged,noNoBase, newNames, "manNoWomanNo", 50,"heuristic")
10000*round(sort(values(bestFeatures), decreasing=TRUE),4)
newNames = names(10000*round(sort(values(bestFeatures), decreasing=TRUE),4))[1:20]


for(i in 1:nrow(merged)){
  print(i)
  arr = 1:nrow(merged)
  trainIdxs = arr[arr != i]
  train = merged[trainIdxs,]
  test = merged[i,]
  merged[i,"noNoProb"] =  getProbs(train, test, noNoBase, "manNoWomanNo", "heuristic")[1]
}

manNoWomanYesBase = c("manNoWomanYesGuess", "betterProbDecM", "betterProbDecW", "raceWhiteM_raceWhiteWCross", "readingArtActW",
                      "datesWomanProbDecW", "fieldsManProbDecM")
bestFeatures = evenBetterBooster(merged,manNoWomanYesBase, c("fieldsManProbDecM"), "manNoWomanYes", 100,"heuristic")
10000*round(sort(values(bestFeatures), decreasing=TRUE),4)
newNames = names(10000*round(sort(values(bestFeatures), decreasing=TRUE),4))[1:10]


manYesWomanNoBase = c("manYesWomanNoGuess", "impraceW")
bestFeatures = evenBetterBooster(merged,manYesWomanNoBase, c("impraceW"), "manYesWomanNo", 500,"heuristic")
10000*round(sort(values(bestFeatures), decreasing=TRUE),4)
newNames = names(10000*round(sort(values(bestFeatures), decreasing=TRUE),4))[1:20]



for(i in 1:nrow(merged)){
  print(i)
  arr = 1:nrow(merged)
  trainIdxs = arr[arr != i]
  train = merged[trainIdxs,]
  test = merged[i,]
  merged[i,"manNoWomanYesProb"] =  getProbs(train, test, manNoWomanYesBase, "manNoWomanYes", "heuristic")[1]
  
  merged[i,"manYesWomanNoProb"] =  getProbs(train, test, manYesWomanNoBase, "manYesWomanNo", "heuristic")[1]
}

accuracyFrame(merged[c("manNoWomanNo", "manNoWomanNoGuess", "noNoProb")],"manNoWomanNo")
accuracyFrame(merged[c("manYesWomanNo", "manYesWomanNoGuess", "manYesWomanNoProb")],"manYesWomanNo")
accuracyFrame(merged[c("manNoWomanYes", "manNoWomanYesGuess", "manNoWomanYesProb")],"manNoWomanYes")


finalMatchBase = c("betterMatchProb", "goalsCrossProbMatch", "fieldsCrossProbMatch", "funAvgRatingM")
finalMatchFeatures = c("manYesWomanNoGuess", 
                       "manNoWomanNoGuess", 
                   "manNoWomanYesGuess", 
                   "betterMatchProb",
                   "goalsCrossProbMatch", 
                   "fieldsCrossProbMatch", 
                   "funAvgRatingM", 
                   "ageM")
bestFeatures = evenBetterBooster(merged,finalMatchBase, finalMatchFeatures, "match", 300,"heuristic")
10000*round(sort(values(bestFeatures), decreasing=TRUE),4)
newNames = names(10000*round(sort(values(bestFeatures), decreasing=TRUE),4))[1:20]



manYesWomanNoBase = c("manYesWomanNoGuess", "impraceW")
bestFeatures = evenBetterBooster(merged,manYesWomanNoBase, c("impraceW"), "manYesWomanNo", 500,"heuristic")

veryLastMatchBase = c("raterDecAvgM","decAvgM", "decAvgW", "raterDecAvgW", "fieldsCrossProbMatch", "goalsCrossProbMatch",
                      "datesCrossProbMatch", "attrAvgRatingM")
bestFeatures = evenBetterBooster(merged,veryLastMatchBase, newNames,"match", 100, "heuristic")
10000*round(sort(values(bestFeatures), decreasing=TRUE),4)
newNames = names(10000*round(sort(values(bestFeatures), decreasing=TRUE),4))[1:10]


for(i in 1:nrow(merged)){
  print(i)
  arr = 1:nrow(merged)
  trainIdxs = arr[arr != i]
  train = merged[trainIdxs,]
  test = merged[i,]
  merged[i,"directMatchprob"] =  getProbs(train, test, veryLastMatchBase, "match", "heuristic")[1]
}

merged["anotherMatchGuess"] =  1 - merged["manYesWomanNoGuess"] - merged["manNoWomanNoGuess"] - merged["manNoWomanYesGuess"]
accuracyFrame(merged[c("match", "baselineMatchProb", "betterMatchProb", "enhancedProbMatch", "anotherMatchGuess")],"match")


bestFeatures = evenBetterBooster(merged,c("betterMatchProb"), c("anotherMatchGuess"), "match", 300,"heuristic")


for(i in 1:nrow(merged)){
  print(i)
  arr = 1:nrow(merged)
  trainIdxs = arr[arr != i]
  train = merged[trainIdxs,]
  test = merged[i,]
  merged[i,"reallyLastMatchGuess"] =  getProbs(train, test, c("anotherMatchGuess", ), "match", "heuristic")[1]
}

merged[["naiveMatchGuess"]] =  ifelse(merged[["baselineProbDecM"]] > 0.5 & merged[["baselineProbDecW"]] > 0.5, 1, 0)
merged[["naiveMatchGuessBetter"]] =  ifelse(merged[["betterProbDecM"]] > 0.5 & merged[["betterProbDecW"]] > 0.5, 1, 0)

table(ifelse(merged[["enhancedProbMatch"]] > 0.4, 1, 0),merged[["match"]])
table(merged[["naiveMatchGuess"]],merged[["match"]])
table(merged[["naiveMatchGuess"]],merged[["match"]])
table(merged[["naiveMatchGuessBetter"]],merged[["match"]])


table(ifelse(merged[["baselineProbDecM"]] > 0.5, 1, 0),merged[["decM"]])
table(ifelse(merged[["betterProbDecM"]] > 0.5, 1, 0),merged[["decM"]])


table(ifelse(merged[["baselineProbDecW"]] > 0.5, 1, 0),merged[["decW"]])
table(ifelse(merged[["betterProbDecW"]] > 0.5, 1, 0),merged[["decW"]])

table(merged[["baselineProbDecM"]],merged[["decM"]])
table(merged[["baselineProbDecW"]],merged[["decW"]])


table(merged[["betterProbDecM"]],merged[["decM"]])
table(merged[["betterProbDecW"]],merged[["decW"]])
table(ifelse(merged[["baselineProbDecM"]] > 0.5 & merged[["baselineProbDecW"]] > 0.5, 1, 0),merged[["match"]])
table(ifelse(merged[["betterProbDecM"]] > 0.5 & merged[["betterProbDecW"]] > 0.5, 1, 0),merged[["match"]])


table(ifelse(merged[["baselineProbDecW"]] > 0.5, 1, 0),merged[["decW"]])

newFeatures = c(features,guesses)
newFeatures2 = newFeatures[c(1:8,10:54,58:80,82:126)]
rfForTrain = randomForest(y=as.factor(merged[,"match"]), x=merged[c(newFeatures2)], importance=TRUE, ntree=200)
rfForTrain = randomForest(y=as.factor(merged[,"decW"]), x=merged[c(newFeatures2)], importance=TRUE, ntree=200)
rfForTrain = randomForest(y=as.factor(merged[,"decW"]), x=merged[c(features, guesses)], importance=TRUE, ntree=5000, mtry=length(newFeatures))

rfForTrain = randomForest(y=as.factor(merged[,"decM"]), x=merged[c(features, guesses)], importance=TRUE, ntree=5000, mtry=length(newFeatures))
rfForTrain = randomForest(y=as.factor(merged[,"match"]), x=merged[c(features, guesses)], importance=TRUE, ntree=5000, mtry=length(newFeatures))
