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
menTraits = n[grep("M$",n)][3:98]
womenTraits = n[grep("W$",n)][3:98]
waves = unique(merged[["wave"]])
waves1 = merged[merged["wave"] %in% c(1,4,7,10,13,16),]
waves2 = merged[merged["wave"] %in% c(2,5,8,11,14,17),]
waves3 = merged[merged["wave"] %in% c(2,5,8,11,14,17),]
train2 = merged[c(2,4,6,8,10)]
baselineDecM = c("raterDecAvgM", "decAvgW","avgWaveDecM")
baselineDecW = c("raterDecAvgW", "decAvgM","avgWaveDecW")

probs = c("baselineMen","baselineWomen", "baselineMatch", "guessMen", "guessWomen", "guessMatch")
merged[probs] = 0

getProbs = function(train, test, features, tar){
  tryCosts = 10^(c(-1))
  bestLogLoss = 1000
  bestProbs = NA
  s=scale(train[features],center=TRUE,scale=TRUE)
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

newestDecMenTraits = c(baselineDecM, "attrAvgRatingW", "fieldsCrossProbTraitDecM",
                       "goalsCrossProbTraitDecM", "datesCrossProbTraitDecM")
newestDecWomenTraits = c(baselineDecW, "attrAvgRatingM", "raceAsianM_raceWhiteWCross",
                         "fieldsCrossProbDecW", "datesCrossProbDecW", "raterDecAvgM",
                         "raceWhiteM_raceWhiteWCross", "raceAsianM_raceAsianWCross")
train = merged[merged["wave"] != 1,]
test = merged[merged["wave"] == i,]
# 
# 
# for(i in waves){
#   trainWaves = waves[!(waves==i)] 
#   train = merged[merged["wave"] != i,]
#   test = merged[merged["wave"] == i,]
#   merged[merged["wave"] == i,][["baselineMen"]] =  getProbs(train, test, baselineDecM, "decM")
#   merged[merged["wave"] == i,][["baselineWomen"]] =  getProbs(train, test, baselineDecW, "decW")
#   merged[merged["wave"] == i,][["guessMen"]] =  getProbs(train, test, newestDecMenTraits, "decM")
#   merged[merged["wave"] == i,][["guessWomen"]] =  getProbs(train, test, newestDecWomenTraits, "decW")
# }
# 
# 
# merged["baselineMatchGuess"] = merged["baselineMen"]*merged["baselineWomen"]
# merged["betterMatchGuess"] = merged["guessMen"]*merged["guessWomen"]
# 
# for(i in waves){
#   trainWaves = waves[!(waves==i)] 
#   train = merged[merged["wave"] != i,]
#   test = merged[merged["wave"] == i,]
#   baselineFeatures = c("avgWaveMatch", "baselineMatchGuess", "matchAvgM", "matchAvgW")
#   betterFeatures = c("betterMatchGuess", "fieldsCrossProbMatch", baselineDecM[c(1)])
#   
#   merged[merged["wave"] == i,][["baselineMatch"]] =  getProbs(train, test, baselineFeatures, "match")
#   merged[merged["wave"] == i,][["guessMatch"]] =  getProbs(train, test, betterFeatures, "match")
# }
# 
# 
# n = names(frameHash[["train"]])
# matchProbs = c("match","baselineMatchGuess",  "betterMatchGuess", "baselineMatch", "guessMatch")
# matchAccuracyFrame = accuracyFrame(frameHash[["train"]][matchProbs],"match")
# table(ifelse(merged[["guessMatch"]] > 0.5, 1, 0), merged[["match"]])
# 10000*(0.3850 - matchAccuracyFrame["logLoss"]) 
# train = merged[merged["wave"] != 1,]
# test = merged[merged["wave"] == i,]
frameHash = hash()
frameHash[["train"]] = train
frameHash[["futureTrain"]] = test

# for(key in keys(frameHash)){
#   df = frameHash[[key]]
#   df["baselineProbDecM"] = getProbs(frameHash[["train"]], df, c(baselineDecM), "decM")
#   df["baselineProbDecW"] = getProbs(frameHash[["train"]], df, c(baselineDecW), "decW")  
#   for(i in 1:8){
#     df[menGuessRatings[i]] = getProbs(frameHash[["train"]], df, c(menGuessRatings[i], menAvgRatings[i]), "decM")
#     df[womenGuessRatings[i]] = getProbs(frameHash[["train"]], df, c(womenGuessRatings[i], womenAvgRatings[i]), "decW")
#     
#   }
#   frameHash[[key]] = df
# }
accuracyFrame = function(df, tar){
  n = names(df)
  probNames = n
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
    
  }
  return(round(errorFrame,4))
} 


newMenTraits = menTraits[c(9:26,28:70,72:96)]
newWomenTraits = gsub("M$", "W", newMenTraits)
menAvgRatings = newMenTraits[grep("AvgRating", newMenTraits)]
menGuessRatings = newMenTraits[grep("RatingGuess", newMenTraits)]
womenAvgRatings = gsub("M$", "W", menAvgRatings)
womenGuessRatings = gsub("M$", "W", gsub("M$", "W", menGuessRatings))
activities = c(newMenTraits[24:40],newWomenTraits[24:40])
rf_fit <- randomForest(y=as.factor(train[,"decW"]), x=train[c(menAvgRatings[c(1,4,6,7)],
                                                              menGuessRatings[c(1,4)], 
                                                              womenAvgRatings[c(1,7)],
                                                              newMenTraits[24:40],
                                                              newWomenTraits[24:40],
                                                              "datesCrossProbDecW",
                                                              "fieldsCrossProbDecW",
                                                              "careersCrossProbDecW",
                                                              "racesCrossProbDecW")], importance=TRUE, ntree=2000)

activities = c(newMenTraits[24:40],newWomenTraits[24:40])
activitiesForest <- randomForest(y=as.factor(train[,"decW"]), x=train[c(newWomenTraits[24:40], newMenTraits[24:40])], importance=TRUE, 
                       ntree=2000)
qualityTraits = c(menAvgRatings, menGuessRatings, womenAvgRatings)


, "datesCrossProbDecW", "fieldsCrossProbDecW",
"careersCrossProbDecW","racesCrossProbDecW"
qualityForest <- randomForest(y=as.factor(train[,"decW"]), x=train[qualityTraits], importance=TRUE, ntree=2000)
rf_fit
rf_fit$importance


rf_fit
newCrosses = crosses[c(1:18,20:60,62:length(crosses))]

features = c("guessMen", "guessWomen")
for(key in keys(frameHash)){
  for(name in c(newMenTraits,newWomenTraits, newCrosses)){
    print(name)
    df = frameHash[[key]]
    df[paste(name,"StrongProbMatch",sep="")] = getProbs(frameHash[["train"]], df, c(features, name), "match")
    frameHash[[key]] = df
  } 
  frameHash[[key]] = df
}




newestDecMenTraits = c(baselineDecM, "attrAvgRatingW", "fieldsCrossProbTraitDecM",
                       "goalsCrossProbTraitDecM", "datesCrossProbTraitDecM")
newestDecWomenTraits = c(baselineDecW, "attrAvgRatingM", "raceAsianM_raceWhiteWCross",
                         "fieldsCrossProbDecW", "datesCrossProbDecW",
                         "funRatingGuessM", "raterDecAvgM",
                         "raceWhiteM_raceWhiteWCross", "raceAsianM_raceAsianWCross")

for(key in keys(frameHash)){
  df = frameHash[[key]]
  print(key)
  df[["womenGuess"]] = getProbs(frameHash[["train"]], df, newestDecWomenTraits, "decW")
  df[["menGuess"]] = getProbs(frameHash[["train"]], df, newestDecMenTraits, "decM")
  frameHash[[key]] = df
}
probs = n[grep("StrongProbDec",n)]
menProbs = c("decM", "baselineProbDecM", probs[grep("ProbDecM", probs)], "menGuess")
womenProbs = c("decW", "baselineProbDecW", "womenGuess")

womenAccuracyFrame = accuracyFrame(frameHash[["futureTrain"]][c( womenProbs)],"decW")



10000*(womenAccuracyFrame["baselineProbDecW","logLoss"] - womenAccuracyFrame["logLoss"])
womenAccuracyFrame
