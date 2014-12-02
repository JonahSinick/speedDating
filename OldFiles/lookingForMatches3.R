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

# merged = read.csv( '~/Desktop/speedDating/mergedCrossFeaturesAdded.csv')
# n = names(merged)
# avgRatings = n[grep("AvgRating", n)]
# guesses = n[grep("RatingGuess",n)]
# crosses = n[grep("Cross",n)]
# probs = n[grep("ProbDec",n)]
# prefs = n[grep("Pref",n)]
# wealthInds = n[grep("WealthInd",n)]
# 
# menTraits = n[grep("M$",n)][c(11:28,30:52,54:59,61:70,73:75,77:96)]
# womenTraits = gsub("M$", "W", menTraits)

# menActs = menTraits[grep("Act", menTraits)]
# womenActs = gsub("M$", "W", gsub("M$", "W", menActs))
# menPrefs = menTraits[grep("Pref", menTraits)]
# womenPrefs = gsub("M$", "W", gsub("M$", "W", menPrefs))
# ratingsDecW = c(menAvgRatings, menGuessRatings, womenAvgRatings, womenGuessRatings)
# 
# baselineDecM = c("raterDecAvgM", "decAvgW","avgWaveDecM")
# baselineDecW = c("raterDecAvgW", "decAvgM","avgWaveDecW")


accuracyFrame = function(df, tar){
  n = names(df)
  probNames = n[grep("Guess|Conjunction|Prob",n)]
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
  return(10000*round(errorFrame,4))
} 


getProbs = function(train, test, features, tar){
  bestLogLoss = 1000
  bestProbs = NA
  s=scale(train[features],center=TRUE,scale=TRUE)
  tryCosts = c(heuristicC(s))
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

# frameHash = hash()
# waves = unique(merged[["wave"]])
#  idxs = sample(1:nrow(merged))
# trainIdxs = idxs[1:(2*nrow(merged)/3)]
# testIdxs = idxs[(2*nrow(merged)/3 + 1):nrow(merged)]
# frameHash[["train"]] = merged[trainIdxs,]
# frameHash[["test"]] = merged[testIdxs,]
# 
# otherFeatures =c("funAvgRatingM",
#                  "attrAvgRatingW",
#                  "ageM", 
#                  "exphappyW",
#                  "shoppingWealthIndW",
#                  "goalMeetNewM_goalFunNightWCross",
#                  "goalMeetNewM_goalMeetNewWCross",
#                  "goalFunNightM_goalMeetNewWCross",
#                  "goalFunNightM_goalFunNightWCross",
#                  "sharPrefW",
#                  "datesCrossProbDecW",
#                  "raceAsianM_raceWhiteWCross",
#                  "fieldsCrossProbMatch",
#                  "goalsCrossProbMatch",
#                  "careerAcademicM",
#                  "racesCrossProbMatch",
#                  "careerFinanceW",
#                  "impraceW",
#                  "impraceM",
#                  "dateW",
#                  "probAvgRatingM",
#                  "probAvgRatingW",
#                  "impreligW",
#                  "raceAsianW",
#                  "careersManProbDecW",
#                  "careersWomanProbDecW")
# 
# features = c(baselineDecW, baselineDecM, otherFeatures)
# 
# merged["baselineMatchGuess"] = 0
# merged["betterMatchGuess"] = 0
# mergedSlice = merged[c("match", "decM", "decW", features)]
# for(i in 1:nrow(mergedSlice)){
#   print(i)
#   arr = 1:nrow(mergedSlice)
#   trainIdxs = arr[arr != i]
#   train = mergedSlice[trainIdxs,]
#   test = mergedSlice[i,]
#   mergedSlice[i,"baselineDecMGuess"] =  getProbs(train, test, c(baselineDecM), "decM")[1]
#   mergedSlice[i,"baselineDecWGuess"] =  getProbs(train, test, c(baselineDecW), "decW")[1]
#   mergedSlice[i,"betterDecMGuess"] =  getProbs(train, test, c(features), "decM")[1]
#   mergedSlice[i,"betterDecWGuess"] =  getProbs(train, test, c(features), "decW")[1]
#   mergedSlice[i,"baselineMatchGuess"] =  getProbs(train, test, c(baselineDecW, baselineDecM), "match")[1]
#   mergedSlice[i,"betterMatchGuess"] =  getProbs(train, test, features, "match")[1]
#   print(round(mergedSlice[i, c("baselineDecMGuess", "baselineDecWGuess", "baselineMatchGuess")],2))
#   print(round(mergedSlice[i, c("betterDecMGuess", "betterDecWGuess", "betterMatchGuess")],2))
#   print(mergedSlice[i, c("decM", "decW", "match")])
# }
# 
# # write.csv(mergedSlice, '~/Desktop/speedDating/mergedSliceProcessed.csv')
# 
# 
# mergedSlice["baselineConjunction"] = mergedSlice["baselineDecMGuess"]*mergedSlice["baselineDecWGuess"]
# mergedSlice["betterConjunction"] = mergedSlice["betterDecMGuess"]*mergedSlice["betterDecWGuess"]
# mergedSlice[c("baselineMatchGuess+","betterMatchGuess+")] = 0
# accuracyFrame(mergedSlice[c("match", 
#                             "baselineMatchGuess", 
#                             "betterMatchGuess",
#                             "baselineConjunction",
#                             "betterConjunction",
#                             "betterMatchGuessNoFeatures+"
#                             )],
#               "match")
# 
# for(i in 1:nrow(mergedSlice)){
#   print(i)
#   arr = 1:nrow(mergedSlice)
#   trainIdxs = arr[arr != i]
#   train = mergedSlice[trainIdxs,]
#   test = mergedSlice[i,]
#   mergedSlice[i,"baselineMatchGuess+"] =  getProbs(train, test, c(baselineDecW, baselineDecM, "baselineConjunction"), "match")[1]
#   mergedSlice[i,"betterMatchGuess+"] =  getProbs(train, test, c(features, 
#                                                                 "baselineDecMGuess", 
#                                                                 "baselineDecWGuess",
#                                                                 "betterDecMGuess",
#                                                                 "betterDecWGuess"), "match")[1]
#   mergedSlice[i,"betterMatchGuessNoFeatures+"] =  getProbs(train, test, c("baselineDecMGuess", 
#                                                                 "baselineDecWGuess",
#                                                                 "betterDecMGuess",
#                                                                 "betterDecWGuess"), "match")[1]
# }
# 
# matchProbs = c("match", "baselineMatchGuess", "betterMatchGuess","baselineConjunction",
#                "betterConjunction", "baselineMatchGuess+", "betterMatchGuess+", 
#                "betterMatchGuessNoFeatures+")
#                
# 
# 
# 
# 
# 
# baselineMatchGuess
# accuracyFrame(mergedSlice[c(matchProbs)],"match")
# mergedSlice["superMatchGuess"] =  0
# 
# for(i in 1:nrow(mergedSlice)){
#   print(i)
#   arr = 1:nrow(mergedSlice)
#   trainIdxs = arr[arr != i]
#   train = mergedSlice[trainIdxs,]
#   test = mergedSlice[i,]
#   mergedSlice[i,"superMatchGuess"] =  getProbs(train, test, c(otherFeatures, "betterMatchGuess+", "betterConjunction"), "match")[1]
# }
# 
# matchProbs = c("match", "baselineMatchGuess", "betterMatchGuess","baselineConjunction",
#                "betterConjunction", "baselineMatchGuess+", "betterMatchGuess+", 
#                "betterMatchGuessNoFeatures+", "superMatchGuess", "superSuperMatchGuess")
# 
# accuracyFrame(mergedSlice[c(matchProbs)],"match")
# 
# for(i in 1:nrow(mergedSlice)){
#   print(i)
#   arr = 1:nrow(mergedSlice)
#   trainIdxs = arr[arr != i]
#   train = mergedSlice[trainIdxs,]
#   test = mergedSlice[i,]
#   mergedSlice[i,"superSuperMatchGuess"] =  getProbs(train, test, c(features, "baselineConjunction", "betterConjunction"), "match")[1]
# }
# 
# matchProbs = c("match", "baselineMatchGuess", "betterMatchGuess","baselineConjunction",
#                "betterConjunction", "baselineMatchGuess+", "betterMatchGuess+", 
#                "betterMatchGuessNoFeatures+", "superMatchGuess", "superSuperMatchGuess")
# 
# accuracyFrame(mergedSlice[c(matchProbs)],"match")
# 
# for(i in 1:nrow(mergedSlice)){
#   print(i)
#   arr = 1:nrow(mergedSlice)
#   trainIdxs = arr[arr != i]
#   train = mergedSlice[trainIdxs,]
#   test = mergedSlice[i,]
#   mergedSlice[i,"super3MatchGuess"] =  getProbs(train, test, c(otherFeatures, 
#                                                                "baselineConjunction"), "match")[1]
# }
# 
# 
# matchProbs = c("match", "baselineMatchGuess", "betterMatchGuess","baselineConjunction",
#                "betterConjunction", "baselineMatchGuess+", "betterMatchGuess+", 
#                "betterMatchGuessNoFeatures+", "superMatchGuess", "superSuperMatchGuess",
#                "super3MatchGuess")
# 
# accuracyFrame(mergedSlice[c(matchProbs)],"match")
# merged[matchProbs] = mergedSlice[matchProbs]
# merged = merged[1:239]
# write.csv(merged, '~/Desktop/speedDating/mergedProbsAdded.csv')

# merged = read.csv( '~/Desktop/speedDating/mergedProbsAdded.csv')
n = names(merged)
avgRatings = n[grep("AvgRating", n)]
guesses = n[grep("RatingGuess",n)]
crosses = n[grep("Cross",n)]
probs = n[grep("ProbDec",n)]
prefs = n[grep("Pref",n)]
wealthInds = n[grep("WealthInd",n)]

menTraits = n[grep("M$",n)][c(11:28,30:52,54:59,61:70,73:75,77:96)]
womenTraits = gsub("M$", "W", menTraits)
menAvgRatings = menTraits[grep("AvgRating", menTraits)]
womenAvgRatings = gsub("M$", "W", menAvgRatings)
menGuessRatings = menTraits[grep("RatingGuess", menTraits)]
womenGuessRatings = gsub("M$", "W", gsub("M$", "W", menGuessRatings))
menActs = menTraits[grep("Act", menTraits)]
womenActs = gsub("M$", "W", gsub("M$", "W", menActs))
menPrefs = menTraits[grep("Pref", menTraits)]
womenPrefs = gsub("M$", "W", gsub("M$", "W", menPrefs))
ratingsDecW = c(menAvgRatings, menGuessRatings, womenAvgRatings, womenGuessRatings)

baselineDecM = c("raterDecAvgM", "decAvgW","avgWaveDecM")
baselineDecW = c("raterDecAvgW", "decAvgM","avgWaveDecW")




 merged = read.csv( '~/Desktop/speedDating/mergedCrossFeaturesAdded.csv')
# frameHash = hash()
# frameHash[["train"]] = merged
menFeatures = c(baselineDecM, 
             "datesCrossProbDecM", 
             "datesManProbDecM",
             "fieldsCrossProbDecM", 
             "goalsCrossProbDecM",
             "decAvgM",
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

# for(key in keys(frameHash)){
#   if(key == "train"){
#     df = frameHash[[key]]
#     frameHash[[key]] = df
#     df["baselineProbDecM"] = getProbs(frameHash[["train"]], df, c(baselineDecM), "decM")
#     
#     df["featuresProbDecM"] = getProbs(frameHash[["train"]], df, c(features), "decM")
# #     for(name in c(menTraits, womenTraits,crosses)){
# #       print(name)
# #       df[paste(name,"ProbDecM",sep="")] = getProbs(frameHash[["train"]], df, c(features, name), "decM")
# #       frameHash[[key]] = df
# #     } 
#     frameHash[[key]] = df
#   }
# }
# n = names(frameHash[["train"]])
# menProbs = c("decM", n[grep("ProbDecM",n)])
# 
# df = accuracyFrame(frameHash[["train"]][menProbs],"decM")
# currentBest = df["featuresProbDecM","logLoss"]
# currentBest - df[df["logLoss"]  < currentBest,]["logLoss"]
# 
# 

merged[c("baseProbDecM","betterProbDecM"] = 0
mergedSlice = merged[c("decM", menFeatures)]


for(i in 1:nrow(mergedSlice)){
  arr = 1:nrow(merged)
  trainIdxs = arr[arr != i]
  train = mergedSlice[trainIdxs,]
  test = mergedSlice[i,]
  merged[i,"baseProbDecM"] =  getProbs(train, test, baselineDecM, "decM")[1]
  merged[i,"betterProbDecM"] =  getProbs(train, test, menFeatures, "decM")[1]
  print(round(merged[i, c("decM", "baseProbDecM", "betterProbDecM")],2))
}


# accuracyFrame(merged[c("decM", "baseProbDecM", "betterProbDecM")], "decM")
# 
# for(i in 0:19){
#   print((i/20))
#   print(mean(merged[(20*merged["baseProbDecM"]) > i & (20*merged["baseProbDecM"]) < (i + 1) ,][["decM"]]))
#   print(mean(merged[(20*merged["betterProbDecM"]) > i & (20*merged["betterProbDecM"]) < (i + 1) ,][["decM"]]))
# }
# 
# 
# for(i in unique(merged[["iidM"]])){
#   slice = merged[merged["iidM"] ==i, c("decM", "baseProbDecM", "betterProbDecM")]
#   print(i)
#   print((colSums(slice)/nrow(slice)))
# }
# 



womenFeatures = c(baselineDecW, 
             "attrAvgRatingM", 
             "datesCrossProbDecW", 
             "fieldsCrossProbDecW", 
             "goalsCrossProbDecW",
             "careersWomanProbDecW", 
             "careersCrossProbDecW", 
             "exphappyW", 
             "raterDecAvgM", 
             "funAvgRatingM",
             "avgWaveDecM", 
             "probAvgRatingM", 
             "ageM", 
             "raceWhiteM_raceWhiteWCross", 
             "raceWhiteW",
             "museumsArtActW", 
             "intelAvgRatingW", 
             "shoppingWealthIndW",
             "intelAvgRatingM",
             "goOutM",
             "ambPrefM",
             "tvsportsMiscActW",
             "date3W",
             "raceAsianW",
             "incomeWealthIndM",
             "diningWealthIndM",
             "decAvgW",
             "impraceW",
             "raceWhiteM_raceAsianWCross",
             "incomeWealthIndW",
             "raceWhiteM",
             "gamingMiscActM",
             "sharPrefW",
             "careerAcademicW",
             "careerFinanceW",
             "moviesArtActW",
             "hikingPhysActM",
             "exercisePhysActW",
             "goalsWomanProbDecW",
             "fieldsManProbDecW",
             "tvMiscActW",
             "date7W",
             "fieldsWomanProbDecW",
             "readingArtActW")

# for(key in keys(frameHash)){
#   if(key == "train"){
#     df = frameHash[[key]]
#     frameHash[[key]] = df
#     df["baselineProbDecW"] = getProbs(frameHash[["train"]], df, c(baselineDecW), "decW")
#     
#     df["featuresProbDecW"] = getProbs(frameHash[["train"]], df, c(features), "decW")
#     for(name in c(menTraits, womenTraits,crosses)){
#       print(name)
#       df[paste(name,"ProbDecW",sep="")] = getProbs(frameHash[["train"]], df, c(features, name), "decW")
#       frameHash[[key]] = df
#     } 
#     frameHash[[key]] = df
#   }
# }
# n = names(frameHash[["train"]])
# womenProbs = c("decW", n[grep("ProbDecW",n)])
# 
# df = accuracyFrame(frameHash[["train"]][womenProbs],"decW")
# currentBest = df["featuresProbDecW","logLoss"]
# currentBest - df[df["logLoss"]  < currentBest - 2,]["logLoss"]




merged[c("baseProbDecW","betterProbDecW"] = 0
mergedSlice = merged[c("decW", womenFeatures)]


for(i in 1:nrow(mergedSlice)){
  arr = 1:nrow(merged)
  trainIdxs = arr[arr != i]
  train = mergedSlice[trainIdxs,]
  test = mergedSlice[i,]
  merged[i,"baseProbDecW"] =  getProbs(train, test, baselineDecW, "decW")[1]
  merged[i,"betterProbDecW"] =  getProbs(train, test, womenFeatures, "decW")[1]
  print(round(merged[i, c("decW", "baseProbDecW", "betterProbDecW")],2))
}

write.csv(merged, '~/Desktop/speedDating/menAndWomenGuessesAdded.csv')

merged["baselineConjunction"] = merged["baseProbDecM"]*merged["baseProbDecW"]
merged["betterConjunction"] = merged["betterProbDecM"]*merged["betterProbDecW"]
# accuracyFrame(merged[c("match", "baselineConjunction", "betterConjunction")],"match")

matchFeatures = c("attrAvgRatingM", "decAvgM", "goalsCrossProbMatch", "funAvgRatingM",
             "fieldsCrossProbMatch", "careerAcademicM_careerAcademicWCross", "matchAvgM", "datesCrossProbMatch",
             "ageM", "concertsMiscActW", "incomeWealthIndM", "matchAvgW", "date6M", "shoppingWealthIndW",
             "exercisePhysActM", "readingArtActM", "yogaPhysActM", "impraceW", "fieldsCrossProbDecM", "racesWomanProbDecW",
             "careerFinanceW", "raceAsianM_raceWhiteWCross", "avgWaveDecM", "attrAvgRatingW", "artArtActW",
             "incomeWealthIndW", "avgWaveDecW", "raterDecAvgW", "careersWomanProbDecW",
             "raceWhiteW", "impreligW", "moviesArtActW", "diningWealthIndW", "exphappyM", "fieldsWomanProbDecW",
             "careersCrossProbMatch", "probAvgRatingM", "racesManProbDecM", "careerFinanceM", "sincAvgRatingW",
             "date3W", "intelPrefW", "intelAvgRatingW", "attrPrefM", "intelPrefM","fieldScienceM",
             "musicArtActW", "careersWomanProbDecM", "gamingMiscActW", "raceAsianW", "hikingPhysActW", "date4M_date6WCross",
             "racesWomanProbDecM", "tvMiscActW", "theaterArtActW", "fieldScienceW", "matchAvgW", "raceWhiteM_raceAsianWCross", 
             "goalMeetNewM_goalMeetNewWCross", "fieldBusinessW", "datesWomanProbDecW","raterDecAvgM", "goalsWomanProbDecM",
             "fieldBusinessW", "sharPrefW")
# candidates = c(menTraits, womenTraits,crosses)
# for(key in keys(frameHash)){
#   if(key == "train"){
#     df = frameHash[[key]]
#     frameHash[[key]] = df
#     df["baselineProbMatch"] = getProbs(frameHash[["train"]], df, c("baselineConjunction"), "match")
#     
#     df["featuresProbMatch"] = getProbs(frameHash[["train"]], df, c(matchFeatures, candidates), "match")
#     for(name in candidates){
#       print(name)
#       df[paste(name,"ProbMatch",sep="")] = getProbs(frameHash[["train"]], df, c(features, name), "match")
#       frameHash[[key]] = df
#     } 
#     frameHash[[key]] = df
#   }
# }



# 
# n = names(frameHash[["train"]])
# # matchProbs = c("match", n[grep("ProbMatch",n)])
# # 
# df = accuracyFrame(frameHash[["train"]][matchProbs],"match")
# currentBest = df["featuresProbMatch","logLoss"]
# currentBest - df[df["logLoss"]  < currentBest,]["logLoss"]


merged[c("bestProbMatch", "besterProbMatch")] = 0


for(i in 1:nrow(merged)){
  arr = 1:nrow(merged)
  trainIdxs = arr[arr != i]
  train = merged[trainIdxs,]
  test = merged[i,]
  merged[i,"bestProbMatch"] =  getProbs(train, test, c("baselineConjunction", matchFeatures), "match")[1]
  merged[i,"besterProbMatch"] =  getProbs(train, test, c("betterConjunction", matchFeatures), "match")[1]
  print(round(merged[i, c("match", "baselineConjunction", "betterConjunction", "bestProbMatch", "besterProbMatch")],2))
}


accuracyFrame(merged[c("match", "baselineConjunction", "betterConjunction", "bestProbMatch", "besterProbMatch")],"match")

write.csv(merged, '~/Desktop/speedDating/matchGuessesAdded.csv')
