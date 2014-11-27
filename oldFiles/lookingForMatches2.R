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
# menAvgRatings = menTraits[grep("AvgRating", menTraits)]
# womenAvgRatings = gsub("M$", "W", menAvgRatings)
# menGuessRatings = menTraits[grep("RatingGuess", menTraits)]
# womenGuessRatings = gsub("M$", "W", gsub("M$", "W", menGuessRatings))
# menActs = menTraits[grep("Act", menTraits)]
# womenActs = gsub("M$", "W", gsub("M$", "W", menActs))
# menPrefs = menTraits[grep("Pref", menTraits)]
# womenPrefs = gsub("M$", "W", gsub("M$", "W", menPrefs))
# ratingsDecW = c(menAvgRatings, menGuessRatings, womenAvgRatings, womenGuessRatings)
# 
# baselineDecM = c("raterDecAvgM", "decAvgW","avgWaveDecM")
# baselineDecW = c("raterDecAvgW", "decAvgM","avgWaveDecW")
# 
# 
# accuracyFrame = function(df, tar){
#   n = names(df)
#   probNames = n[grep("Guess|Conjunction|Prob",n)]
#   errorFrame = df[c(1),c()]
#   errorFrame["logLoss"] = 0
#   errorFrame["overallErrorRate"] = 0
#   errorFrame["yesGuessErrorRate"] = 0
#   errorFrame["noGuessErrorRate"] = 0
#   for(name in probNames){
#     probs = df[[name]]
#     target = df[[tar]]
#     errorFrame[name,] = 0
#     guesses = ifelse(probs > 0.5, 1, 0)
#     correctYesGuessSum = sum(ifelse(guesses == 1 & target == 1, 1,0))
#     incorrectYesGuessSum = sum(ifelse(guesses == 1 & target == 0, 1,0))
#     correctNoGuessSum = sum(ifelse(guesses == 0 & target == 0, 1,0))
#     incorrectNoGuessSum = sum(ifelse(guesses == 0 & target == 1, 1,0))
#     errorFrame[name,"logLoss"] = logLoss(target, probs)
#     errorFrame[name, "overallErrorRate"] = (incorrectYesGuessSum + incorrectNoGuessSum)/length(target)
#     errorFrame[name, "yesGuessErrorRate"] = (incorrectYesGuessSum)/(incorrectYesGuessSum + correctYesGuessSum)    
#     errorFrame[name, "noGuessErrorRate"] = (incorrectNoGuessSum)/(incorrectNoGuessSum + correctNoGuessSum)
#     errorFrame[name, "fracYesFound"] = (correctYesGuessSum)/(correctYesGuessSum +  incorrectNoGuessSum)
#   }
#   return(1000*round(errorFrame,3))
# } 
# 
# 
# getProbs = function(train, test, features, tar){
#   bestLogLoss = 1000
#   bestProbs = NA
#   s=scale(train[features],center=TRUE,scale=TRUE)
#   tryCosts = c(heuristicC(s))
#   bestCost = NA
#   for(co in tryCosts){
#     
#     m=LiblineaR(data=s,labels=factor(train[,tar]),type=0,cost=co,bias=TRUE,verbose=FALSE)
#     s2= scale(test[features],attr(s,"scaled:center"),attr(s,"scaled:scale"))
#     p=predict(m,s2,prob=TRUE)
#     probs = p$probabilities[,"1"]  
#     logLoss = round(logLoss(test[[tar]], probs),4)
#     if(logLoss < bestLogLoss){
#       bestProbs = probs
#       bestLogLoss = logLoss
#       bestCost = co
#     }
#   }
#   return(bestProbs)
# }
# 
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





merged = read.csv('~/Desktop/speedDating/mergedProbsAdded.csv')
frameHash[["train"]] = merged
features = c("baselineConjunction", "metM", "tvsportsMiscActW", "fieldsCrossProbDecW", "racesManProbDecM")

for(key in keys(frameHash)){
  if(key == "train"){
    df = frameHash[[key]]
    frameHash[[key]] = df
    df["featuresProbDecM"] = getProbs(frameHash[["train"]], df, c(features), "match")
    for(name in c(menTraits, womenTraits,crosses)){
      print(name)
      df[paste(name,"ProbDecM",sep="")] = getProbs(frameHash[["train"]], df, c(features, name), "match")
      frameHash[[key]] = df
    } 
    frameHash[[key]] = df
  }
}
n = names(frameHash[["train"]])
matchProbs = c("match", n[grep("ProbMatch",n)])

df = accuracyFrame(frameHash[["train"]][matchProbs],"match")
df = df[c("logLoss", "overallErrorRate", "fracYesFound")]
currentBest = df["featuresProbMatch","fracYesFound"]
currentBest - df[df["fracYesFound"]  < currentBest,]
