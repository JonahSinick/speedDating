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

#   tryCosts =  c(1.584893)
  tryCosts = 10^(seq(-2,2,by=0.1))
#   
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
  print(bestCost)
  return(bestProbs)
}


baselineDecM = c("raterDecAvgM", "decAvgW","avgWaveDecM")
baselineDecW = c("raterDecAvgW", "decAvgM","avgWaveDecW")

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


idxs = sample(1:nrow(merged))
trainIdxs = idxs[1:(2*nrow(merged)/3)]
testIdxs = idxs[(2*nrow(merged)/3 + 1):nrow(merged)]
train = merged[trainIdxs,]
test = merged[trainIdxs,]


test["baselineProbDecM"] = getProbs(train, test,baselineDecM, "decM") 
test["currentFeatureProbDecM"] = getProbs(train, test, menFeatures, "decM")
# currentMenFeatures = menFeatures
# for(m in currentMenFeatures){
#   print(m)
#   features = currentMenFeatures[currentMenFeatures != m]
#   test[paste(m,"ExcProbDecM",sep="")] = getProbs(train, test, features, "decM")
# }
# 
# 
# n = names(test)
# menProbs = c("decM", n[grep("ProbDecM",n)])
# df = accuracyFrame(test[menProbs],"decM")
# currentBaseline = df["baselineProbDecM", "logLoss"]
# currentBest = df["currentFeatureProbDecM", "logLoss"]
# 
# df[df["logLoss"] - currentBest < 4600,]["logLoss"]
# n = names(merged)
# crosses = n[grep("Cross",n)]
# menTraits = n[grep("M$",n)][c(11:28,30:52,54:59,61:70,73:75,77:96)]
# womenTraits = gsub("M$", "W", menTraits)
# 
# for(m in c(menTraits, womenTraits,crosses)){
#   if(!(m %in% menFeatures)){
#     print(m)
#     
#     test[paste(m,"IncProbDecM",sep="")] = getProbs(train, test, c(m, menFeatures), "decM")  
#   }  
# }
# n = names(test)
# menProbs = c("decM", n[grep("ProbDecM",n)])
# df = accuracyFrame(test[menProbs],"decM")
# currentBaseline = df["baselineProbDecM", "logLoss"]
# currentBest = df["currentFeatureProbDecM", "logLoss"]
# 
# currentBest - df["logLoss"]
# 
# 
# n = names(test)
# menProbs = c("decM", n[grep("ProbDecM",n)])
# df = accuracyFrame(test[menProbs],"decM")
# currentBaseline = df["baselineProbDecM", "logLoss"]
# currentBest = df["currentFeatureProbDecM", "logLoss"]

features = c(menTraits, womenTraits,crosses)
badFeatures = n[grep("Match$|DecW$|Guess",n)]
features = features[!(features %in% badFeatures)]
test["everythingProbDecM"] = getProbs(train, test, features, "decM")  
df = accuracyFrame(test[c("decM", "baselineProbDecM", "currentFeatureProbDecM", "everythingProbDecM")],"decM")
