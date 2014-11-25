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
baselineDecM = c("raterDecAvgM", "decAvgW","avgWaveDecM")

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


getProbs = function(train, test, features, tar, tryCosts){
  bestLogLoss = 1000
  bestProbs = NA
  s=scale(train[features],center=TRUE,scale=TRUE)
  if(tryCosts == "all"){
    tryCosts = 10^(seq(-2,0,by=0.25))
  }
  else{
    tryCosts = c(heuristicC(s))
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


betterBooster = function(df, base, features, tar, numTimes){
  remainingFeatures = features
  chosenFeatures = base
  currentLogLoss = 0.65
  for(i in 1:length(features)){
    print(i)
    print(length(remainingFeatures))
    bestCandidateLogLoss = currentLogLoss
    bestAdditionalFeature = FALSE
    currentDrops = c()
    for(f in remainingFeatures){
#       cat("Feature: ", f, " #Left: ", length(remainingFeatures), " currentLogLoss: ", round(bestCandidateLogLoss,4),"\n", sep="")      
      currentFeatures = c(chosenFeatures, f)
      predictionsVector = c()
      targetVector = c()
      for(j in 1:numTimes){
        idxs = sample(1:nrow(merged))
        startIdx = 1
        midIdx = floor(2*(nrow(merged)/3))
        trainIdxs = idxs[startIdx:midIdx]
        testIdxs = idxs[(midIdx + 1):nrow(merged)]
        train = merged[trainIdxs,]
        test = merged[testIdxs,]
        targetVector = c(targetVector, test[[tar]])
        predictionsVector = c(predictionsVector, getProbs(train, test, currentFeatures, tar, "all"))
      }
      avgNewLogLoss = logLoss(targetVector, predictionsVector)
      if(avgNewLogLoss < bestCandidateLogLoss){
        cat("NewBestFeature: ", f, " currentLogLoss: ", round(bestCandidateLogLoss,4),"\n", sep="")      
        print(currentFeatures)
        
        bestCandidateLogLoss = avgNewLogLoss
        bestAdditionalFeature = f
      }
      if(avgNewLogLoss > currentLogLoss){
        currentDrops  = c(currentDrops, f)
      }      
    } 
    currentLogLoss = bestCandidateLogLoss + 0.002
    if(bestAdditionalFeature != FALSE){
      currentDrops  = c(currentDrops, bestAdditionalFeature)
      chosenFeatures = c(chosenFeatures, bestAdditionalFeature)
      remainingFeatures = remainingFeatures[!(remainingFeatures %in% currentDrops)]
      cat("Best additional feature: ", bestAdditionalFeature, " New logLoss: ", currentLogLoss,"\n",sep="")
    }
    else{
      return(chosenFeatures)
    }
  }
}

bestFeatures = betterBooster(merged,baselineDecM, features, "decM", 30)
