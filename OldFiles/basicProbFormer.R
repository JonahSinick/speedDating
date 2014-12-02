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




f = function(train, test ,tryCosts,features, tar){
  bestCost=NA
  bestLogLoss=1000
  worstCost=NA
  worstLogLoss= 0
  bestType=NA
  t=0
  for(ty in 1:1){
    for(co in tryCosts){
      s=scale(train[features],center=TRUE,scale=TRUE)
      m=LiblineaR(data=s,labels=train[,tar],type=0,cost=co,bias=TRUE,verbose=FALSE)
      s2= scale(test[features],attr(s,"scaled:center"),attr(s,"scaled:scale"))
      p=predict(m,s2,prob=TRUE)
      probs = p$probabilities[,"1"]
      preds = p$predictions
      bothYes = sum(ifelse(preds == 1 & test[[tar]] == 1, 1, 0))
      bothNo = sum(ifelse(preds == 0 & test[[tar]] == 0, 1, 0))
      falseNeg = sum(ifelse(preds == 0 & test[[tar]] == 1, 1, 0))
      falsePos = sum(ifelse(preds == 1 & test[[tar]] == 0, 1, 0))
      num = nrow(test)
      logLoss = round(logLoss(test[[tar]], probs),3)
      totalErr = round((falseNeg + falsePos)/num,3)
      falseNegRate = falseNeg/(falseNeg + bothNo)
      cat("Accuracies for log(C) = ",log(co, 10),": LogLoss: ",logLoss, " Error: ", totalErr, " Positives Found: ", bothYes,"\n",sep="")
      if(logLoss < bestLogLoss){
        bestCost=co
        bestLogLoss=logLoss
        bestType=ty
      }
      if(logLoss > worstLogLoss){
        worstCost=co
        worstLogLoss=logLoss
        bestType=ty
      }
    }
  }
  cat("Best log cost is: ",log(bestCost,10),"\n")
  cat("Best logLoss is: ",bestLogLoss,"\n")
  cat("Worst log cost is: ",log(worstCost,10),"\n")
  cat("Worst logLoss is: ",worstLogLoss,"\n")

}

manTraitsDecW = c("attrAvgRatingbyWofM", 
                           "funAvgRatingbyWofM", 
                           "attrRatingGuessbyWofM",
                           "likeRatingGuessbyWofM",
                           "datesManTraitDecW",
                           "decAvgbyWofM", 
                           "probAvgRatingbyWofM", 
                           "fieldsManTraitDecW",
                           "careersManTraitDecW")
womanTraitsDecW = c("raterDecAvgW", 
                "probRatingbyMofW", 
                "datesWomanTraitDecW", "avgWaveDecW")

crossTraitsDecW = c("careersCrossTraitDecW", 
                    "datesCrossTraitDecW",
                    "fieldsCrossTraitDecW", 
                    "racesCrossTraitDecW",
                    "raceAsianM_raceWhiteW")


newWomenTraitsDecW = womanTraitsDecW[c(1,2,4)]
newMenTraitsDecW = manTraitsDecW[c(1,6,9)]
newCrossTraitsDecW = crossTraitsDecW[c(1:5)]
newTraitsDecW = c(newWomenTraits, newMenTraits, newCrossTraitsDecW)

tryCosts= 10^(c(-4, -3, -2, -1, 0,1,2,3))

f(tryCosts, newTraitsDecW, "decW")


manTraitsDecM = c("goOutM", "funAvgRatingbyWofM",
                  "raterDecAvgM", "avgWaveDecM")
womanTraitsDecM = c("attrAvgRatingbyMofW", 
                    "likeRatingGuessbyMofW",
                    "careersWomanTraitDecM", 
                    "fieldsWomanTraitDecM",
                    "goalsWomanTraitDecM", 
                    "racesWomanTraitDecM",
                    "decAvgbyMofW")

crossTraitsDecM = c("careersCrossTraitDecM", 
                    "datesCrossTraitDecM", 
                    "fieldsCrossTraitDecM", 
                    "racesCrossTraitDecM",
                    "raceAsianM_raceWhiteW")
newTraitsDecM = c(womanTraitsDecM, manTraitsDecM, crossTraitsDecM)

getProbs = function(train, test, features, tar, tryCosts){
  s=scale(train[features],center=TRUE,scale=TRUE)
  bestLogLoss = 1000
  bestProbs = NA
  for(co in tryCosts){
    m=LiblineaR(data=s,labels=factor(train[,tar]),type=0,cost=co,bias=TRUE,verbose=FALSE)
    s2= scale(test[features],attr(s,"scaled:center"),attr(s,"scaled:scale"))
    p=predict(m,s2,prob=TRUE)
    probs = p$probabilities[,"1"]  
    logLoss = round(logLoss(test[[tar]], probs),3)
    if(logLoss < bestLogLoss){
      bestProbs = probs
      bestLogLoss=logLoss
    }
  }
  return(bestProbs)
}


merged = read.csv('~/Desktop/speedDating/mergedCrossFeaturesAdded.csv')
merged = merged[!(merged[["wave"]] %in% c()),]
waves = unique(merged[["wave"]])
merged["decMGuessCheating"] = 0
merged["decMGuess"] = 0
merged["badDecMGuessCheating"] = 0
merged["badDecMGuess"] = 0
tryCosts = 10^(c(-2, -1.5, -1, -0.5, 0,0.5,1,1.5,2,2.5))
basicTraitsDecM = c("raterDecAvgM", "decAvgbyMofW", "avgWaveDecM")
for(i in 1:length(waves)){
  testWaves = waves[i]
  train = merged[!(merged[["wave"]] %in% testWaves),]
  test = merged[(merged[["wave"]]  %in%  testWaves),]
  merged[(merged[["wave"]]  %in%  testWaves),][["decMGuessCheating"]] = getProbs(train, test, newTraitsDecM, "decM",tryCosts)
  merged[(merged[["wave"]]  %in%  testWaves),][["badDecMGuessCheating"]] = getProbs(train, test, basicTraitsDecM, "decM",tryCosts)
  merged[(merged[["wave"]]  %in%  testWaves),][["decMGuess"]] = getProbs(train, test, newTraitsDecM, "decM",10^c(-1.2))
  merged[(merged[["wave"]]  %in%  testWaves),][["badDecMGuess"]] = getProbs(train, test, basicTraitsDecM, "decM",10^c(-1.2))
}

logLoss(merged[["decM"]], merged[["badDecMGuess"]])
logLoss(merged[["decM"]], merged[["decMGuess"]])
logLoss(merged[["decM"]], merged[["badDecMGuessCheating"]])
logLoss(merged[["decM"]], merged[["decMGuessCheating"]])

for(s in c("badDecMGuess", "decMGuess", "badDecMGuessCheating","decMGuessCheating")){
  merged[paste(s,"Solid",sep="")] = ifelse(merged[[s]] > 0.5, 1, 0)
}

table(merged[["decM"]], merged[["badDecMGuessSolid"]])
table(merged[["decM"]], merged[["decMGuessSolid"]])
table(merged[["decM"]], merged[["badDecMGuessCheatingSolid"]])
table(merged[["decM"]], merged[["decMGuessCheatingSolid"]])


test[["decWGuess"]] = getProbs(train, test, newTraitsDecW, "decW",tryCosts)
train[["decWGuess"]] = getProbs(train, train, newTraitsDecW, "decW",tryCosts)

test[["badDecWGuess"]] = getProbs(train, test, c("raterDecAvgW", "avgWaveDecW", "decAvgbyWofM"), "decW",tryCosts)
train[["badDecWGuess"]] = getProbs(train, train, c("raterDecAvgW", "avgWaveDecW", "decAvgbyWofM"), "decW",tryCosts)




test[["decMGuess"]] = getProbs(train, test, newTraitsDecM, "decM",tryCosts)
train[["decMGuess"]] = getProbs(train, train, newTraitsDecM, "decM",tryCosts)

test[["badDecMGuess"]] = getProbs(train, test,features2, "decM",tryCosts)
train[["badDecMGuess"]] = getProbs(train, train, features2, "decM",tryCosts)

test[["badNaiveMatchGuess"]] = ifelse(round(test[["badDecMGuess"]],2) > 0.5,1,0)*ifelse(round(test[["badDecWGuess"]],2) > 0.5,1,0)
train[["badNaiveMatchGuess"]] = ifelse(round(train[["badDecMGuess"]],2) > 0.5,1,0)*ifelse(round(train[["badDecWGuess"]],2) > 0.5,1,0)
test[["naiveMatchGuess"]] = ifelse(round(test[["decMGuess"]],2) > 0.5,1,0)*ifelse(round(test[["decWGuess"]],2) > 0.5,1,0)
train[["naiveMatchGuess"]] = ifelse(round(train[["decMGuess"]],2) > 0.5,1,0)*ifelse(round(train[["decWGuess"]],2) > 0.5,1,0)
test[["naiveMatchGuessProb"]] = test[["decMGuess"]]*test[["decWGuess"]]
train[["naiveMatchGuessProb"]] = train[["decMGuess"]]*train[["decWGuess"]]
test[["matchGuess"]] = getProbs(train, test,c("naiveMatchGuessProb", crossTraitsDecW, crossTraitsDecM, crosses), "match",tryCosts)
train[["matchGuess"]] = getProbs(train, train, c("naiveMatchGuessProb", crossTraitsDecW, crossTraitsDecM, crosses), "match",tryCosts)

n = names(train)
crosses = n[10:337]
for(name in crosses){
  train[[paste(name,"DecMGuess",sep="")]] = getProbs(train, train, c(name, "decMGuess"), "decM",tryCosts)
  test[[paste(name,"DecMGuess",sep="")]] = getProbs(train, test, c(name, "decMGuess"), "decM",tryCosts)
}


for(t in 338:ncol(test)){
  loss = logLoss(test[["decM"]],test[[t]])
  if(loss < logLoss(test[["decM"]],test[["decMGuess"]])){
    print(names(test)[t])
    print(round(loss,4))
  }
}




