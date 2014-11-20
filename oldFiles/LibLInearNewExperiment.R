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

waveReader = function(i){
  return(read.csv(paste("~/Desktop/waves/wave",paste(toString(i),".csv",sep=""),sep="")))
}
wave = waveReader(2)

# base_men = read.csv("~/Desktop/menPartiallyProcessed.csv")
# base_women = read.csv("~/Desktop/womenPartiallyProcessed.csv")
# men = base_men
# women = base_women
# men = men[men["wave.x"] < 6 | men["wave.x"] > 10,]
# women = women[women["wave.x"] < 6 | women["wave.x"] > 10,]

linearPredictor = function(df, target, features, ty){
  train = seq(1, nrow(df)/2)
  y = factor(df[,target])
  x = df[,features]
  xTrain=x[train,]
  xTest=x[-train,]
  yTrain=y[train]
  yTest=y[-train]
  s=scale(xTrain,center=TRUE,scale=TRUE)
  co=heuristicC(s)
  m=LiblineaR(data=s,labels=yTrain,type=ty,cost=co,bias=TRUE,verbose=FALSE)
  s2=scale(xTest,attr(s,"scaled:center"),attr(s,"scaled:scale"))
  p=predict(m,s2)
  s = table(yTest == 0)[[1]]/length(yTest)
  t = table(yTest ==p$predictions)[[1]]/length(yTest)
  print(c(s,t,s/t))
  return(m) 
}





randForestPredictor = function(df, target, features, num_trees){
  train = seq(1, nrow(df)/2)
  dfTrain = df[train,]
  dfTest = df[-train,]  
  rf_fit <- randomForest(y=as.factor(dfTrain[,target]), x=dfTrain[features], importance=TRUE, ntree=num_trees)
  dfTest[paste("predicted",target,sep="_")] <- predict(rf_fit, dfTest)
  s = table(dfTest[target] == 0)[[1]]/nrow(dfTest)
  t = (table(dfTest[paste("predicted",target,sep="_")] == dfTest[target])[[1]])/nrow(dfTest)
  print(c(s,t,s/t))
  return(rf_fit)
}
# women["dec.x"] <- ifelse(women["dec.x"] > 0,1, 0 )
# men["dec.x"] <- ifelse(men["dec.x"] > 0,1, 0 )
# 
# cols = names(men)
# 
# othersRatings = cols[362:370]
# actsInters = cols[345:358]
# partnerActivities = cols[328:346]
# ownActivities = cols[159:175]
# origPartnerActivities = cols[189:205]
# ownBinaries = cols[65:85]
# partnerBinaries = cols[234:254]
# partnerGoOut = partnerBinaries[1:5]
# partnerDate = partnerBinaries[8:14]
# partnerRace = partnerBinaries[15:21]
# goOut = ownBinaries[1:7]
# date = ownBinaries[8:14]
# race = ownBinaries[15:21]
# ratingInters = cols[371:375]

# selectivity = c("attr.ownRating.x",  "sinc.ownRating.x",  "intel.ownRating.x", "fun.ownRating.x",   "amb.ownRating.x",   "shar.ownRating.x",  "like.ownRating.x",  "prob.ownRating.x")
# features = c(othersRatings,actsInters,partnerActivities,ownActivities,"racialInter.x", binaries,ratingInters,origPartnerActivities)
# expectations = c("exphappy.x", "exphappy.y", "expnum.x", "expnum.y")
# features = c(othersRatings, selectivity, date, expectations)
base_features0 =  c("attr", "fun", "like")
base_features1 =  c("attrNorm", "funNorm", "likeNorm")
base_features2 = c("attrPartnersAvg", "funPartnersAvg", "likePartnersAvg")
base_features3 = c("attrNormPartnersAvg", "funNormPartnersAvg", "likeNormPartnersAvg")

f = function(wave, features){
  
  print("linear")
  men_pred = linearPredictor(wave,"dec",features,0)
  
  print("forest")
  men_forest_pred = randForestPredictor(wave,"dec",features,1000)  
}
base_features0 =  c("attr", "fun", "like")
f(wave, c(base_features0, "decPartnersAvg", "decAvg"))
base_features1 =  c("attrNorm", "funNorm", "likeNorm")
f(wave, c(base_features1, "decPartnersAvg", "decAvg"))
base_features2 = c("attrPartnersAvg", "funPartnersAvg", "likePartnersAvg")
f(wave, c(base_features2, "decPartnersAvg", "decAvg"))
base_features3 = c("attrNormPartnersAvg", "funNormPartnersAvg", "likeNormPartnersAvg")
f(wave, c(base_features3, "decPartnersAvg", "decAvg"))

f(wave, c("decPartnersAvg", "attrPartnersAvg", "funPartnersAvg"))
f(wave, c("decPartnersAvg", "attrNormPartnersAvg", "funNormPartnersAvg"))
