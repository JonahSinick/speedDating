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

train = read.csv('~/Desktop/speedDating/currentTrain.csv')
test = read.csv('~/Desktop/speedDating/currentTest.csv')
adjusted_acts = n[grep("ActAdj",n)]
train = train[,!(n %in% adjusted_acts)]
test = test[,!(n %in% adjusted_acts)]
n = names(train)

feature_hash = hash()
# feature_hash[["actScore1"]] = n[grep("Act",n)]
feature_hash[["physActScore1"]] =n[grep("exercise|^sports|yoga|hiking",n)]
feature_hash[["artActScore1"]] =n[grep("movies|music|reading|concerts|theater|art|museums",n)]
feature_hash[["raceScore1"]] =n[grep("raceAsian|raceBlack|raceWhite|raceLatino|samerace|imprace",n)]
feature_hash[["racePrefScore1"]] =n[grep("raceAsian|raceBlack|raceWhite|raceLatino|samerace|imprace",n)]
feature_hash[["raceCrossScore1"]] =n[grep("Cross_race",n)]
feature_hash[["raceCrossPrefScore1"]] =n[grep("Cross_race|imprace",n)]
feature_hash[["asianWhiteScore1"]] =n[grep("raceAsian|raceWhite",n)]
feature_hash[["goalScore1"]] = n[grep("goal",n)]
feature_hash[["goalScore1"]] = n[grep("goal",n)]
feature_hash[["dateScore1"]] = n[grep("date",n)]
feature_hash[["goOutScore1"]] = n[grep("go_out",n)]
feature_hash[["dateGoOutScore1"]] = n[grep("go_out|date",n)]
feature_hash[["fieldScore1"]] = n[grep("field",n)][1:38]
feature_hash[["careerScore1"]] = n[grep("career",n)][1:36]
feature_hash[["careerCrossScore1"]] = n[grep("Cross_career",n)]
feature_hash[["fieldCrossScore1"]] = n[grep("Cross_field",n)]
feature_hash[["fieldCareerScore1"]] = c(feature_hash[["fieldScore1"]], feature_hash[["careerScore1"]])
# feature_hash[["fieldCareerCrossScore1"]] = c(feature_hash[["fieldCrossScore1"]], feature_hash[["careerCrossScore1"]])
feature_hash[["STEMScore1"]] =  n[grep("Engin|Math|fieldScience",n)][1:8]
feature_hash[["STEMCrossScore1"]] = n[grep("Engin|Math|fieldScience",n)][9:30]
feature_hash[["prefScore1"]] = n[grep("Pref",n)]
feature_hash[["qualityAvgScore1"]] = n[grep("Avg_W_of_M|Avg_M_of_W",n)]
feature_hash[["qualityGuessScore1"]] = n[grep("Guess",n)]
feature_hash[["qualityScore1"]] = n[grep("Avg_W_of_M|Avg_M_of_W|Guess",n)]
for(trait in c("attr","sinc","intel","fun", "amb", "shar", "like")){
  str1 = paste(trait,"Score1",sep="")
  str2 = paste(trait,"RatingAvg",sep="")
  str3 = paste(trait,"Pref",sep="")
  str4 = paste(trait,"Guess",sep="")
  str5 = paste(str2,str3,sep="|")
  str6 = paste(str5,str4,sep="|")
  feature_hash[[str1]] = n[grep(str6,n)]
}

for(key in keys(feature_hash)){
  features = feature_hash[[key]]
  feature_hash[[paste(key,"M",sep="_")]] = features[grep("_M",features)]
  feature_hash[[paste(key,"W",sep="_")]] = features[grep("_W",features)]
}

scores = keys(feature_hash)
for(k in scores){
  print(k)
  k_new = paste(k,"M",sep="")
  
  train2 = test
  test2 = train
  features = feature_hash[[k]]
  for(string in c("dec_M")){
    print(string)
    rf_fit_x <- randomForest(y=as.factor(train[,string]), x=train[features], importance=TRUE, ntree=200, m_try = length(features))
    predictions_x = predict(rf_fit_x, test, type="prob")
    predictions_col_x = matrix(predictions_x[,2])[,1]
    test[[k_new]] = predictions_col_x
    print(string)
    rf_fit_x <- randomForest(y=as.factor(train2[,string]), x=train2[features], importance=TRUE, ntree=200, m_try = length(features))
    predictions_x = predict(rf_fit_x, test2, type="prob")
    predictions_col_x = matrix(predictions_x[,2])[,1]
    test2[[k_new]] = predictions_col_x
  }
  train = test2
}




for(k in scores){
  print(k)
  k_new = paste(k,"W",sep="")
  train2 = test
  test2 = train
  features = feature_hash[[k]]
  for(string in c("dec_W")){
    print(string)
    rf_fit_x <- randomForest(y=as.factor(train[,string]), x=train[features], importance=TRUE, ntree=200, m_try = length(features))
    predictions_x = predict(rf_fit_x, test, type="prob")
    predictions_col_x = matrix(predictions_x[,2])[,1]
    test[[k_new]] = predictions_col_x
    print(string)
    rf_fit_x <- randomForest(y=as.factor(train2[,string]), x=train2[features], importance=TRUE, ntree=200, m_try = length(features))
    predictions_x = predict(rf_fit_x, test2, type="prob")
    predictions_col_x = matrix(predictions_x[,2])[,1]
    test2[[k_new]] = predictions_col_x
  }
  train = test2
}


for(k in scores){
  print(k)
  k_new = paste(k,"Match",sep="")
  train2 = test
  test2 = train
  features = feature_hash[[k]]
  for(string in c("match_W")){
    print(string)
    rf_fit_x <- randomForest(y=as.factor(train[,string]), x=train[features], importance=TRUE, ntree=200, m_try = length(features))
    predictions_x = predict(rf_fit_x, test, type="prob")
    predictions_col_x = matrix(predictions_x[,2])[,1]
    test[[k_new]] = predictions_col_x
    print(string)
    rf_fit_x <- randomForest(y=as.factor(train2[,string]), x=train2[features], importance=TRUE, ntree=200, m_try = length(features))
    predictions_x = predict(rf_fit_x, test2, type="prob")
    predictions_col_x = matrix(predictions_x[,2])[,1]
    test2[[k_new]] = predictions_col_x
  }
  train = test2
}



write.csv(train, '~/Desktop/speedDating/enhancedTrain.csv')
write.csv(test, '~/Desktop/speedDating/enhancedTest.csv')
