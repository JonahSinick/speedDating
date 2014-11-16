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

my_train = read.csv('~/Desktop/speedDating/currentTrain.csv')
my_test = read.csv('~/Desktop/speedDating/currentTest.csv')

f = function(sequence){
  round(100*cor(my_train[sequence], my_train[c("dec_M", "dec_W", "match")]),0)
}

n = names(my_train)
race_hash = hash()
races = c("raceAsian", "raceBlack", "raceWhite", "raceLatino")
racial_importance = c("imprace_W", "imprace_M", "samerace_W")
race_hash[["menRaces"]] = c( gsub("$", "_M",races), racial_importance)
race_hash[["womenRaces"]]= c( gsub("$", "_W",races), racial_importance)
# race_hash[["asianManRacesCross"]] = c(n[grep("raceAsian_M_Cross",n)], "imprace_W", "imprace_M")
# race_hash[["whiteManRacesCross"]] = c(n[grep("raceWhite_M_Cross",n)], "imprace_W", "imprace_M")
# race_hash[["latinoManRacesCross"]] = c(n[grep("raceLatino_M_Cross",n)], "imprace_W", "imprace_M")
# race_hash[["blackManRacesCross"]] = c(n[grep("raceBlack_M_Cross",n)], "imprace_W", "imprace_M")
createProbs = function(train, test, feature_hash, tar,models){
  print(tar)
  for(k in keys(feature_hash)){
    features = feature_hash[[k]]
    suffix = paste("Pred", tar, sep = "_")
    col_name = paste(k,suffix,sep="")
    if(models == "Both" | models == "Forest"){
        rf_fit <- randomForest(y=as.factor(train[,tar]), x=train[features], importance=TRUE, ntree=400, m_try = length(features))
        predictions = predict(rf_fit, test, type="prob")
        predictions_col = matrix(predictions[,"1"])[,1]
        test[[paste(col_name,"RF",sep="_")]] = predictions_col
    }
    if(models == "Both" | models == "Linear"){
      s=scale(train[features],center=TRUE,scale=TRUE)
      co=heuristicC(s)
      m=LiblineaR(data=s,labels=factor(train[,tar]),type=0,cost=co,bias=TRUE,verbose=FALSE)
      s2= scale(test[features],attr(s,"scaled:center"),attr(s,"scaled:scale"))
      p=predict(m,s2,prob=TRUE)
      test[[paste(col_name,"Lin",sep="_")]] = matrix(p$probabilities[,"1"])[,1]
    }
  }
  return(test)
}
createProbsForAll = function(train,test,feature_hash,models){
  for(tar in c("dec_M", "dec_W", "match")){
    test = createProbs(train, test, feature_hash, tar, models)
  }
  return(test)
}
my_test = createProbsForAll(my_train,my_test,race_hash,"Linear")
my_train = createProbsForAll(my_test,my_train,race_hash,"Linear")

n =names(my_train)[319:336]
n = n[grep("Lin", n)]

for(x in c("_M", "_W",  "match")){
  h = hash()
  if(x == "_M"){
    tartar = "dec_M"
  }
  if(x == "_W"){
    tartar = "dec_W"
  }  
  if(x == "match"){
    tartar = "match"
  }  
  h[["raceComposite"]] = n[grep(x, n)]
  my_train = createProbs(my_train,my_test,h, tartar,"Both")
  my_test = createProbs(my_test,my_train,h, tartar,"Both")
}
f(-319:0)
n = names(my_train)
target = my_train[["dec_W"]]
orig_probs = my_train[["menRacesPred_dec_W_Lin"]]
logistic_probs = my_train[["raceCompositePred_dec_W_Lin"]] 
rf_probs = my_train[["raceCompositePred_dec_W_RF"]] 
  
orig_table = table(target, ifelse(orig_probs > 0.5, 1, 0))
logistic_table = table(target, ifelse(logistic_probs > 0.5, 1, 0))
rf_table = table(my_train[["dec_W"]],ifelse(rf_probs > 0.5, 1, 0))

g = function(){
  print("OrigLogLoss")
  print(logLoss(target, orig_probs))
  
  print("logisticLogLoss")
  print(logLoss(target, logistic_probs))
  
  print("rfLogLoss")
  print(logLoss(target, rf_probs))

  print("OrigMisClass")
  print((orig_table[1,2] + orig_table[2,1])/nrow(my_train))
  
  print("logisticMisClass")
  print((logistic_table[1,2] + logistic_table[2,1])/nrow(my_train))
  print("rfmisClass")
  print((rf_table[1,2] + rf_table[2,1])/nrow(my_train))
}




logOdds = function(df){
  return(log((df + 0.001)/(1.001 - df)))
}
