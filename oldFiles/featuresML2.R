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

# my_train = read.csv('~/Desktop/speedDating/superTrain.csv')
# my_test = read.csv('~/Desktop/speedDating/currentCV.csv')

f = function(sequence){
  round(100*cor(my_train[sequence], my_train[c("dec_M", "dec_W", "match")]),0)
}

n = names(my_train)
race_hash = hash()
races = c("raceAsian", "raceBlack", "raceWhite", "raceLatino")
racial_importance = c("imprace_W", "imprace_M", "samerace_W")
race_hash[["racialImportance"]] = c(racial_importance)
race_hash[["menRaces"]] = c( gsub("$", "_M",races), racial_importance)
race_hash[["womenRaces"]]= c( gsub("$", "_W",races), racial_importance)
race_hash[["asianManRacesCross"]] = c(n[grep("raceAsian_M_Cross",n)], "imprace_W", "imprace_M")
race_hash[["whiteManRacesCross"]] = c(n[grep("raceWhite_M_Cross",n)], "imprace_W", "imprace_M")
race_hash[["latinoManRacesCross"]] = c(n[grep("raceLatino_M_Cross",n)], "imprace_W", "imprace_M")
race_hash[["blackManRacesCross"]] = c(n[grep("raceBlack_M_Cross",n)], "imprace_W", "imprace_M")
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
# 
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
  my_test = createProbs(my_train,my_test,h, tartar,"Both")
  my_train = createProbs(my_test,my_train,h, tartar,"Both")
}
# 
# # new_train = rbind(my_train, my_test)
# # write.csv(new_train, '~/Desktop/speedDating/superTrain.csv')
# 
# rf_probs_m = my_test[["raceCompositePred_dec_M_RF"]] 
# rf_probs_w = my_test[["raceCompositePred_dec_W_RF"]] 
# rf_probs_match = my_test[["raceCompositePred_match_RF"]] 
# 
# lin_probs_m = my_test[["raceCompositePred_dec_M_Lin"]] 
# lin_probs_w = my_test[["raceCompositePred_dec_W_Lin"]] 
# lin_probs_match = my_test[["raceCompositePred_match_Lin"]] 
# 

h = function(tar,probs){
  my_table = table(ifelse(probs > 0.5, 1, 0),tar)
  a = logLoss(tar, probs)
#   print(a)
  return(my_table)
}

# tab = h(my_test[["dec_W"]], lin_probs_w)
# b = (tab[1,2])/(tab[1,1] + tab[1,2])
# c = (tab[2,1])/(tab[2,1] + tab[2,2])
# print(c("FalseNeg","FalsePos"))
# print(c(b,c))

# m_table = table(my_test[["dec_M"]], ifelse(rf_probs_m > 0.5, 1, 0))
# w_table = table(my_test[["dec_W"]], ifelse(rf_probs_w > 0.5, 1, 0))
# match_table = table(my_test[["match"]],ifelse(rf_probs_match > 0.5, 1, 0))
# 
# table(my_test[["dec_M"]], p$predictions)



g = function(){
  print(c("MenLogLoss","MenFalseNeg", "MenFalsePos" ))
  a = logLoss(my_test[["dec_M"]], rf_probs_m)
  b = (m_table[2,1])/(m_table[1,1] + m_table[2,1])
  c = (m_table[1,2])/(m_table[1,2] + m_table[2,2])
  
  print(c("WomenLogLoss","WomenFalseNeg", "WomenFalsePos" ))
  d = logLoss(my_test[["dec_W"]], rf_probs_w)
  e = (w_table[2,1])/(w_table[1,1] + w_table[2,1])
  f = (w_table[1,2])/(w_table[1,2] + w_table[2,2])


  print(c("MatchLogLoss","MatchFalseNeg", "MatchFalsePos" ))
  g = logLoss(my_test[["match"]], rf_probs_match)
#   h = (match_table[2,1])/(match_table[1,1] + match_table[2,1])
#   i = (match_table[1,2])/(match_table[1,2] + match_table[2,2])
  h = NA
  i = NA
  matrix = matrix(nrow = 3, ncol = 3)
  matrix[1,1] = a
  matrix[1,2] = b
  matrix[1,3] = c
  matrix[2,1] = d
  matrix[2,2] = e
  matrix[2,3] = f
  matrix[3,1] = g
  matrix[3,2] = h
  matrix[3,3] = i
  
  print(round(matrix,3))
}

g()



tar = "match"
train = my_train
test = my_test
features = c("decAvg_W_of_M", "raterDecAvg_W", "decAvg_M_of_W", "raterDecAvg_M")
s=scale(train[features],center=TRUE,scale=TRUE)
co=heuristicC(s)
m=LiblineaR(data=s,labels=factor(train[,tar]),type=0,cost=co,bias=TRUE,verbose=FALSE)
s2= scale(test[features],attr(s,"scaled:center"),attr(s,"scaled:scale"))
p=predict(m,s2,prob=TRUE)
probs = p$probabilities[,"1"]
b = (tab[1,2])/(tab[1,1] + tab[1,2])
c = (tab[2,1])/(tab[2,1] + tab[2,2])
allWrong = (tab[2,1] +tab[1,2] )/((tab[1,1] + tab[1,2]) + (tab[2,1] + tab[2,2]))

# print(c("FalseNeg","FalsePos", "totalErr"))
# print(c(b,c,allWrong))'
tab = h(my_test[[tar]], probs)

tar = my_train[["match"]]
for(i in 319:ncol(my_train)){
  print(names(my_train)[i])
  col = my_train[[i]]
  print(logLoss(tar, col))
 
}
eligibles = names(my_train)[c(14:45,56:117,120:134,138:169, 179:240,252:345)]
for(name in 1:1){
  tar = "match"
  train = my_train
  test = my_test
  if(sum(my_train[[name]]) != 0 & sum(my_train[[name]]) != 0 ){
    features = c("decAvg_M_of_W", "raterDecAvg_M", "decAvg_W_of_M", "raterDecAvg_W")
    s=scale(train[features],center=TRUE,scale=TRUE)
    co=heuristicC(s)
    m=LiblineaR(data=s,labels=factor(train[,tar]),type=0,cost=co,bias=TRUE,verbose=FALSE)
    s2= scale(test[features],attr(s,"scaled:center"),attr(s,"scaled:scale"))
    p=predict(m,s2,prob=TRUE)
    probs = p$probabilities[,"1"]
    tab = h(my_test[[tar]], probs)
    print(tab)
    b = round((tab[2,2])/(tab[2,2] + tab[1,2]),3)
    allWrong = round((tab[2,1] +tab[1,2] )/((tab[1,1] + tab[1,2]) + (tab[2,1] + tab[2,2])),3)
#     if(b > 0.347){
      print(name)
      print(c("fractionGot", "totalErr"))
      print(c(b,allWrong))          
#     }
  }
}

