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


train = read.csv('~/Desktop/speedDating/enhancedTrain.csv')
test = read.csv('~/Desktop/speedDating/enhancedTest.csv')



# features = c("decAvg_W_of_M", "raterDecAvg_W", "qualityGuessScore1W", "qualityAvgScore1W")
# target = factor(train[,"dec_W"])
# s=scale(train[features],center=TRUE,scale=TRUE)
# co=heuristicC(s)
# m=LiblineaR(data=s,labels=target,type=0,cost=co,bias=TRUE,verbose=FALSE)
# s2= scale(test[features],attr(s,"scaled:center"),attr(s,"scaled:scale"))
# p=predict(m,s2,prob=TRUE)
# t = table(p$predictions, test[["dec_W"]])
# print(t)
# t2 = table(p$predictions == test[["dec_W"]])
# print(t2[1]/nrow(test))
# 
# test[["predicted_W_Dec"]] = p$probabilities
# 
# 
# 
# features = c("decAvg_M_of_W", "raterDecAvg_M", "qualityGuessScore1M", "qualityAvgScore1M")
# target = factor(train[,"dec_M"])
# s=scale(train[features],center=TRUE,scale=TRUE)
# co=heuristicC(s)
# m=LiblineaR(data=s,labels=target,type=0,cost=co,bias=TRUE,verbose=FALSE)
# s2= scale(test[features],attr(s,"scaled:center"),attr(s,"scaled:scale"))
# p=predict(m,s2,prob=TRUE)
# t = table(p$predictions, test[["dec_M"]])
# print(t)
# t2 = table(p$predictions == test[["dec_M"]])
# print(t2[1]/nrow(test))
# 
# test[["predicted_M_Dec"]] = p$probabilities
# 
# 
# train2 = test[(test[["wave_W"]] %in% c(1,9,2,12)),]
# test2 = test[(test[["wave_W"]] %in% c(14,17,21,3)),]
# 
# train = train2
# test = test2

features = c("decAvg_M_of_W", "raterDecAvg_M", "decAvg_W_of_M", "raterDecAvg_W", "qualityAvgScore1W")
target = factor(train[,"dec_W"])
s=scale(train[features],center=TRUE,scale=TRUE)
co=heuristicC(s)
m=LiblineaR(data=s,labels=target,type=0,cost=co,bias=TRUE,verbose=FALSE)
s2= scale(test[features],attr(s,"scaled:center"),attr(s,"scaled:scale"))
p=predict(m,s2,prob=TRUE)
t = table(p$predictions, test[["dec_W"]])
print(t)
t2 = table(p$predictions == test[["dec_W"]])
print(t2[1]/nrow(test))
predicted = as.integer(p$predictions)
logLoss(test[["dec_W"]], p$probabilities[,2])





