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
waveReader = function(wave_num, gender){
  if(gender == "male"){
    str = paste("~/Desktop/waves/menWave",paste(toString(wave_num),".csv",sep=""),sep="")  
  }
  else{
    str = paste("~/Desktop/waves/womenWave",paste(toString(wave_num),".csv",sep=""),sep="")
  }
  return(read.csv(str))
}
wave = waveReader(2, "male")

# men = men[men["wave.x"] < 6 | men["wave.x"] > 10,]

linearPredictor = function(df, target, features, ty){
  newDF = df
  train = seq(1, nrow(newDF)/2)
  y = factor(newDF[,target])
  x = newDF[,features]
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
  return(t) 
}





randForestPredictor = function(df, target, features, num_trees){
  newDF = df
  train = seq(1, nrow(newDF)/2)
  dfTrain = newDF[train,]
  dfTest = newDF[-train,]  
  rf_fit <- randomForest(y=as.factor(dfTrain[,target]), x=dfTrain[features], importance=TRUE, ntree=num_trees)
  dfTest[paste("predicted",target,sep="_")] <- predict(rf_fit, dfTest)
  s = table(dfTest[target] == 0)[[1]]/nrow(dfTest)
  t = (table(dfTest[paste("predicted",target,sep="_")] == dfTest[target])[[1]])/nrow(dfTest)
  return(t)
  return(rf_fit)
}
f = function(wave, features){
  s = 0
  for(i in 1:10){
    temp = randForestPredictor(wave,"dec",features,1000)     
    #     temp = linearPredictor(wave,"dec",features,0)     
    s = s + temp
  }
  print(s/10)
  return(s/10)
}

checker = function(features){
  var = 0
  totalLength = 0
  for(i in 1:20){
    wave = waveReader(i)
    len = length(unique(wave[["iid"]]))
    print(c(i,len))
    totalLength = totalLength + len
    var = var + f(wave,features)*len
  }
  return(var/totalLength)
}

features = c("decNormPartnersAvg","decAvg", "attrNormPartnersAvg", "funNormPartnersAvg", "likeNormPartnersAvg")
f(w)
checker(features)

# base_features0 =  c("attr", "fun", "like")
# f(wave, c(base_features0, "decPartnersAvg", "decAvg"))
# base_features1 =  c("attrNorm", "funNorm", "likeNorm")
# f(wave, c(base_features1, "decPartnersAvg", "decAvg"))
# base_features2 = c("attrPartnersAvg", "funPartnersAvg", "likePartnersAvg")
# f(wave, c(base_features2, "decPartnersAvg", "decAvg"))
# base_features3 = c("attrNormPartnersAvg", "funNormPartnersAvg", "likeNormPartnersAvg")
# f(wave, c(base_features3, "decPartnersAvg", "decAvg"))

men = ddply(men, .() ,transform ,avgMatches = mean(match))
women = ddply(women, .() ,transform ,avgMatches = mean(match))
men = ddply(men, .() ,transform ,avgDecO = mean(dec_o))
women = ddply(women, .() ,transform ,avgDecO = mean(dec_o))
men = ddply(men, .() ,transform ,avgDec = mean(dec))
women = ddply(women, .() ,transform ,avgDec = mean(dec))
