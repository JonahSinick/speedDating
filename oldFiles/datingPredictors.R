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


f=function(x){
  x<-as.numeric(as.character(x)) #first convert each column into numeric if it is from factor
  x[is.na(x)] =median(x, na.rm=TRUE) #convert the item with NA to median value from the column
  return(x) # the column
}

accuracy = function(wave, features, model_name, tar, j){
  wave = wave[sample(nrow(wave)),]
  wave_train = wave[seq(1, nrow(wave)/2),] 
  wave_test = wave[seq(nrow(wave)/2 + 1, nrow(wave)),]
  target = factor(wave_train[,tar])
  if(model_name == "linear"){      
    wave_train = wave_train[c(tar, features)]
    wave_test = wave_test[c(tar, features)]  
    s=scale(wave_train[features],center=TRUE,scale=TRUE)
    co=heuristicC(s)
    m=LiblineaR(data=s,labels=target,type=j,cost=co,bias=TRUE,verbose=FALSE)
    s2= scale(wave_test[features],attr(s,"scaled:center"),attr(s,"scaled:scale"))
    p=predict(m,s2)
    t = table(p$predictions == wave_test[[tar]])
    num_incorrect =  t[[1]]
    total_num = length(wave_test[[tar]])
  }
  if(model_name == "forest"){
    rf_fit <- randomForest(y=as.factor(wave_train[,tar]), x=wave_train[features], importance=TRUE)
    p = predict(rf_fit, wave_test[c(tar, features)])
    t = table(p == wave_test[[tar]])
    total_num  = t[[1]] + t[[2]]
    num_incorrect = t[[1]]      
  }
  return(num_incorrect/total_num)
}


accuracies = function(){
  total_acc = 0
  for(i in 1:20){
    wave = read.csv(paste("~/Desktop/waves/newestWave",paste(toString(i),".csv",sep=""),sep=""))
    n = names(wave)    
    betterProb = n[grep(c("betterProb."), n)]
    probs = n[grep(c("prob."), n)]
    cors = n[grep(c("raterCor."), n)]
    print(cors)
    sq_cors = n[grep(c("raterCorSq."), n)]
    y_ans = n[grep(c("yesAnswerX."), n)]
    n_ans = n[grep(c("noAnswerX."), n)]
    features = c(betterProb, sq_cors)
    for(f in features){
      wave[f] = ifelse(wave[f] == 0, 0.01, wave[[f]])
      wave[f] = ifelse(wave[f] == 1, 0.99, wave[[f]])
    }
    print(accuracy(wave, features, "linear", "dec.x", 0))
    total_acc = total_acc + accuracy(wave, features, "linear", "dec.x", 0)
  }
  return(total_acc/20)
}

accuracies()

