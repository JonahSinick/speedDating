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
    total_num  = nrow(wave_test)
    num_incorrect = t[[1]]
    total_num = length(wave_test[[tar]])
  }
  if(model_name == "forest"){
    rf_fit <- randomForest(y=as.factor(wave_train[,tar]), x=wave_train[features], importance=TRUE)
    p = predict(rf_fit, wave_test[c(tar, features)])
    t = table(p == wave_test[[tar]])
    total_num  = nrow(wave_test)
    num_incorrect = t[[1]]
    total_num = length(wave_test[[tar]])
  }
  ans = c(num_incorrect, total_num)
  return(ans)
}


accuracies = function(){
  total_num = 0
  total_incorrect = 0
  for(i in 1:15){
    print(i)
    wave = read.csv(paste("~/Desktop/waves/newestWave",paste(toString(i),".csv",sep=""),sep=""))
    wave = data.frame(apply(wave,2,f))
    wave = ddply(wave, .(id.x) ,transform ,decSumX = sum(dec.x))
    wave = ddply(wave, .(partner.x) ,transform ,decSumPartnerX = sum(dec.x))
    wave["decSumX"] = (wave["decSumX"] - wave["dec.x"])/(length(wave[["id.x"]]) - 1)
    wave["decSumPartnerX"] = (wave["decSumPartnerX"] - wave["dec.x"])/(length(wave[["id.x"]]) - 1)
    
    n = names(wave)    
    sq_cors = n[grep(c("raterCorSq."), n)]    
    
    for(name in sq_cors){
      wave[paste("sq",name,sep="_")] = wave[name]
      wave = wave[,!(names(wave) %in% c(name))]
    }
    n = names(wave)    
    
    sq_cors = n[grep(c("sq."), n)]    
    betterProb = n[grep(c("betterProb."), n)]
    probs = n[grep(c("prob."), n)]
    
    cors = n[grep(c("raterCor."), n)]    
    y_ans = n[grep(c("yesAnswerX."), n)]
    n_ans = n[grep(c("noAnswerX."), n)]
    wave["newIDs"] = 0
    wave["newPartnerss"] = 0
    
    for(id in wave[["id.x"]]){
      wave[wave["id.x"] == id,][["newIDs"]] = toString(id)
    }
    for(id in wave[["id.x"]]){
      wave[wave["id.x"] == id,][["newIDs"]] = toString(id)
    }
    features = c(betterProb[1:length(betterProb)/1], cors[1:length(cors)/1], "betterDecProb", "decProb", "decSumPartnerX", "decSumX")
    features = c("betterDecProb", "decProb", "decSumPartnerX", "decSumX", "betterGuess", "guess")
    for(feat in features){
      wave[feat] = ifelse(wave[feat] == 0, 0.001, wave[[feat]])
      wave[feat] = ifelse(wave[feat] == 1, 0.999, wave[[feat]])
    }
    response = accuracy(wave, features, "forest", "dec.x", 0)
    if(response[2] > 0){
      print(response[1]/response[2])
      total_incorrect = total_incorrect + response[1]
      total_num = total_num + response[2]      
      print(total_incorrect/total_num)
    }
  }
  return(total_incorrect/total_num)
}

accuracies()

