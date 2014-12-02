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
    wave_train = wave_train[c(tar, features)]
    wave_test = wave_test[c(tar, features)]  
    s=scale(wave_train[features],center=TRUE,scale=TRUE)
    co=heuristicC(s)
    m=LiblineaR(data=s,labels=target,type=j,cost=co,bias=TRUE,verbose=FALSE)
    s2= scale(wave_test[features],attr(s,"scaled:center"),attr(s,"scaled:scale"))
    p=predict(m,s2)
    t = table(p$predictions == wave_test[[tar]])
  print(t)
    total_num  = nrow(wave_test)
    num_incorrect = t[[1]]
    total_num = length(wave_test[[tar]])
    return(c(num_incorrect, total_num))
  }
  
  
  accuracies = function(){
    total_num = 0
    total_incorrect = 0
    for(i in 4:4){
      wave = read.csv(paste("~/Desktop/waves/decisions",paste(toString(i),".csv",sep=""),sep=""))
      n = names(wave)
      votes = n[grep(c("RaterX."), n)]    
      decs = n[grep(c("decX."), n)]    

      features = c(votes)
      response = accuracy(wave, features, "linear", "dec.x", 0)
      total_incorrect = total_incorrect + response[1]
      total_num = total_num + response[2]      
    }
    return(total_incorrect/total_num)
  }

accuracies()
