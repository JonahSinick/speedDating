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




accuracies = function(){
  total_num = 0
  total_incorrect = 0
  for(i in 1:20){
    wave = read.csv(paste("~/Desktop/waves/decisions",paste(toString(i),".csv",sep=""),sep=""))
    n = names(wave)
    decs = n[grep(c("decX ."), n)]    
    votes = n[grep(c("^RaterX ."), n)] 
    probs = n[grep(c("^probX."), n)] 
    sq_votes = n[grep(c("SqRaterX ."), n)]    
    features = c("rateeDecAvgExc.x", "raterDecAvgExc.x", probs)
    response = accuracy(wave, features, "linear", "dec.x", 0)
    total_incorrect = total_incorrect + response[1]
    total_num = total_num + response[2]      
  }
  return(total_incorrect/total_num)
}

printer = function(num){
  s = 0
  for(i in 1:num){
    ob = accuracies()
    print(ob)
    s = s + ob
  }
  return(s/num)
}


printer(10)



# sequenceGenerator = function(num, num2){
#   s = sample(seq(from = 0, to = 1, by = 1), size = num, replace = TRUE)  
#   t = sample(seq(from = 0, to = 1, by = 1), size = num, replace = TRUE)  
#   print(cor(s,t))
#   print(cor(s[1:num2],t[1:num2]))
# }
# 

