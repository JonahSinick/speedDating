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

df <- read.csv('~/Desktop/speedDatingData.csv')
f=function(x){
  x<-as.numeric(as.character(x)) #first convert each column into numeric if it is from factor
  x[is.na(x)] =median(x, na.rm=TRUE) #convert the item with NA to median value from the column
  x #display the column
}

keeps = c("iid", "pid", "wave", "gender", "id", "partner", "dec", "attr", "fun" , "like", "prob", "amb", "intel", "sinc", "shar")
keeps_to_fix = c("dec", "attr", "fun" , "like", "prob", "amb", "intel", "sinc", "shar")
df = df[keeps]
df_na_rm = df[keeps_to_fix]
df_na_rm = data.frame(apply(df_na_rm,2,f))
df[keeps_to_fix] = df_na_rm[keeps_to_fix]
df = na.omit(df)

men = df[df["gender"] == 1,]







f = function(features){
  num_incorrect = 0
  total_num = 0
  nums = seq(1, 20)
  incorrects = seq(1, 20)
  fracs = seq(1,20)
  for(i in 1:20){
    wave = men[men["wave"] == i,]
    if(nrow(wave) >= 200){
      print(i)
#       wave = ddply(wave, .(iid) ,transform ,numPartners = length(unique(pid)))
#       wave = ddply(wave, .(iid) ,transform ,decSum = sum(dec))
#       wave = ddply(wave, .(pid) ,transform ,partnerDecSum = sum(dec))
#       wave = ddply(wave, .(iid) ,transform ,attrMean = mean(attr))
#       wave = ddply(wave, .(iid) ,transform ,funMean = mean(fun))
#       wave = ddply(wave, .(iid) ,transform ,likeMean = mean(like))  
#       len = length(unique(wave[["pid"]]))
#       wave["decAvgExc"] = (wave["decSum"] - wave["dec"] + median(wave[["dec"]]))/(len - 1)
#       wave["partnerDecAvgExc"] = (wave["partnerDecSum"] - wave["dec"] + median(wave[["dec"]]))/(length(unique(wave[["iid"]])) - 1)
#       wave["decAvgExc"] = ifelse(wave[["decAvgExc"]] == 0, 0.01, wave[["decAvgExc"]])
#       wave["partnerDecAvgExc"] = ifelse(wave[["partnerDecAvgExc"]] == 0, 0.01, wave[["partnerDecAvgExc"]])
#       wave["attr"] = wave["attr"] - wave["attrMean"]
#       wave["fun"] = wave["fun"] - wave["funMean"]
#       wave["like"] = wave["like"] - wave["likeMean"]
#     
#       wave["attrMeanExc"] = (wave["attrMean"]*len - wave["attr"] + median(wave[["attr"]]))/(len - 1)
#       wave["funMeanExc"] = (wave["funMean"]*len - wave["fun"]  + median(wave[["fun"]]))/(len - 1)
#       wave["likeMeanExc"] = (wave["likeMean"]*len - wave["like"]  + median(wave[["like"]]))/(len - 1)
#       
#       wave  = wave[sample(nrow(wave)),]
#       cutoff = nrow(wave)/2
#       train = wave[1:cutoff,]
#       test = wave[(cutoff + 1):nrow(wave),]
#       target = factor(train[,"dec"])
#       s=scale(train[features],center=TRUE,scale=TRUE)
#       co=heuristicC(s)
#       m=LiblineaR(data=s,labels=target,type=0,cost=co,bias=TRUE,verbose=FALSE)
#       s2= scale(test[features],attr(s,"scaled:center"),attr(s,"scaled:scale"))
#       p=predict(m,s2)
#       t = table(p$predictions == test[["dec"]])
#       new_incorrect = t[[1]]
#       nums[i] = nrow(test)
#       fracs[i] = new_incorrect/nrow(test)
#       num_incorrect = num_incorrect + new_incorrect
#       total_num = total_num + nrow(test)
    }
  }
  print(num_incorrect/total_num)
  return(num_incorrect/total_num)

}
features = c("decAvgExc", "partnerDecAvgExc", "attr", "fun", "like")
s = 0
for(i in 1:5){
  s = s +  f(features)
}
print(s/5)



