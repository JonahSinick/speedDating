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

drops = c("income", "zipcode", "career",  "field", "field_cd", "undergra", "mn_sat", "tuition", "X" ,  "idg" , "condtn" ,"wave" ,    "round" ,   "position" ,"positin1" ,"order" , "match", "attr_o",  "sinc_o",  "intel_o", "fun_o",   "amb_o",  "shar_o",  "like_o",  "prob_o", "attr_inter",      "sinc_inter", "intel_inter",     "fun_inter",       "amb_inter",       "shar_inter",      "dec_oSum",        "dec_oCount",     "attr_oSum",       "attr_oCount",     "sinc_oSum",       "sinc_oCount",     "intel_oSum",      "intel_oCount",   "fun_oSum",        "fun_oCount",      "amb_oSum",        "amb_oCount",      "shar_oSum",       "shar_oCount", "like_oSum",       "like_oCount",     "prob_oSum",       "prob_oCount",      "dec_oAvg",        "attr_oAvg",       "sinc_oAvg",       "intel_oAvg",      "fun_oAvg", "amb_oAvg",        "shar_oAvg",       "like_oAvg", "pf_o_att"      ,     "pf_o_sin"    ,       "pf_o_int",           "pf_o_fun"        ,   "pf_o_amb"    ,       "pf_o_sha", "career_c", "from")
df = df[,!(names(df) %in% drops)]
df = data.frame(apply(df,2,f))
keeps = c("wave", "gender", "iid", "pid", "id", "partner",  "dec_o", "attr_o", "sinc_o", "fun_o", "intel_o", "like_o", "amb_o")
df = df[keeps]
keeps_to_fix = c("dec_o", "attr_o", "sinc_o", "fun_o", "intel_o", "like_o", "amb_o")

df = data.frame(apply(df,2,f))

df = na.omit(df)


# 
# men = df[df["gender"] == 1,]
# 
# 
# men = ddply(men, .(wave) ,transform ,waveMeanDecO = mean(dec_o))
# men = ddply(men, .(wave) ,transform ,waveMeanFunO = mean(fun_o))
# men = ddply(men, .(wave) ,transform ,waveMeanSincO = mean(sinc_o))
# men = ddply(men, .(wave) ,transform ,waveMeanIntelO = mean(intel_o))
# men = ddply(men, .(wave) ,transform ,waveMeanAmbO = mean(amb_o))
# men = ddply(men, .(wave) ,transform ,waveMeanLikeO = mean(like_o))
# men = ddply(men, .(wave) ,transform ,waveMeanAttrO = mean(attr_o))
# 
# 
# men = ddply(men, .(iid) ,transform ,otherDecSum = sum(dec_o))
# men = ddply(men, .(pid) ,transform ,partnerDecSum = sum(dec_o))
# men = ddply(men, .(iid) ,transform ,otherAttrSum = sum(attr_o))
# men = ddply(men, .(iid) ,transform ,otherFunSum = sum(fun_o))
# men = ddply(men, .(iid) ,transform ,otherAmbSum = sum(amb_o))
# men = ddply(men, .(iid) ,transform ,otherLikeSum = sum(like_o))
# men = ddply(men, .(iid) ,transform ,otherIntelSum = sum(intel_o))
# men = ddply(men, .(iid) ,transform ,otherSincSum = sum(sinc_o))
# 
# men["otherDecAvg"] = men["otherDecSum"] - men["dec_o"] + men["waveMeanDecO"]
# men["partnerDecAvg"] = men["partnerDecSum"] - men["dec_o"] + men["waveMeanDecO"]
# men["otherAttrSum"] = men["otherAttrSum"] - men["attr_o"] + men["waveMeanAttrO"]
# men["otherSincSum"] = men["otherSincSum"] - men["sinc_o"] + men["waveMeanSincO"]
# men["otherAmbSum"] = men["otherAmbSum"] - men["amb_o"] + men["waveMeanAmbO"]
# men["otherIntelSum"] = men["otherIntelSum"] - men["intel_o"] + men["waveMeanIntelO"]
# men["otherFunSum"] = men["otherFunSum"] - men["fun_o"] + men["waveMeanFunO"]
# men["otherLikeSum"] = men["otherLikeSum"] - men["like_o"] + men["waveMeanLikeO"]
# 
# sequence = c(names(men)[1:7], names(men)[21:30])
# men = men[sequence]
# 
# 
# for(k in c("wave", "id", "pid", "dec_o")){
#   men[k, 1] = toString(men[k, 1])
# }
# men = na.omit(men)
# 
# big_total_num = 0
# big_total_incorrect = 0
#   for(i in 1:20){
# 
#     print(i)
#     total_num = 0
#     total_incorrect = 0
#     for(j in 1:10){
#       
#       wave = men[men["wave"] == toString(i),]
#       wave= wave[sample(nrow(wave)),]
#       wave_train = wave[seq(1, 9*nrow(wave)/10),] 
#       wave_test = wave[seq(9*nrow(wave)/10 +  1, nrow(wave)),] 
#       fit <- randomForest(as.factor(dec_o) ~ otherDecSum +  partnerDecSum + otherAttrSum + otherLikeSum +  otherFunSum, data=wave_train, importance=TRUE, ntree=500)
#       wave_test["prediction"]  <- predict(fit, wave_test)
#       t = table(wave_test["prediction"] == wave_test["dec_o"])
#       total_incorrect = total_incorrect + t[[1]]
#       total_num = total_num + nrow(wave_test)
#     }
#     print(total_incorrect/total_num)
#     big_total_num = big_total_num + total_num
#     big_total_incorrect = big_total_incorrect + total_incorrect
#   }
# print(big_total_incorrect/big_total_num)
# 
# 
# 
# 
# 
# for(i in 1:20){
#   print(i)
#   wave = merged[merged["wave"] == i,]
#   wave = processWave(wave)
#   wave = round(wave, 3)
#   write.csv(wave, paste("~/Desktop/waves/decisions",paste(toString(i),".csv",sep=""),sep=""))    
# }