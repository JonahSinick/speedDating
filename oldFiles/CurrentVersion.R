

library(gdata)
library(ggplot2)
library(miscTools)
library(xlsx)
library(hash)
library(aod)
library(randomForest)
library(LiblineaR)
library(Matrix)

men <- read.csv('~/Desktop/men_matrix.csv') 
sample = men[men["wave"] == 11,]
head(sample)
# new_men <- read.csv('~/Desktop/new_men2.csv')
# # new_women <- read.csv('~/Desktop/new_women2.csv')
# men_cleaned <- read.csv('~/Desktop/men_cleaned.csv')
# women_cleaned <- read.csv('~/Desktop/women_cleaned.csv')
# 
# 
# men = na.omit(men)
# women = na.omit(women)
# 
# # temp = df[["iid"]]
# # attrs = c('age_o', 'pf_o_att', 'pf_o_sin', 'pf_o_int', 'pf_o_fun', 'pf_o_amb', 'pf_o_sha', 'dec_o', 'attr_o', 'sinc_o', 'intel_o', 'fun_o', 'amb_o', 'shar_o', 'like_o', 'prob_o', 'met_o', 'age', 'dec')
# # for(r in attrs){
# #     men[paste(r,"Mean",sep="")] = 0.0
# #     men[paste(r,"Adjusted",sep="")] = 0.0
# # }
# # 
# # 
# # agged = aggregate(men, by=men["iid"], FUN=mean )
# # women_agged = aggregate(men, by=men["iid"], FUN=mean )
# # for(r in attrs){
# #   for(i in agged[["iid"]]){
# #     print(i)
# #     men[men["iid"] == i,][paste(r,"Mean",sep="")] = agged[agged["iid"] ==i,][r][[1]]
# #     men[men["iid"] == i,][paste(r,"Adjusted",sep="")] = men[men["iid"] == i,][r][[1]] - agged[agged["iid"] ==i,][r][[1]]
# # 
# #   }
# # }
# # 
# # 
# # for(r in attrs){
# #   for(i in women_agged[["iid"]]){
# #     print(i)
# #     women[women["iid"] == i,][paste(r,"Mean",sep="")] = women_agged[women_agged["iid"] ==i,][r][[1]]
# #     women[women["iid"] == i,][paste(r,"Adjusted",sep="")] = women_agged[women["iid"] == i,][r][[1]] - women_agged[women_agged["iid"] ==i,][r][[1]]
# #   }
# # }
# # write.csv(men, '~/Desktop/new_men2.csv')
# # write.csv(women, '~/Desktop/new_women.csv')
# # 
# 
# # 
# # 
# # for(i in 10:15){
# #   
# #   women = women[sample(nrow(women)),]
# #   women_train = women[seq(1, nrow(women)/2),] 
# #   women_test = women[seq(nrow(women)/2 + 1, nrow(women)),]
# #   w_fit <- randomForest(as.factor(dec_o) ~  attr_oMean + dec_oMean + fun_oMean + like_oMean,
# #                         data=women_train, importance=TRUE, ntree=2000)
# #   women_test["predicted_decision"] <- predict(w_fit, women_test)
# #   s = nrow(women_test[women_test[["dec_o"]] == 0,])/nrow(women_test)
# #   t = nrow(women_test[women_test[["predicted_decision"]] == women_test[["dec_o"]],])/nrow(women_test)
# #   print(c(1-s,1-t,(1-s)/(1 - t),t-s))
# # }    
# # 
# new_men <- read.csv('~/Desktop/new_men2.csv')
# # new_women <- read.csv('~/Desktop/new_women2.csv')
# men_cleaned <- read.csv('~/Desktop/men_cleaned.csv')
# 
# men = new_men
# men = men[sample(nrow(men)),]
# men_train = men[seq(1, nrow(men)/2),] 
# men_test = men[seq(nrow(men)/2 + 1, nrow(men)),]
# m_fit <- glm(dec_o ~ dec_oMean + attr_oMean + like_oMean + sinc_oMean + intel_oMean, data = men_train, family = "binomial")
# men_test["predicted_decision"] <- as.numeric(as.character(predict(m_fit, men_test)))
# men_test["predicted_decision"] <- ifelse(men_test["predicted_decision"] < 0.5,0, 1 )
# s =  nrow(men_test[men_test[["dec_o"]] == 0,])/nrow(men_test)
# t = nrow(men_test[men_test[["predicted_decision"]] == men_test[["dec_o"]],])/nrow(men_test) 
# print(c(1 - s,1 - t,(1-s)/(1 -t),t-s))
# 
# men = men_cleaned
# men = men[sample(nrow(men)),]
# men_train = men[seq(1, nrow(men)/2),] 
#   men_test = men[seq(nrow(men)/2 + 1, nrow(men)),]
#   m_fit <- glm(dec_o.x ~ dec_oAvg.x  + attr_oAvg.x + sinc_oAvg.x + intel_oAvg.x +fun_oAvg.x + 
#                amb_oOthersAvg.x, data = men_train, family = "binomial")
# men_test["predicted_decision"] <- as.numeric(as.character(predict(m_fit, men_test)))
# men_test["predicted_decision"] <- ifelse(men_test["predicted_decision"] < 0.5,0, 1 )
# s =  nrow(men_test[men_test[["dec_o.x"]] == 0,])/nrow(men_test)
# t = nrow(men_test[men_test[["predicted_decision"]] == men_test[["dec_o.x"]],])/nrow(men_test) 
# print(c(1 - s,1 - t,(1-s)/(1 -t),t-s))
#   
# # 
# # 
# men = men[sample(nrow(men)),]
# 
# men_train = men[seq(1, nrow(men)/2),] 
# men_test = men[seq(nrow(men)/2 + 1, nrow(men)),]
# m_fit <- randomForest(as.factor(dec_oAdjusted) ~ like_oAdjusted + attr_oAdjusted + fun_oAdjusted + sinc_oAdjusted, data=men_train, importance=TRUE, ntree=2000) 
# men_test["predicted_decision"] <- as.numeric(as.character(predict(m_fit, men_test)))
# men_test["predicted_decision"] = men_test["predicted_decision"] + men_test["dec_oMean"]
# men_test["predicted_decision"] <- ifelse(men_test["predicted_decision"] < 0.5,0, 1 )
# s =  nrow(men_test[men_test[["dec_o"]] == 0,])/nrow(men_test)
# t = nrow(men_test[men_test[["predicted_decision"]] == men_test[["dec_o"]],])/nrow(men_test) 
# print(c(1 - s,1 - t,(1-s)/(1 -t),t-s))
# 
# # men["num"] = 1
# # # men_test["correct?"] = 1 - abs(men_test[["dec_o"]] - men_test[["predicted_decision"]])
# # # aggregate(men_test, by = men_test["iid"], sum)
# # # agged = aggregate(men_test, by = men_test["iid"], sum)
# # # newagged = aggregate(men_test, by = men_test["iid"], FUN=length)
# # # agged["correct?"]/ newagged["correct?"]
# # # 
# # # 
# # # # print(ggplot(women_test) + aes_string(y="predicted_decision", x="dec_oOthersAvg.x") + geom_point() + geom_smooth(method="lm") + ggtitle("dec_OthersAvg/dec_o"))
# # # # 
# # # # 
# # # # 
# # # # 
# # # # 
# # # # 
# # # # 
# # # # 
# # # # print(ggplot(women_train) + aes_string(y="predicted_decision", x="dec_oOthersAvg.x") + geom_point() + geom_smooth(method="lm") + ggtitle("date/go_out"))
# # # # print(ggplot(women_train) + aes_string(x="dec_o.x", y="go_out.x") + geom_point() + geom_smooth(method="lm") + ggtitle("date/go_out"))
# # # # 
# # # # 
# # # 
# # # for(i in 10:15){
# # #   women= women[sample(nrow(women)),]
# # #   women_train = women[seq(1, nrow(women)/2),] 
# # #   women_test = women[seq(nrow(women)/2 + 1, nrow(women)),]
# # #   mylogit <- glm(dec_o.x ~ dec_oOthersAvg.x + dec_oOthersAvg.y + attr_oOthersAvg.x + attr_oOthersAvg.y, data = women_train, family = "binomial")
# # #   women_test$predicted_decision <- predict(mylogit, newdata = women_test, type = "response")
# # #   women_test["predicted_decision"] <- ifelse(women_test["predicted_decision"]>= 0.5,1, 0)
# # #   s = nrow(women_test[women_test["dec_o.x"] == 0,])/nrow(women_test)
# # #   t = nrow(women_test[women_test[["predicted_decision"]] == women_test[["dec_o.x"]],])/nrow(women_test)
# # #   print(c(s,t,(1-s)/(1 - t),t-s))
# # # }   
# # # 
# # # 
# # # 
# # # for(i in 10:15){
# # #   men = men[sample(nrow(men)),]
# # #   men_train = men[seq(1, nrow(men)/2),] 
# 
# # # 
# # # # 
# # # 
# # # 
