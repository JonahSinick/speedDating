

library(gdata)
library(ggplot2)
library(miscTools)
library(xlsx)
library(hash)
library(aod)
library(randomForest)
library(LiblineaR)
library(Matrix)


men <- read.csv('~/Desktop/new_men.csv')
women <- read.csv('~/Desktop/new_women.csv')

men = na.omit(men)
women = na.omit(women)



for(i in 10:15){
    
  women = women[sample(nrow(women)),]
  women_train = women[seq(1, nrow(women)/2),] 
  women_test = women[seq(nrow(women)/2 + 1, nrow(women)),]
  w_fit <- randomForest(as.factor(dec_o) ~  attr_oMean + dec_oMean + fun_oMean + like_oMean,
                        data=women_train, importance=TRUE, ntree=2000)
  women_test["predicted_decision"] <- predict(w_fit, women_test)
  s = nrow(women_test[women_test[["dec_o"]] == 0,])/nrow(women_test)
  t = nrow(women_test[women_test[["predicted_decision"]] == women_test[["dec_o"]],])/nrow(women_test)
  print(c(1-s,1-t,(1-s)/(1 - t),t-s))
}    



  men = men[sample(nrow(men)),]
  
  men_train = men[seq(1, nrow(men)/2),] 
  men_test = men[seq(nrow(men)/2 + 1, nrow(men)),]
  m_fit <- randomForest(as.factor(dec_o.x) ~ attr_o.x, data=men_train, importance=TRUE, ntree=2000) 
  men_test["predicted_decision"] <- as.numeric(as.character(predict(m_fit, men_test)))
  s =  nrow(men_test[men_test[["dec_o.x"]] == 0,])/nrow(men_test)
  t = nrow(men_test[men_test[["predicted_decision"]] == men_test[["dec_o.x"]],])/nrow(men_test) 
  print(c(1 - s,1 - t,(1-s)/(1 - t),t-s))

# men["num"] = 1
# men_test["correct?"] = 1 - abs(men_test[["dec_o"]] - men_test[["predicted_decision"]])
# aggregate(men_test, by = men_test["iid"], sum)
# agged = aggregate(men_test, by = men_test["iid"], sum)
# newagged = aggregate(men_test, by = men_test["iid"], FUN=length)
# agged["correct?"]/ newagged["correct?"]
# 
# 
# # print(ggplot(women_test) + aes_string(y="predicted_decision", x="dec_oOthersAvg.x") + geom_point() + geom_smooth(method="lm") + ggtitle("dec_OthersAvg/dec_o"))
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # print(ggplot(women_train) + aes_string(y="predicted_decision", x="dec_oOthersAvg.x") + geom_point() + geom_smooth(method="lm") + ggtitle("date/go_out"))
# # print(ggplot(women_train) + aes_string(x="dec_o.x", y="go_out.x") + geom_point() + geom_smooth(method="lm") + ggtitle("date/go_out"))
# # 
# # 
# 
# for(i in 10:15){
#   women= women[sample(nrow(women)),]
#   women_train = women[seq(1, nrow(women)/2),] 
#   women_test = women[seq(nrow(women)/2 + 1, nrow(women)),]
#   mylogit <- glm(dec_o.x ~ dec_oOthersAvg.x + dec_oOthersAvg.y + attr_oOthersAvg.x + attr_oOthersAvg.y, data = women_train, family = "binomial")
#   women_test$predicted_decision <- predict(mylogit, newdata = women_test, type = "response")
#   women_test["predicted_decision"] <- ifelse(women_test["predicted_decision"]>= 0.5,1, 0)
#   s = nrow(women_test[women_test["dec_o.x"] == 0,])/nrow(women_test)
#   t = nrow(women_test[women_test[["predicted_decision"]] == women_test[["dec_o.x"]],])/nrow(women_test)
#   print(c(s,t,(1-s)/(1 - t),t-s))
# }   
# 
# 
# 
# for(i in 10:15){
#   men = men[sample(nrow(men)),]
#   men_train = men[seq(1, nrow(men)/2),] 
#   men_test = men[seq(nrow(men)/2 + 1, nrow(men)),]
#   mylogit <- glm(dec_o.x ~ dec_oOthersAvg.x + dec_oOthersAvg.y + fun_oOthersAvg.x + attr_oOthersAvg.x, data = men_train, family = "binomial")
#   men_test$predicted_decision = predict(mylogit, newdata = men_test, type = "response")
#   men_test["predicted_decision"] = ifelse(men_test["predicted_decision"]>= 0.5,1, 0)
#   s = nrow(men_test[men_test["dec_o.x"] == 0,])/nrow(men_test)
#   t = nrow(men_test[men_test[["predicted_decision"]] == men_test[["dec_o.x"]],])/nrow(men_test)
#   print(c(s,t,(1-s)/(1 - t),t-s))
# }   
# 
# # 
# 
# 
