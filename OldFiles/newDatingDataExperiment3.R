

library(gdata)
library(ggplot2)
library(miscTools)
library(xlsx)
library(hash)
library(aod)
library(randomForest)
library(LiblineaR)
library(Matrix)
library(ddply)
library(plyr)

men <- read.csv('~/Desktop/new_men.csv')
women <- read.csv('~/Desktop/new_women.csv')

attrs = ['match', 'int_corr', 'samerace', 'age_o', 'race_o', 'pf_o_att', 'pf_o_sin', 'pf_o_int', 'pf_o_fun', 'pf_o_amb', 'pf_o_sha', 'attr_o', 'sinc_o', 'intel_o', 'fun_o', 'amb_o', 'shar_o', 'like_o', 'prob_o', 'met_o', 'age', 'race', 'imprace', 'imprelig', 'goal', 'date', 'go_out', 'sports', 'tvsports', 'exercise', 'dining', 'museums', 'art', 'hiking', 'gaming', 'clubbing', 'reading', 'tv', 'theater', 'movies', 'concerts', 'music', 'shopping', 'yoga', 'exphappy', 'expnum', 'attr1_1', 'sinc1_1', 'intel1_1', 'fun1_1', 'amb1_1', 'shar1_1', 'attr3_1', 'sinc3_1', 'fun3_1', 'intel3_1', 'amb3_1', 'attr5_1', 'sinc5_1', 'intel5_1', 'fun5_1', 'amb5_1', 'attr', 'sinc', 'intel', 'fun', 'amb', 'shar', 'like', 'prob', 'met']


for(i in 1:5){  
  women= women[sample(nrow(women)),]
  women_train = women[seq(1, nrow(women)/2),] 
  women_test = women[seq(nrow(women)/2 + 1, nrow(women)),]
  w_fit <- randomForest(as.factor(dec_o.x) ~  attr_o.x + dec_oAvg.x + fun_o.x + go_out.x,
                        data=women_train, importance=TRUE, ntree=2000)
  women_test["predicted_decision"] <- predict(w_fit, women_test)
  s = nrow(women_test[women_test[["dec_o.x"]] == 0,])/nrow(women_test)
  t = nrow(women_test[women_test[["predicted_decision"]] == women_test[["dec_o.x"]],])/nrow(women_test)
  print(c(1 - s, 1 - t,(1-s)/(1 - t),t-s))
}

for(i in 10:15){
  women= women[sample(nrow(women)),]
  women_train = women[seq(1, nrow(women)/2),] 
  women_test = women[seq(nrow(women)/2 + 1, nrow(women)),]
  mylogit <- glm(dec_o.x ~ attr_oOthersAvg.x + dec_oOthersAvg.x + go_out.x + date.x, data = women_train, family = "binomial")
  women_test$predicted_decision <- predict(mylogit, newdata = women_test, type = "response")
  women_test["predicted_decision"] <- ifelse(women_test["predicted_decision"]>= 0.5,1, 0)
  s = nrow(women_test[women_test["dec_o.x"] == 0,])/nrow(women_test)
  t = nrow(women_test[women_test[["predicted_decision"]] == women_test[["dec_o.x"]],])/nrow(women_test)
  print(c(1 - s, 1 - t,(1-s)/(1 - t),t-s))
}


for(i in 1:5){
  men = men[sample(nrow(men)),]
  
  men_train = men[seq(1, nrow(men)/2),] 
  men_test = men[seq(nrow(men)/2 + 1, nrow(men)),]
  m_fit <- randomForest(as.factor(dec_o) ~ attr_oMean + dec_oMean + go_out + date, data=men_train, importance=TRUE, ntree=2000) 
  men_test["predicted_decision"] <- as.numeric(as.character(predict(m_fit, men_test)))
  s =  nrow(men_test[men_test[["dec_o"]] == 0,])/nrow(men_test)
  t = nrow(men_test[men_test[["predicted_decision"]] == men_test[["dec_o"]],])/nrow(men_test) 
  print(c(1 - s, 1- t,(1-s)/(1 - t),t-s))
}



for(i in 10:15){
  men = men[sample(nrow(men)),]
  men_train = men[seq(1, nrow(men)/2),] 
  men_test = men[seq(nrow(men)/2 + 1, nrow(men)),]
  mylogit <- glm(dec_o.x ~ attr_oOthersAvg.x + dec_oOthersAvg.x + go_out.x + date.x, data = men_train, family = "binomial")
  men_test$predicted_decision = predict(mylogit, newdata = men_test, type = "response")
  men_test["predicted_decision"] = ifelse(men_test["predicted_decision"]>= 0.5,1, 0)
  s = nrow(men_test[men_test["dec_o.x"] == 0,])/nrow(men_test)
  t = nrow(men_test[men_test[["predicted_decision"]] == men_test[["dec_o.x"]],])/nrow(men_test)
  print(c(1 - s, 1 - t,(1-s)/(1 - t),t-s))
}   
men <- read.csv('~/Desktop/new_men.csv')
men = men[sample(nrow(men)),]
men_train = men[seq(1, nrow(men)/2),] 
men_test = men[seq(nrow(men)/2 + 1, nrow(men)),]
m_fit <- randomForest(as.factor(dec_o.x) ~ dec_oOthersAvg.x + attr_oOthersAvg.x + go_out.x + date.x, data=men_train, importance=TRUE, ntree=2000) 
men_test["predicted_decision"] <- as.numeric(as.character(predict(m_fit, men_test)))
s =  nrow(men_test[men_test[["dec_o.x"]] == 0,])/nrow(men_test)
t = nrow(men_test[men_test[["predicted_decision"]] == men_test[["dec_o.x"]],])/nrow(men_test) 
print(c(s,t,(1-s)/(1 - t),t-s))

# men["num"] = 1


