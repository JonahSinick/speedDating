# 
# 
# library(gdata)
# library(ggplot2)
# library(miscTools)
# library(xlsx)
# library(hash)
# library(aod)
# library(randomForest)
# library(LiblineaR)
# library(Matrix)
# library(plyr)
# library(recommenderlab)
# 
# men <- read.csv('~/Desktop/new_men.csv')
# women <- read.csv('~/Desktop/new_women.csv')
# 
# 
# 
# men = ddply(men, .(iid), transform, int_corr.mean = mean(int_corr))
# men = ddply(men, .(iid), transform, pf_o_att.mean = mean(pf_o_att))
# men = ddply(men, .(iid), transform, pf_o_sin.mean = mean(pf_o_sin))
# men = ddply(men, .(iid), transform, pf_o_int.mean = mean(pf_o_int))
# men = ddply(men, .(iid), transform, pf_o_fun.mean = mean(pf_o_fun))
# men = ddply(men, .(iid), transform, pf_o_amb.mean = mean(pf_o_amb))
# men = ddply(men, .(iid), transform, pf_o_sha.mean = mean(pf_o_sha))
# men = ddply(men, .(iid), transform, attr_o.mean = mean(attr_o))
# men = ddply(men, .(iid), transform, sinc_o.mean = mean(sinc_o))
# men = ddply(men, .(iid), transform, intel_o.mean = mean(intel_o))
# men = ddply(men, .(iid), transform, amb_o.mean = mean(amb_o))
# men = ddply(men, .(iid), transform, fun_o.mean = mean(fun_o))
# men = ddply(men, .(iid), transform, shar_o.mean = mean(shar_o))
# men = ddply(men, .(iid), transform, like_o.mean = mean(like_o))
# men = ddply(men, .(iid), transform, prob_o.mean = mean(prob_o))
# men = ddply(men, .(iid), transform, dec_o.mean = mean(dec_o))
# men = ddply(men, .(iid), transform, imprace.mean = mean(imprace))
# men = ddply(men, .(iid), transform, imprelig.mean = mean(imprelig))
# men = ddply(men, .(iid), transform, attr.mean = mean(attr))
# men = ddply(men, .(iid), transform, sinc.mean = mean(sinc))
# men = ddply(men, .(iid), transform, intel.mean = mean(intel))
# men = ddply(men, .(iid), transform, amb.mean = mean(amb))
# men = ddply(men, .(iid), transform, shar.mean = mean(shar))
# men = ddply(men, .(iid), transform, fun.mean = mean(fun))
# men = ddply(men, .(iid), transform, like.mean = mean(like))
# men = ddply(men, .(iid), transform, prob.mean = mean(prob))
# men = ddply(men, .(iid), transform, dec.mean = mean(dec))
# men = ddply(men, .(iid), transform, fun5_1.mean = mean(fun5_1))
# men = ddply(men, .(iid), transform, attr5_1.mean = mean(attr5_1))
# men = ddply(men, .(iid), transform, sinc5_1.mean = mean(sinc5_1))
# men = ddply(men, .(iid), transform, intel5_1.mean = mean(intel5_1))
# men = ddply(men, .(iid), transform, amb5_1.mean = mean(amb5_1))
# men = ddply(men, .(iid), transform, attr3_1.mean = mean(attr3_1))
# men = ddply(men, .(iid), transform, fun3_1.mean = mean(fun3_1))
# men = ddply(men, .(iid), transform, sinc3_1.mean = mean(sinc3_1))
# men = ddply(men, .(iid), transform, intel3_1.mean = mean(intel3_1))
# men = ddply(men, .(iid), transform, amb3_1.mean = mean(amb3_1))
# men = ddply(men, .(iid), transform, sports.mean = mean(sports))
# men = ddply(men, .(iid), transform, tvsports.mean = mean(tvsports))
# men = ddply(men, .(iid), transform, exercise.mean = mean(exercise))
# men = ddply(men, .(iid), transform, dining.mean = mean(dining))
# men = ddply(men, .(iid), transform, museums.mean = mean(museums))
# men = ddply(men, .(iid), transform, art.mean = mean(art))
# men = ddply(men, .(iid), transform, hiking.mean = mean(hiking))
# men = ddply(men, .(iid), transform, gaming.mean = mean(gaming))
# men = ddply(men, .(iid), transform, clubbing.mean = mean(clubbing))
# men = ddply(men, .(iid), transform, reading.mean = mean(reading))
# men = ddply(men, .(iid), transform, tv.mean = mean(tv))
# men = ddply(men, .(iid), transform, theater.mean = mean(theater))
# men = ddply(men, .(iid), transform, movies.mean = mean(movies))
# men = ddply(men, .(iid), transform, concerts.mean = mean(concerts))
# men = ddply(men, .(iid), transform, music.mean = mean(music))
# men = ddply(men, .(iid), transform, shopping.mean = mean(shopping))
# men = ddply(men, .(iid), transform, yoga.mean = mean(yoga))
# men = ddply(men, .(iid), transform, exphappy.mean = mean(exphappy))
# men = ddply(men, .(iid), transform, expnum.mean = mean(expnum))
# men = ddply(men, .(iid), transform, fun1_1.mean = mean(fun1_1))
# men = ddply(men, .(iid), transform, attr1_1.mean = mean(attr1_1))
# men = ddply(men, .(iid), transform, sinc1_1.mean = mean(sinc1_1))
# men = ddply(men, .(iid), transform, intel1_1.mean = mean(intel1_1))
# men = ddply(men, .(iid), transform, amb1_1.mean = mean(amb1_1))
# 
# 
# women = ddply(women, .(iid), transform, int_corr.mean = mean(int_corr))
# women = ddply(women, .(iid), transform, pf_o_att.mean = mean(pf_o_att))
# women = ddply(women, .(iid), transform, pf_o_sin.mean = mean(pf_o_sin))
# women = ddply(women, .(iid), transform, pf_o_int.mean = mean(pf_o_int))
# women = ddply(women, .(iid), transform, pf_o_fun.mean = mean(pf_o_fun))
# women = ddply(women, .(iid), transform, pf_o_amb.mean = mean(pf_o_amb))
# women = ddply(women, .(iid), transform, pf_o_sha.mean = mean(pf_o_sha))
# women = ddply(women, .(iid), transform, attr_o.mean = mean(attr_o))
# women = ddply(women, .(iid), transform, sinc_o.mean = mean(sinc_o))
# women = ddply(women, .(iid), transform, fun_o.mean = mean(fun_o))
# women = ddply(women, .(iid), transform, intel_o.mean = mean(intel_o))
# women = ddply(women, .(iid), transform, amb_o.mean = mean(amb_o))
# women = ddply(women, .(iid), transform, shar_o.mean = mean(shar_o))
# women = ddply(women, .(iid), transform, like_o.mean = mean(like_o))
# women = ddply(women, .(iid), transform, prob_o.mean = mean(prob_o))
# women = ddply(women, .(iid), transform, dec_o.mean = mean(dec_o))
# women = ddply(women, .(iid), transform, imprace.mean = mean(imprace))
# women = ddply(women, .(iid), transform, imprelig.mean = mean(imprelig))
# women = ddply(women, .(iid), transform, attr.mean = mean(attr))
# women = ddply(women, .(iid), transform, sinc.mean = mean(sinc))
# women = ddply(women, .(iid), transform, intel.mean = mean(intel))
# women = ddply(women, .(iid), transform, amb.mean = mean(amb))
# women = ddply(women, .(iid), transform, shar.mean = mean(shar))
# women = ddply(women, .(iid), transform, like.mean = mean(like))
# women = ddply(women, .(iid), transform, prob.mean = mean(prob))
# women = ddply(women, .(iid), transform, fun.mean = mean(fun))
# women = ddply(women, .(iid), transform, dec.mean = mean(dec))
# women = ddply(women, .(iid), transform, attr5_1.mean = mean(attr5_1))
# women = ddply(women, .(iid), transform, sinc5_1.mean = mean(sinc5_1))
# women = ddply(women, .(iid), transform, intel5_1.mean = mean(intel5_1))
# women = ddply(women, .(iid), transform, amb5_1.mean = mean(amb5_1))
# women = ddply(women, .(iid), transform, fun5_1.mean = mean(fun5_1))
# women = ddply(women, .(iid), transform, attr3_1.mean = mean(attr3_1))
# women = ddply(women, .(iid), transform, sinc3_1.mean = mean(sinc3_1))
# women = ddply(women, .(iid), transform, intel3_1.mean = mean(intel3_1))
# women = ddply(women, .(iid), transform, amb3_1.mean = mean(amb3_1))
# women = ddply(women, .(iid), transform, fun3_1.mean = mean(fun3_1))
# women = ddply(women, .(iid), transform, sports.mean = mean(sports))
# women = ddply(women, .(iid), transform, tvsports.mean = mean(tvsports))
# women = ddply(women, .(iid), transform, exercise.mean = mean(exercise))
# women = ddply(women, .(iid), transform, dining.mean = mean(dining))
# women = ddply(women, .(iid), transform, museums.mean = mean(museums))
# women = ddply(women, .(iid), transform, art.mean = mean(art))
# women = ddply(women, .(iid), transform, hiking.mean = mean(hiking))
# women = ddply(women, .(iid), transform, gaming.mean = mean(gaming))
# women = ddply(women, .(iid), transform, clubbing.mean = mean(clubbing))
# women = ddply(women, .(iid), transform, reading.mean = mean(reading))
# women = ddply(women, .(iid), transform, tv.mean = mean(tv))
# women = ddply(women, .(iid), transform, theater.mean = mean(theater))
# women = ddply(women, .(iid), transform, movies.mean = mean(movies))
# women = ddply(women, .(iid), transform, concerts.mean = mean(concerts))
# women = ddply(women, .(iid), transform, music.mean = mean(music))
# women = ddply(women, .(iid), transform, shopping.mean = mean(shopping))
# women = ddply(women, .(iid), transform, yoga.mean = mean(yoga))
# women = ddply(women, .(iid), transform, exphappy.mean = mean(exphappy))
# women = ddply(women, .(iid), transform, expnum.mean = mean(expnum))
# women = ddply(women, .(iid), transform, attr1_1.mean = mean(attr1_1))
# women = ddply(women, .(iid), transform, sinc1_1.mean = mean(sinc1_1))
# women = ddply(women, .(iid), transform, intel1_1.mean = mean(intel1_1))
# women = ddply(women, .(iid), transform, amb1_1.mean = mean(amb1_1))
# women = ddply(women, .(iid), transform, fun1_1.mean = mean(fun1_1))

# attrs = c('int_corr', 'pf_o_att', 'pf_o_sin', 'pf_o_int', 'pf_o_fun', 'pf_o_amb', 'pf_o_sha', 'attr_o', 'sinc_o', 'intel_o', 'fun_o', 'amb_o', 'shar_o', 'like_o', 'prob_o',  'imprace', 'imprelig', 'sports', 'tvsports', 'exercise', 'dining', 'museums', 'art', 'hiking', 'gaming', 'clubbing', 'reading', 'tv', 'theater', 'movies', 'concerts', 'music', 'shopping', 'yoga', 'exphappy', 'expnum', 'attr1_1', 'sinc1_1', 'intel1_1', 'fun1_1', 'amb1_1', 'shar1_1', 'attr3_1', 'sinc3_1', 'fun3_1', 'intel3_1', 'amb3_1', 'attr5_1', 'sinc5_1', 'intel5_1', 'fun5_1', 'amb5_1', 'attr', 'sinc', 'intel', 'fun', 'amb', 'shar', 'like', 'prob', "dec_o", "dec" )
# attrs  = c('attr', 'sinc', 'intel', 'fun', 'amb', 'shar', 'like', 'prob', "dec_o", "dec")
# for(at in attrs){
#   print(at)
#   men[paste(at,"New",sep="")] = men[at] - men[paste(at,".mean",sep="")]
#   women[paste(at,".New",sep="")]	= women[at] - women[paste(at,".mean",sep="")]
# }
# 
# write.csv(men, '~/Desktop/newest_men.csv')
# write.csv(women, '~/Desktop/newest_women.csv')

base_men = read.csv('~/Desktop/newest_men.csv')
base_women = read.csv('~/Desktop/newest_women.csv')

for(i in 1:2){  
  women= women[sample(nrow(women)),]
  women_train = women[seq(1, nrow(women)/2),] 
  women_test = women[seq(nrow(women)/2 + 1, nrow(women)),]
  w_fit <- randomForest(as.factor(dec_o) ~  attr_o.mean + dec_o.mean + go_out + date,
                        data=women_train, importance=TRUE, ntree=2000)
  women_test["predicted_decision"] <- predict(w_fit, women_test)
  s = nrow(women_test[women_test[["dec_o"]] == 0,])/nrow(women_test)
  t = nrow(women_test[women_test[["predicted_decision"]] == women_test[["dec_o"]],])/nrow(women_test)
  print(c(1 - s, 1 - t,(1-s)/(1 - t),t-s))
}


for(i in 1:2){  
  men= men[sample(nrow(men)),]
  men_train = men[seq(1, nrow(men)/2),] 
  men_test = men[seq(nrow(men)/2 + 1, nrow(men)),]
  m_fit <- randomForest(as.factor(dec_o) ~  like_o.mean +  dec_o.mean + attr_o.mean + dec,
                        data=men_train, importance=TRUE, ntree=2000)
  men_test["predicted_decision"] <- predict(w_fit, men_test)
  s = nrow(men_test[men_test[["dec_o"]] == 0,])/nrow(men_test)
  t = nrow(men_test[men_test[["predicted_decision"]] == men_test[["dec_o"]],])/nrow(men_test)
  print(c(1 - s, 1 - t,(1-s)/(1 - t),t-s))
}

for(i in 10:15){
  men= men[sample(nrow(men)),]
  men_train = men[seq(1, nrow(men)/2),] 
  men_test = men[seq(nrow(men)/2 + 1, nrow(men)),]
  mylogit <- glm(dec_o ~ like_o.mean +  dec_o.mean + attr_o.mean, data = men_train, family = "binomial")
  men_test$predicted_decision <- predict(mylogit, newdata = men_test, type = "response")
  men_test["predicted_decision"] <- ifelse(men_test["predicted_decision"]>= 0.5,1, 0)
  s = nrow(men_test[men_test["dec_o"] == 0,])/nrow(men_test)
  t = nrow(men_test[men_test[["predicted_decision"]] == men_test[["dec_o"]],])/nrow(men_test)
  print(c(1 - s, 1 - t,(1-s)/(1 - t),t-s))
}
for(i in 10:15){
  women= women[sample(nrow(women)),]
  women_train = women[seq(1, nrow(women)/2),] 
  women_test = women[seq(nrow(women)/2 + 1, nrow(women)),]
  mylogit <- glm(dec_o ~ attr_o.mean + dec_o.mean + fun_o.mean, data = women_train, family = "binomial")
  women_test$predicted_decision <- predict(mylogit, newdata = women_test, type = "response")
  women_test["predicted_decision"] <- ifelse(women_test["predicted_decision"]>= 0.5,1, 0)
  s = nrow(women_test[women_test["dec_o"] == 0,])/nrow(women_test)
  t = nrow(women_test[women_test[["predicted_decision"]] == women_test[["dec_o"]],])/nrow(women_test)
  print(c(1 - s, 1 - t,(1-s)/(1 - t),t-s))
}



# for(i in 1:5){  
#   women= women[sample(nrow(women)),]
#   women_train = women[seq(1, nrow(women)/2),] 
#   women_test = women[seq(nrow(women)/2 + 1, nrow(women)),]
#   w_fit <- randomForest(as.factor(dec_o.x) ~  attr_o.x + dec_oAvg.x + fun_o.x + go_out.x,
#                         data=women_train, importance=TRUE, ntree=2000)
#   women_test["predicted_decision"] <- predict(w_fit, women_test)
#   s = nrow(women_test[women_test[["dec_o.x"]] == 0,])/nrow(women_test)
#   t = nrow(women_test[women_test[["predicted_decision"]] == women_test[["dec_o.x"]],])/nrow(women_test)
#   print(c(1 - s, 1 - t,(1-s)/(1 - t),t-s))
# }
# 
# for(i in 10:15){
#   women= women[sample(nrow(women)),]
#   women_train = women[seq(1, nrow(women)/2),] 
#   women_test = women[seq(nrow(women)/2 + 1, nrow(women)),]
#   mylogit <- glm(dec_o.x ~ attr_oOthersAvg.x + dec_oOthersAvg.x + go_out.x + date.x, data = women_train, family = "binomial")
#   women_test$predicted_decision <- predict(mylogit, newdata = women_test, type = "response")
#   women_test["predicted_decision"] <- ifelse(women_test["predicted_decision"]>= 0.5,1, 0)
#   s = nrow(women_test[women_test["dec_o.x"] == 0,])/nrow(women_test)
#   t = nrow(women_test[women_test[["predicted_decision"]] == women_test[["dec_o.x"]],])/nrow(women_test)
#   print(c(1 - s, 1 - t,(1-s)/(1 - t),t-s))
# }
# 
# 
# for(i in 1:5){
#   men = men[sample(nrow(men)),]
#   
#   men_train = men[seq(1, nrow(men)/2),] 
#   men_test = men[seq(nrow(men)/2 + 1, nrow(men)),]
#   m_fit <- randomForest(as.factor(dec_o) ~ attr_oMean + dec_oMean + go_out + date, data=men_train, importance=TRUE, ntree=2000) 
#   men_test["predicted_decision"] <- as.numeric(as.character(predict(m_fit, men_test)))
#   s =  nrow(men_test[men_test[["dec_o"]] == 0,])/nrow(men_test)
#   t = nrow(men_test[men_test[["predicted_decision"]] == men_test[["dec_o"]],])/nrow(men_test) 
#   print(c(1 - s, 1- t,(1-s)/(1 - t),t-s))
# }
# 
# 
# 
# for(i in 10:15){
#   men = men[sample(nrow(men)),]
#   men_train = men[seq(1, nrow(men)/2),] 
#   men_test = men[seq(nrow(men)/2 + 1, nrow(men)),]
#   mylogit <- glm(dec_o.x ~ attr_oOthersAvg.x + dec_oOthersAvg.x + go_out.x + date.x, data = men_train, family = "binomial")
#   men_test$predicted_decision = predict(mylogit, newdata = men_test, type = "response")
#   men_test["predicted_decision"] = ifelse(men_test["predicted_decision"]>= 0.5,1, 0)
#   s = nrow(men_test[men_test["dec_o.x"] == 0,])/nrow(men_test)
#   t = nrow(men_test[men_test[["predicted_decision"]] == men_test[["dec_o.x"]],])/nrow(men_test)
#   print(c(1 - s, 1 - t,(1-s)/(1 - t),t-s))
# }   
# men <- read.csv('~/Desktop/new_men.csv')
# men = men[sample(nrow(men)),]
# men_train = men[seq(1, nrow(men)/2),] 
# men_test = men[seq(nrow(men)/2 + 1, nrow(men)),]
# m_fit <- randomForest(as.factor(dec_o.x) ~ dec_oOthersAvg.x + attr_oOthersAvg.x + go_out.x + date.x, data=men_train, importance=TRUE, ntree=2000) 
# men_test["predicted_decision"] <- as.numeric(as.character(predict(m_fit, men_test)))
# s =  nrow(men_test[men_test[["dec_o.x"]] == 0,])/nrow(men_test)
# t = nrow(men_test[men_test[["predicted_decision"]] == men_test[["dec_o.x"]],])/nrow(men_test) 
# print(c(s,t,(1-s)/(1 - t),t-s))
# 
# # men["num"] = 1
# 
# 
