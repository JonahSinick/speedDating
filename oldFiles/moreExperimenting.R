

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

men <- read.csv('~/Desktop/new_men.csv')
women <- read.csv('~/Desktop/new_women.csv')

men = ddply(men, .(iid), transform, int_corr.mean = mean(int_corr))
men = ddply(men, .(iid), transform, pf_o_att.mean = mean(pf_o_att))
men = ddply(men, .(iid), transform, pf_o_sin.mean = mean(pf_o_sin))
men = ddply(men, .(iid), transform, pf_o_int.mean = mean(pf_o_int))
men = ddply(men, .(iid), transform, pf_o_fun.mean = mean(pf_o_fun))
men = ddply(men, .(iid), transform, pf_o_amb.mean = mean(pf_o_amb))
men = ddply(men, .(iid), transform, pf_o_sha.mean = mean(pf_o_sha))
men = ddply(men, .(iid), transform, attr_o.mean = mean(attr_o))
men = ddply(men, .(iid), transform, sinc_o.mean = mean(sinc_o))
men = ddply(men, .(iid), transform, intel_o.mean = mean(intel_o))
men = ddply(men, .(iid), transform, amb_o.mean = mean(amb_o))
men = ddply(men, .(iid), transform, fun_o.mean = mean(fun_o))
men = ddply(men, .(iid), transform, shar_o.mean = mean(shar_o))
men = ddply(men, .(iid), transform, like_o.mean = mean(like_o))
men = ddply(men, .(iid), transform, prob_o.mean = mean(prob_o))
men = ddply(men, .(iid), transform, dec_o.mean = mean(dec_o))
men = ddply(men, .(iid), transform, imprace.mean = mean(imprace))
men = ddply(men, .(iid), transform, imprelig.mean = mean(imprelig))
men = ddply(men, .(iid), transform, attr.mean = mean(attr))
men = ddply(men, .(iid), transform, sinc.mean = mean(sinc))
men = ddply(men, .(iid), transform, intel.mean = mean(intel))
men = ddply(men, .(iid), transform, amb.mean = mean(amb))
men = ddply(men, .(iid), transform, shar.mean = mean(shar))
men = ddply(men, .(iid), transform, fun.mean = mean(fun))
men = ddply(men, .(iid), transform, like.mean = mean(like))
men = ddply(men, .(iid), transform, prob.mean = mean(prob))
men = ddply(men, .(iid), transform, dec.mean = mean(dec))
men = ddply(men, .(iid), transform, fun5_1.mean = mean(fun5_1))
men = ddply(men, .(iid), transform, attr5_1.mean = mean(attr5_1))
men = ddply(men, .(iid), transform, sinc5_1.mean = mean(sinc5_1))
men = ddply(men, .(iid), transform, intel5_1.mean = mean(intel5_1))
men = ddply(men, .(iid), transform, amb5_1.mean = mean(amb5_1))
men = ddply(men, .(iid), transform, attr3_1.mean = mean(attr3_1))
men = ddply(men, .(iid), transform, fun3_1.mean = mean(fun3_1))
men = ddply(men, .(iid), transform, sinc3_1.mean = mean(sinc3_1))
men = ddply(men, .(iid), transform, intel3_1.mean = mean(intel3_1))
men = ddply(men, .(iid), transform, amb3_1.mean = mean(amb3_1))
men = ddply(men, .(iid), transform, sports.mean = mean(sports))
men = ddply(men, .(iid), transform, tvsports.mean = mean(tvsports))
men = ddply(men, .(iid), transform, exercise.mean = mean(exercise))
men = ddply(men, .(iid), transform, dining.mean = mean(dining))
men = ddply(men, .(iid), transform, museums.mean = mean(museums))
men = ddply(men, .(iid), transform, art.mean = mean(art))
men = ddply(men, .(iid), transform, hiking.mean = mean(hiking))
men = ddply(men, .(iid), transform, gaming.mean = mean(gaming))
men = ddply(men, .(iid), transform, clubbing.mean = mean(clubbing))
men = ddply(men, .(iid), transform, reading.mean = mean(reading))
men = ddply(men, .(iid), transform, tv.mean = mean(tv))
men = ddply(men, .(iid), transform, theater.mean = mean(theater))
men = ddply(men, .(iid), transform, movies.mean = mean(movies))
men = ddply(men, .(iid), transform, concerts.mean = mean(concerts))
men = ddply(men, .(iid), transform, music.mean = mean(music))
men = ddply(men, .(iid), transform, shopping.mean = mean(shopping))
men = ddply(men, .(iid), transform, yoga.mean = mean(yoga))
men = ddply(men, .(iid), transform, exphappy.mean = mean(exphappy))
men = ddply(men, .(iid), transform, expnum.mean = mean(expnum))
men = ddply(men, .(iid), transform, fun1_1.mean = mean(fun1_1))
men = ddply(men, .(iid), transform, attr1_1.mean = mean(attr1_1))
men = ddply(men, .(iid), transform, sinc1_1.mean = mean(sinc1_1))
men = ddply(men, .(iid), transform, intel1_1.mean = mean(intel1_1))
men = ddply(men, .(iid), transform, amb1_1.mean = mean(amb1_1))


women = ddply(women, .(iid), transform, int_corr.mean = mean(int_corr))
women = ddply(women, .(iid), transform, pf_o_att.mean = mean(pf_o_att))
women = ddply(women, .(iid), transform, pf_o_sin.mean = mean(pf_o_sin))
women = ddply(women, .(iid), transform, pf_o_int.mean = mean(pf_o_int))
women = ddply(women, .(iid), transform, pf_o_fun.mean = mean(pf_o_fun))
women = ddply(women, .(iid), transform, pf_o_amb.mean = mean(pf_o_amb))
women = ddply(women, .(iid), transform, pf_o_sha.mean = mean(pf_o_sha))
women = ddply(women, .(iid), transform, attr_o.mean = mean(attr_o))
women = ddply(women, .(iid), transform, sinc_o.mean = mean(sinc_o))
women = ddply(women, .(iid), transform, fun_o.mean = mean(fun_o))
women = ddply(women, .(iid), transform, intel_o.mean = mean(intel_o))
women = ddply(women, .(iid), transform, amb_o.mean = mean(amb_o))
women = ddply(women, .(iid), transform, shar_o.mean = mean(shar_o))
women = ddply(women, .(iid), transform, like_o.mean = mean(like_o))
women = ddply(women, .(iid), transform, prob_o.mean = mean(prob_o))
women = ddply(women, .(iid), transform, dec_o.mean = mean(dec_o))
women = ddply(women, .(iid), transform, imprace.mean = mean(imprace))
women = ddply(women, .(iid), transform, imprelig.mean = mean(imprelig))
women = ddply(women, .(iid), transform, attr.mean = mean(attr))
women = ddply(women, .(iid), transform, sinc.mean = mean(sinc))
women = ddply(women, .(iid), transform, intel.mean = mean(intel))
women = ddply(women, .(iid), transform, amb.mean = mean(amb))
women = ddply(women, .(iid), transform, shar.mean = mean(shar))
women = ddply(women, .(iid), transform, like.mean = mean(like))
women = ddply(women, .(iid), transform, prob.mean = mean(prob))
women = ddply(women, .(iid), transform, fun.mean = mean(fun))
women = ddply(women, .(iid), transform, dec.mean = mean(dec))
women = ddply(women, .(iid), transform, attr5_1.mean = mean(attr5_1))
women = ddply(women, .(iid), transform, sinc5_1.mean = mean(sinc5_1))
women = ddply(women, .(iid), transform, intel5_1.mean = mean(intel5_1))
women = ddply(women, .(iid), transform, amb5_1.mean = mean(amb5_1))
women = ddply(women, .(iid), transform, fun5_1.mean = mean(fun5_1))
women = ddply(women, .(iid), transform, attr3_1.mean = mean(attr3_1))
women = ddply(women, .(iid), transform, sinc3_1.mean = mean(sinc3_1))
women = ddply(women, .(iid), transform, intel3_1.mean = mean(intel3_1))
women = ddply(women, .(iid), transform, amb3_1.mean = mean(amb3_1))
women = ddply(women, .(iid), transform, fun3_1.mean = mean(fun3_1))
women = ddply(women, .(iid), transform, sports.mean = mean(sports))
women = ddply(women, .(iid), transform, tvsports.mean = mean(tvsports))
women = ddply(women, .(iid), transform, exercise.mean = mean(exercise))
women = ddply(women, .(iid), transform, dining.mean = mean(dining))
women = ddply(women, .(iid), transform, museums.mean = mean(museums))
women = ddply(women, .(iid), transform, art.mean = mean(art))
women = ddply(women, .(iid), transform, hiking.mean = mean(hiking))
women = ddply(women, .(iid), transform, gaming.mean = mean(gaming))
women = ddply(women, .(iid), transform, clubbing.mean = mean(clubbing))
women = ddply(women, .(iid), transform, reading.mean = mean(reading))
women = ddply(women, .(iid), transform, tv.mean = mean(tv))
women = ddply(women, .(iid), transform, theater.mean = mean(theater))
women = ddply(women, .(iid), transform, movies.mean = mean(movies))
women = ddply(women, .(iid), transform, concerts.mean = mean(concerts))
women = ddply(women, .(iid), transform, music.mean = mean(music))
women = ddply(women, .(iid), transform, shopping.mean = mean(shopping))
women = ddply(women, .(iid), transform, yoga.mean = mean(yoga))
women = ddply(women, .(iid), transform, exphappy.mean = mean(exphappy))
women = ddply(women, .(iid), transform, expnum.mean = mean(expnum))
women = ddply(women, .(iid), transform, attr1_1.mean = mean(attr1_1))
women = ddply(women, .(iid), transform, sinc1_1.mean = mean(sinc1_1))
women = ddply(women, .(iid), transform, intel1_1.mean = mean(intel1_1))
women = ddply(women, .(iid), transform, amb1_1.mean = mean(amb1_1))
women = ddply(women, .(iid), transform, fun1_1.mean = mean(fun1_1))

attrs = c('int_corr', 'pf_o_att', 'pf_o_sin', 'pf_o_int', 'pf_o_fun', 
          'pf_o_amb', 'pf_o_sha', 'attr_o', 'sinc_o', 'intel_o', 'fun_o', 'amb_o', 
          'shar_o', 'like_o', 'prob_o', 'imprace', 'imprelig', 'sports', 'tvsports', 
          'exercise', 'dining', 'museums', 'art', 'hiking', 'gaming', 'clubbing', 
          'reading', 'tv', 'theater', 'movies', 'concerts', 'music', 'shopping', 'yoga', 
          'exphappy', 'expnum', 'attr1_1', 'sinc1_1', 'intel1_1', 'fun1_1', 'amb1_1', 
          'attr', 'sinc', 'intel', 'fun', 'amb', 'shar', 'like', 'prob', "dec_o", "dec" )



for(at in attrs){
  print(at)
  men[paste(at,"New.y",sep="")] = men[paste(at,".y",sep="")] - men[paste(at,".mean.y",sep="")]
  women[paste(at,"New.y",sep="")]  = women[paste(at,".y",sep="")] - women[paste(at,".mean.y",sep="")]
}
men = base_men
women = base_women

acts = c('sports', 'tvsports', 
         'exercise', 'dining', 'museums', 'art', 'hiking', 'gaming', 'clubbing', 
         'reading', 'tv', 'theater', 'movies', 'concerts', 'music', 'shopping', 'yoga')
for(at in acts){
  print(at)
  men[paste(at,"Inter",sep="")] = men[paste(at,".x",sep="")]*men[paste(at,".y",sep="")]
  women[paste(at,"Inter",sep="")] = women[paste(at,".x",sep="")]*women[paste(at,".y",sep="")]
}


base_men = merge(men, women, by.x = c("iid", "pid", "id", "partner"), by.y = c("pid", "iid", "partner", "id"), all = FALSE)
base_women = merge(women, men, by.x = c("iid", "pid", "id", "partner"), by.y = c("pid", "iid", "partner", "id"), all = FALSE)


# attrs = c('int_corr.x', 'pf_o_att.x', 'pf_o_sin.x', 'pf_o_int.x', 'pf_o_fun.x', 'pf_o_amb.x', 'pf_o_sha.x', 'attr_o.x', 'sinc_o.x', 'intel_o.x', 'fun_o.x', 'amb_o.x', 'shar_o.x', 'like_o.x', 'prob_o.x',  'imprace.x', 'imprelig.x', 'sports.x', 'tvsports.x', 'exercise.x', 'dining.x', 'museums.x', 'art.x', 'hiking.x', 'gaming.x', 'clubbing.x', 'reading.x', 'tv.x', 'theater.x', 'movies.x', 'concerts.x', 'music.x', 'shopping.x', 'yoga.x', 'exphappy.x', 'expnum.x', 'attr1_1.x', 'sinc1_1.x', 'intel1_1.x', 'fun1_1.x', 'amb1_1.x', 'attr.x', 'sinc.x', 'intel.x', 'fun.x', 'amb.x', 'shar.x', 'like.x', 'prob.x', "dec_o.x", "dec.x", 'int_corr.y', 'pf_o_att.y', 'pf_o_sin.y', 'pf_o_int.y', 'pf_o_fun.y', 'pf_o_amb.y', 'pf_o_sha.y', 'attr_o.y', 'sinc_o.y', 'intel_o.y', 'fun_o.y', 'amb_o.y', 'shar_o.y', 'like_o.y', 'prob_o.y',  'imprace.y', 'imprelig.y', 'sports.y', 'tvsports.y', 'exercise.y', 'dining.y', 'museums.y', 'art.y', 'hiking.y', 'gaming.y', 'clubbing.y', 'reading.y', 'tv.y', 'theater.y', 'movies.y', 'concerts.y', 'music.y', 'shopping.y', 'yoga.y', 'exphappy.y', 'expnum.y', 'attr1_1.y', 'sinc1_1.y', 'intel1_1.y', 'fun1_1.y', 'amb1_1.y', 'attr.y', 'sinc.y', 'intel.y', 'fun.y', 'amb.y', 'shar.y', 'like.y', 'prob.y', "dec_o.x", "dec.x" )

# attrs = c('int_corr', 'pf_o_att', 'pf_o_sin', 'pf_o_int', 'pf_o_fun', 'pf_o_amb', 'pf_o_sha', 'attr_o', 'sinc_o', 'intel_o', 'fun_o', 'amb_o', 'shar_o', 'like_o', 'prob_o',  'imprace', 'imprelig', 'sports', 'tvsports', 'exercise', 'dining', 'museums', 'art', 'hiking', 'gaming', 'clubbing', 'reading', 'tv', 'theater', 'movies', 'concerts', 'music', 'shopping', 'yoga', 'exphappy', 'expnum', 'attr1_1', 'sinc1_1', 'intel1_1', 'fun1_1', 'amb1_1', 'attr3_1', 'sinc3_1', 'fun3_1', 'intel3_1', 'amb3_1', 'attr5_1', 'sinc5_1', 'intel5_1', 'fun5_1', 'amb5_1', 'attr', 'sinc', 'intel', 'fun', 'amb', 'shar', 'like', 'prob', "dec_o", "dec" )
# attrs  = c('attr', 'sinc', 'intel', 'fun', 'amb', 'shar', 'like', 'prob', "dec_o", "dec")


write.csv(men, '~/Desktop/newest_men.csv')
write.csv(women, '~/Desktop/newest_women.csv')

base_men = read.csv('~/Desktop/newest_men.csv')
base_women = read.csv('~/Desktop/newest_women.csv')
women = base_women
# dining.x*dining.y + sports.x*sports.y + tvsports.x*tvsports.y + exercise.x*exercise.y + dining.x*dining.y + museums.x*museums.y + art.x*art.y + hiking.x*hiking.y + gaming.x*gaming.y  + clubbing.x*clubbing.y + reading.x*reading.y + tv.x*tv.y + 

for(i in 1:1){  
#   women= women[sample(nrow(women)),]
  women_train = women[seq(1, nrow(women)/2),] 
  women_test = women[seq(nrow(women)/2 + 1, nrow(women)),]
  w_fit <- randomForest(as.factor(dec_o.x) ~  dining.x + dining.y + sports.x + sports.y + tvsports.x + tvsports.y + exercise.x + exercise.y + dining.x + dining.y + museums.x + museums.y + art.x + art.y + hiking.x + hiking.y + gaming.x + gaming.y  + clubbing.x + clubbing.y + reading.x + reading.y + tv.x + tv.y + avg_act_interest.y + avg_act_interest.x,
                        data=women_train, importance=TRUE, ntree=2000)
  women_test["predicted_decision"] <- predict(w_fit, women_test)
  s = nrow(women_test[women_test[["dec_o.x"]] == 0,])/nrow(women_test)
  t = nrow(women_test[women_test[["predicted_decision"]] == women_test[["dec_o.x"]],])/nrow(women_test)
  print(c(1 - s, 1 - t,(1-s)/(1 - t),t-s))
}

men = base_men
men_train = men[seq(1, nrow(men)/2),] 
men_test = men[seq(nrow(men)/2 + 1, nrow(men)),]
m_fit <- randomForest(as.factor(dec_o.x) ~ dining.x*dining.y + sports.x*sports.y + tvsports.x*tvsports.y + exercise.x*exercise.y + dining.x*dining.y + museums.x*museums.y + art.x*art.y + hiking.x*hiking.y + gaming.x*gaming.y  + clubbing.x*clubbing.y + reading.x*reading.y + tv.x*tv.y + dining_normalized.x*dining_normalized.y + sports_normalized.x*sports_normalized.y + tvsports_normalized.x*tvsports_normalized.y + exercise_normalized.x*exercise_normalized.y + dining_normalized.x*dining_normalized.y + museums_normalized.x*museums_normalized.y + art_normalized.x*art_normalized.y + hiking_normalized.x*hiking_normalized.y + gaming_normalized.x*gaming_normalized.y  + clubbing_normalized.x*clubbing_normalized.y + reading_normalized.x*reading_normalized.y + tv_normalized.x*tv_normalized.y + avg_act_interest.y + avg_act_interest.x + avg_act_interest.y*avg_act_interest.x, data=men_train, importance=TRUE, 
                      ntree=200)
men_test["predicted_decision"] <- predict(m_fit, men_test)
s = nrow(men_test[men_test[["dec_o.x"]] == 0,])/nrow(men_test)
t = nrow(men_test[men_test[["predicted_decision"]] == men_test[["dec_o.x"]],])/nrow(men_test)
print(c(1 - s, 1 - t,(1-s)/(1 - t),t-s))

men = base_men
for(i in 10:15){
#   men= men[sample(nrow(men)),]
  men_train = men[seq(1, nrow(men)/2),] 
  men_test = men[seq(nrow(men)/2 + 1, nrow(men)),]
  mylogit <- glm(dec_o.x ~ dining.x + dining.y + sports.x + sports.y + tvsports.x + tvsports.y + exercise.x + exercise.y + dining.x + dining.y + museums.x + museums.y + art.x + art.y + hiking.x + hiking.y + gaming.x + gaming.y  + clubbing.x + clubbing.y + reading.x + reading.y + tv.x + tv.y + avg_act_interest.y + avg_act_interest.x, data = men_train, family = "binomial")
  men_test$predicted_decision <- predict(mylogit, newdata = men_test, type = "response")
  men_test["predicted_decision"] <- ifelse(men_test["predicted_decision"]>= 0.5,1, 0)
  s = nrow(men_test[men_test["dec_o.x"] == 0,])/nrow(men_test)
  t = nrow(men_test[men_test[["predicted_decision"]] == men_test[["dec_o.x"]],])/nrow(men_test)
  print(c(1 - s, 1 - t,(1-s)/(1 - t),t-s))
}

women = base_women
for(i in 10:15){
#   women= women[sample(nrow(women)),]
  
  women_train = women[seq(1, nrow(women)/2),] 
  women_test = women[seq(nrow(women)/2 + 1, nrow(women)),]
  mylogit <- glm(dec_o.x ~ dining.y + sports.y + tvsports.y + exercise.y + dining.y + museums.y + art.y + hiking.y + gaming.y + clubbing.y + reading.y + tv.y, data = women_train, family = "binomial")
  women_test$predicted_decision <- predict(mylogit, newdata = women_test, type = "response")
  women_test["predicted_decision"] <- ifelse(women_test["predicted_decision"]>= 0.5,1, 0)
  s = nrow(women_test[women_test["dec_o.x"] == 0,])/nrow(women_test)
  t = nrow(women_test[women_test[["predicted_decision"]] == women_test[["dec_o.x"]],])/nrow(women_test)
  print(c(1 - s, 1 - t,(1-s)/(1 - t),t-s))
  women_train$predicted_decision <- predict(mylogit, newdata = women_train, type = "response")
  women_train["predicted_decision"] <- ifelse(women_train["predicted_decision"]>= 0.5,1, 0)
  s = nrow(women_train[women_train[["dec_o.x"]] == 0,])/nrow(women_train)
  t = nrow(women_train[women_train[["predicted_decision"]] == women_train[["dec_o.x"]],])/nrow(women_train)
  print(c(1 - s, 1 - t,(1-s)/(1 - t),t-s))  
}


women = base_women
for(i in 10:15){
  women= women[sample(nrow(women)),]
  women_train = women[seq(1, nrow(women)/2),] 
  women_test = women[seq(nrow(women)/2 + 1, nrow(women)),]
  mylogit <- glm(dec_o.x ~ dining.x*dining.y + sports.x*sports.y + tvsports.x*tvsports.y + exercise.x*exercise.y + dining.x*dining.y + museums.x*museums.y + art.x*art.y + hiking.x*hiking.y + gaming.x*gaming.y  + clubbing.x*clubbing.y + reading.x*reading.y + tv.x*tv.y, data = women_train, family = "binomial")
  women_test$predicted_decision <- predict(mylogit, newdata = women_test, type = "response")
  women_test["predicted_decision"] <- ifelse(women_test["predicted_decision"]>= 0.5,1, 0)
  s = nrow(women_test[women_test["dec_o.x"] == 0,])/nrow(women_test)
  t = nrow(women_test[women_test[["predicted_decision"]] == women_test[["dec_o.x"]],])/nrow(women_test)
  print(c(1 - s, 1 - t,(1-s)/(1 - t),t-s))
}

expnum.x + exphappy.y + date.x  +
women = base_women

for(i in 1:5){  
#   women= women[sample(nrow(women)),]
  women_train = women[seq(1, nrow(women)/2),] 
  women_test = women[seq(nrow(women)/2 + 1, nrow(women)),]
#   w_fit <- randomForest(as.factor(dec_o.x) ~ attr_o.mean.x + like_o.mean.x + dec.mean.y + expnum.x + exphappy.y + date.x  + go_out.x*date.x + prob.y,
#                         data=women_train, importance=TRUE, ntree=2000)
#   w_fit  <- randomForest(as.factor(dec_o.x) ~ dining.x*dining.y + sports.x*sports.y + tvsports.x*tvsports.y + exercise.x*exercise.y + dining.x*dining.y + museums.x*museums.y + art.x*art.y + hiking.x*hiking.y + gaming.x*gaming.y  + clubbing.x*clubbing.y + reading.x*reading.y + tv.x*tv.y,
#                         data=women_train, importance=TRUE, ntree=2000)  
  w_fit  <- randomForest(as.factor(dec_o.x) ~  fun5_1.x + expnum.x + exphappy.y + date.x  +dining.x*dining.y + sports.x*sports.y + tvsports.x*tvsports.y + exercise.x*exercise.y + dining.x*dining.y + museums.x*museums.y + art.x*art.y + hiking.x*hiking.y + gaming.x*gaming.y  + clubbing.x*clubbing.y + reading.x*reading.y + tv.x*tv.y,
data=women_train, importance=TRUE, ntree=2000)  
  women_test["predicted_decision"] <- predict(w_fit, women_test)
  s = nrow(women_test[women_test[["dec_o.x"]] == 0,])/nrow(women_test)
  t = nrow(women_test[women_test[["predicted_decision"]] == women_test[["dec_o.x"]],])/nrow(women_test)
  print(c(1 - s, 1 - t,(1-s)/(1 - t),t-s))
}
