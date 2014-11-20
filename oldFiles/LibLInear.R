

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



# base_men = read.csv('~/Desktop/menSuper.csv')
# base_women = read.csv('~/Desktop/womenSuper.csv')

base_men =read.csv("~/Desktop/menPartiallyProcessed.csv")
base_women = read.csv("~/Desktop/womenPartiallyProcessed.csv")
men = base_men
women = base_women
men = men[men["wave.x"] < 6 | men["wave.x"] > 10,]
women = women[women["wave.x"] < 6 | women["wave.x"] > 10,]



linearPredictor = function(df, target, features, ty){
  train = seq(1, nrow(df)/2)
  y = factor(df[,target])
  x = df[,features]
  xTrain=x[train,]
  xTest=x[-train,]
  yTrain=y[train]
  yTest=y[-train]
  s=scale(xTrain,center=TRUE,scale=TRUE)
  co=heuristicC(s)
  m=LiblineaR(data=s,labels=yTrain,type=ty,cost=co,bias=TRUE,verbose=FALSE)
  s2=scale(xTest,attr(s,"scaled:center"),attr(s,"scaled:scale"))
  p=predict(m,s2)
  s = table(yTest == 0)[[1]]/length(yTest)
  t = table(yTest ==p$predictions)[[1]]/length(yTest)
  print(s/t)
  return(m) 
}





randForestPredictor = function(df, target, features, num_trees){
  train = seq(1, nrow(df)/2)
  dfTrain = df[train,]
  dfTest = df[-train,]  
  rf_fit <- randomForest(y=as.factor(dfTrain[,target]), x=dfTrain[features], importance=TRUE, ntree=num_trees)
  dfTest[paste("predicted",target,sep="_")] <- predict(rf_fit, dfTest)
  s = table(dfTest[target] == 0)[[1]]/nrow(dfTest)
  t = (table(dfTest[paste("predicted",target,sep="_")] == dfTest[target])[[1]])/nrow(dfTest)
  print(s/t)
  return(rf_fit)
}

y_acts_normed = c("sports_normalized.y",   "tvsports_normalized.y", "exercise_normalized.y", "dining_normalized.y",  "museums_normalized.y",  "art_normalized.y",      "hiking_normalized.y",   "gaming_normalized.y",   "clubbing_normalized.y",  "reading_normalized.y",  "tv_normalized.y",       "theater_normalized.y",  "movies_normalized.y",   "concerts_normalized.y",  "music_normalized.y",    "shopping_normalized.y", "yoga_normalized.y")
x_acts_normed = c("sports_normalized.x",   "tvsports_normalized.x", "exercise_normalized.x", "dining_normalized.x",  "museums_normalized.x",  "art_normalized.x",      "hiking_normalized.x",   "gaming_normalized.x",   "clubbing_normalized.x",  "reading_normalized.x",  "tv_normalized.x",       "theater_normalized.x",  "movies_normalized.x",   "concerts_normalized.x",  "music_normalized.x",    "shopping_normalized.x", "yoga_normalized.x")
acts_normed = c(y_acts_normed, x_acts_normed)
x_activities = c("sports.x",              "tvsports.x",            "exercise.x",            "dining.x",              "museums.x",             "art.x",                 "hiking.x",              "gaming.x",              "clubbing.x",            "reading.x",             "tv.x",                  "theater.x",             "movies.x",              "concerts.x",            "music.x",               "shopping.x",            "yoga.x")
y_activities = c("sports.y",              "tvsports.y",            "exercise.y",            "dining.y",              "museums.y",             "art.y",                 "hiking.y",              "gaming.y",              "clubbing.y",            "reading.y",             "tv.y",                  "theater.y",             "movies.y",              "concerts.y",            "music.y",               "shopping.y",            "yoga.y")
activities = c(x_activities, y_activities)
act_interests = c("avg_act_interest.y", "avg_act_interest.x")
merged_acts = c(activities, acts_normed)
expectationsX = c("expnum.x", "expnum.y")
expectationsY = c("exphappy.x", "exphappy.y")
expectations = c(expectationsX, expectationsY)
selfPersx = c('attr5_1.x', 'sinc5_1.x', 'intel5_1.x', 'fun5_1.x', 'amb5_1.x')
selfPersy = c('attr5_1.y', 'sinc5_1.y', 'intel5_1.y', 'fun5_1.y', 'amb5_1.y')
selfPers3y = c('attr3_1.y', 'sinc3_1.y', 'intel3_1.y', 'fun3_1.y', 'amb3_1.y')
selfPers3x = c('attr3_1.x', 'sinc3_1.x', 'intel3_1.x', 'fun3_1.x', 'amb3_1.x')
selfAttrsx =  c('attr5_1.x', "attr3_1.x")
selfAttrsy =  c('attr5_1.y', "attr3_1.y")
selfFunsx =  c('attr5_1.x', "attr3_1.x")
selfFunsy =  c('attr5_1.y', "attr3_1.y")
selfPers = c(selfPersx, selfPersy, expectations)
goOutX = c("go_out.x_1", "go_out.x_2",  "go_out.x_3", "go_out.x_4", "go_out.x_5", "go_out.x_6", "go_out.x_7", "go_out.x")
goOutY = c("go_out.y_1", "go_out.y_2",  "go_out.y_3", "go_out.y_4", "go_out.y_5", "go_out.y_6", "go_out.y_7", "go_out.y")
dateX = c("date.x_1", "date.x_2", "date.x_3", "date.x_4","date.x_5",   "date.x_6", "date.x_7", "date.x")
dateY = c("date.y_1", "date.y_2", "date.y_3", "date.y_4","date.y_5",   "date.y_6", "date.y_7", "date.y")
goalX = c("goal.x_1", "goal.x_2", "goal.x_3", "goal.x_4","goal.x_5",   "goal.x_6", "goal.x_7")
goalY = c("goal.y_1", "goal.y_2", "goal.y_3", "goal.y_4","goal.y_5",   "goal.y_6", "goal.y_7")
raceX = c("race.x_1", "race.x_2", "race.x_3", "race.x_4","race.x_5",  "race.x_6", "race.x_7")
raceY = c("race.y_1", "race.y_2", "race.y_3", "race.y_4","race.y_5", "race.y_6", "race.y_7")
binaries = c(goOutX, goOutY, dateX, dateY, goalX, goalY, raceX, raceY)
statedPrefsX = c('pf_o_attNew.x', 'pf_o_sinNew.x', 'pf_o_intNew.x', 'pf_o_funNew.x', 'pf_o_ambNew.x', 'pf_o_shaNew.x')
statedPrefsY = c('pf_o_attNew.y', 'pf_o_sinNew.y', 'pf_o_intNew.y', 'pf_o_funNew.y', 'pf_o_ambNew.y', 'pf_o_shaNew.y')
xValues = c("attr1_1.x", "fun1_1.x", "amb1_1.x", "intel1_1.x", "sinc1_1.x")
yValues = c("attr1_1.y", "fun1_1.y", "amb1_1.y", "intel1_1.y", "sinc1_1.y")
values = c(xValues, yValues)
statedPrefs = c(statedPrefsX, statedPrefsY)
raceAndReligion = c("imprelig.x", "imprelig.y", "imprace.x", "imprace.y", "samerace.x")
commonraces = c("race.x_2", "race.x_4", "race.y_2", "race.y_4")
raceY = c("race.y_1", "race.y_2", "race.y_3", "race.y_4","race.y_5", "race.y_6", "race.y_7")
# features = c(binaries, raceAndReligion, selfPers, statedPrefs, allMeansButDec, merged_acts)

# modMeansX = c("attr_o.mean.x", "fun_o.mean.x", "like_o.mean.x", "prob_o.mean.x")
features = c(x_activities, y_activities)

modMeansX = c("attr_o.mean.x", "fun_o.mean.x", "like_o.mean.x")

allMeansButDecX = c("attr_o.mean.x", "sinc_o.mean.x", "fun_o.mean.x", "amb_o.mean.x", "like_o.mean.x",  "prob_o.mean.x")
allMeansButDecY = c("attr.mean.y", "sinc.mean.y", "fun.mean.y", "amb.mean.y", "like.mean.y", "prob.mean.y")
allMeansButDecNewX = c("attr.mean.x", "sinc.mean.x", "fun.mean.x", "amb.mean.x", "like.mean.x", "prob.mean.x")
allMeansButDecNewY = c("attr_o.mean.y", "sinc_o.mean.y", "fun_o.mean.y", "amb_o.mean.y", "like_o.mean.y",  "prob_o.mean.y")
allMeansButDec = c(allMeansButDecX, allMeansButDecY, allMeansButDecNewX, allMeansButDecNewY)
decMeans = c("dec_o.mean.x", "dec.mean.y")
allMeans = c(allMeansButDec, decMeans)


allRatingsButDecX = c("attr_o.finalMean.x",     "sinc_o.finalMean.x",  "intel_o.finalMean.x",    "fun_o.finalMean.x",      "amb_o.finalMean.x",      "shar_o.finalMean.x",     "like_o.finalMean.x",     "prob_o.finalMean.x")
allRatingsButDecY = c("attr.finalMean.y",     "sinc.finalMean.y",  "intel.finalMean.y",    "fun.finalMean.y",      "amb.finalMean.y",      "shar.finalMean.y",     "like.finalMean.y",     "prob.finalMean.y")
ratings = c(allRatingsButDecX, allRatingsButDecY)


features = c(ratings, "reading.x")


# men_pred = linearPredictor(men,"dec_o.x",features,0)

for(i in 1:5){
  men_forest_pred = randForestPredictor(men,"dec_o.x",features,100)  
}


# women_pred = linearPredictor(women,"dec_o.x",features,0)
for(i in 1:5){
  women_forest_pred = randForestPredictor(women,"dec_o.x",features,500)
}
#Adding activities only makes a difference for women's choices of men, not men's choices of women. 



# men["go_out.x"] = men["go_out.x"] - 4.32
# men["go_out.y"] = men["go_out.y"] - 4.32
# women["go_out.x"] = women["go_out.x"] - 4.32
# women["go_out.y"] = women["go_out.y"] - 4.32
# men["date.x"] = men["date.x"] - 10.02
# men["date.y"] = men["date.y"] - 10.02
# women["date.x"] = men["date.x"]  - 10.02
# women["date.y"] = women["date.y"] - 10.02
# men["goal.x"] = men["goal.x"] -  4.24
# men["goal.y"] = men["goal.y"] - 4.24
# women["goal.y"] = women["goal.y"] - 4.24
# women["goal.x"] = women["goal.x"]  - 4.24
# men["race.x"] = men["race.x"] -  5.5
# men["race.y"] = men["race.y"] -  5.5
# women["race.y"] = women["race.y"] - 5.5
# women["race.x"] = women["race.x"] - 5.5
# 
# binaries = c("go_out.x", "date.x", "go_out.y", "date.y", "goal.x", "goal.y", "race.x", "race.y")
# 
# for(bin in binaries){
#   for(i in 1:7){
#     men[paste(bin,toString(i),sep="_")] <- ifelse(men[bin] == i,1,0)  
#     women[paste(bin,toString(i),sep="_")] <- ifelse(women[bin] == i,1,0)  
#   }
# }
# 
# write.csv(men, '~/Desktop/newest_men.csv')
# write.csv(women, '~/Desktop/newest_women.csv')
