

library(gdata)
library(ggplot2)
library(miscTools)
library(xlsx)
library(hash)
library(aod)
library(randomForest)
library(LiblineaR)

# men <- read.csv('~/Desktop/men.csv')
# women <- read.csv('~/Desktop/women.csv')

df <- read.csv('~/Desktop/df.csv')
f=function(x){
  x<-as.numeric(as.character(x)) #first convert each column into numeric if it is from factor
  x[is.na(x)] =median(x, na.rm=TRUE) #convert the item with NA to median value from the column
  x #display the column
}
drops = c("income", "zipcode", "career",  "field", "field_cd", "undergra", "mn_sat", "tuition", "X" ,  "idg" , "condtn" ,"wave" ,    "round" ,   "position" ,"positin1" ,"order" , "match", "attr_o",  "sinc_o",  "intel_o", "fun_o",   "amb_o",  "shar_o",  "like_o",  "prob_o", "attr_inter",      "sinc_inter", "intel_inter",     "fun_inter",       "amb_inter",       "shar_inter",      "dec_oSum",        "dec_oCount",     "attr_oSum",       "attr_oCount",     "sinc_oSum",       "sinc_oCount",     "intel_oSum",      "intel_oCount",   "fun_oSum",        "fun_oCount",      "amb_oSum",        "amb_oCount",      "shar_oSum",       "shar_oCount", "like_oSum",       "like_oCount",     "prob_oSum",       "prob_oCount",      "dec_oAvg",        "attr_oAvg",       "sinc_oAvg",       "intel_oAvg",      "fun_oAvg", "amb_oAvg",        "shar_oAvg",       "like_oAvg", "pf_o_att"      ,     "pf_o_sin"    ,       "pf_o_int",           "pf_o_fun"        ,   "pf_o_amb"    ,       "pf_o_sha", "career_c", "from")
df = df[,!(names(df) %in% drops)]
df = data.frame(apply(df,2,f))



for(i in 1:5){
  df[paste("goal",toString(i),sep="_")] = 0
}

for(i in nrow(df)){
  for(j in 1:5){
    if(df[i,"goal"] == j){
      df[i,paste(df[i,"goal"],toString(j),sep="_")] = 1
    }
  }
}
df["ageDiff"] = df["age"] - df["age_o"]
men = df[df["gender"] == 1,]
women = df[df["gender"] == 0,]
men = merge(men, women, by.x = c("iid", "pid", "id", "partner"), by.y = c("pid", "iid", "partner", "id"), all = TRUE)
women = merge(women, men, by.x = c("iid", "pid", "id", "partner"), by.y = c("pid", "iid", "partner", "id"), all = TRUE)

activities = c("sports",            "tvsports",          "exercise",         "dining",            "museums",           "art",               "hiking",            "gaming",            "clubbing",         "reading",           "tv",                "theater",           "movies",            "concerts",          "music",            
  "shopping",          "yoga")

ownActs = c("sports.x",            "tvsports.x",          "exercise.x",         "dining.x",            "museums.x",           "art.x",               "hiking.x",            "gaming.x",            "clubbing.x",         "reading.x",           "tv.x",                "theater.x",           "movies.x",            "concerts.x",          "music.x",            
  "shopping.x",          "yoga.x")

otherActs = c("sports.y",            "tvsports.y",          "exercise.y",         "dining.y",            "museums.y",           "art.y",               "hiking.y",            "gaming.y",            "clubbing.y",         "reading.y",           "tv.y",                "theater.y",           "movies.y",            "concerts.y",          "music.y",            
  "shopping.y",          "yoga.y")

for(i in 1:length(activities)){
  men[paste(activities[i],"xy",sep="_")] = men[paste(activities[i],".x",sep="")]*men[paste(activities[i],".y",sep="")]
  women[paste(activities[i],"xy",sep="_")] = women[paste(activities[i],".x",sep="")]*women[paste(activities[i],".y",sep="")]
}
  
all = c("iid", "pid", "dec_o.x", "dec_o.y", "int_corr.x",          "samerace.x",          "age_o.x",             "race_o.x",            "met_o.x",            
       "age.x",               "race.x",              "imprace.x",           "imprelig.x",              
       "date.x",              "go_out.x",                     "sports.x",           
       "tvsports.x",          "exercise.x",          "dining.x",            "museums.x",           "art.x",              
       "hiking.x",            "gaming.x",            "clubbing.x",          "reading.x",           "tv.x",               
       "theater.x",           "movies.x",            "concerts.x",          "music.x",             "shopping.x",         
       "yoga.x",              "exphappy.x",          "expnum.x",            "attr1_1.x",           "sinc1_1.x",          
       "intel1_1.x",          "fun1_1.x",            "amb1_1.x",            "shar1_1.x",    "attr3_1.x",           "sinc3_1.x",           "fun3_1.x",            "intel3_1.x",         
       "amb3_1.x",            "attr5_1.x",           "sinc5_1.x",           "intel5_1.x",          "fun5_1.x",           
       "amb5_1.x",              
       "met.x",                          "dec_oOthersAvg.x",   
       "attr_oOthersAvg.x",   "sinc_oOthersAvg.x",   "intel_oOthersAvg.x",  "fun_oOthersAvg.x",    "amb_oOthersAvg.x",   
       "shar_oOthersAvg.x",   "like_oOthersAvg.x",   "prob_oOthersAvg.x",         "other_attr_inter.x", 
       "other_sinc_inter.x",  "other_intel_inter.x", "other_fun_inter.x",   "other_amb_inter.x",   "other_shar_inter.x", 
       "goal_1.x",            "goal_2.x",            "goal_3.x",            "goal_4.x",            "goal_5.x",           
                "ageDiff.x",           "gender.y",            "int_corr.y",          "samerace.y",         
       "age_o.y",             "race_o.y",            "met_o.y",             "age.y",               "race.y",             
       "imprace.y",           "imprelig.y",                                 "date.y",             
       "go_out.y",            "sports.y",            "tvsports.y",          "exercise.y",         
       "dining.y",            "museums.y",           "art.y",               "hiking.y",            "gaming.y",           
       "clubbing.y",          "reading.y",           "tv.y",                "theater.y",           "movies.y",           
       "concerts.y",          "music.y",             "shopping.y",          "yoga.y",              "exphappy.y",         
       "expnum.y",                       "attr3_1.y",          
       "sinc3_1.y",           "fun3_1.y",            "intel3_1.y",          "amb3_1.y",            "attr5_1.y",          
       "sinc5_1.y",           "intel5_1.y",          "fun5_1.y",            "amb5_1.y",               
       "met.y",                                
       "fun3_3.y",            "amb3_3.y",                        "dec_oOthersAvg.y",    "attr_oOthersAvg.y",   "sinc_oOthersAvg.y",  
       "intel_oOthersAvg.y",  "fun_oOthersAvg.y",    "amb_oOthersAvg.y",    "shar_oOthersAvg.y",   "like_oOthersAvg.y",  
       "prob_oOthersAvg.y", "other_attr_inter.y",  "other_sinc_inter.y",  "other_intel_inter.y",
       "other_fun_inter.y",   "other_amb_inter.y",   "other_shar_inter.y",  "goal_1.y",            "goal_2.y",           
       "goal_3.y",            "goal_4.y",            "goal_5.y",          "ageDiff.y",          
       "sports_xy",           "tvsports_xy",         "exercise_xy",         "dining_xy",           "museums_xy",         
       "art_xy",              "hiking_xy",           "gaming_xy",           "clubbing_xy",         "reading_xy",         
       "tv_xy",               "theater_xy",          "movies_xy",           "concerts_xy",         "music_xy")


men = men[all]
women = women[all]


men= men[sample(nrow(men)),]
women= women[sample(nrow(women)),]
men = na.omit(men)
women = na.omit(women)
men_train = men[seq(1, nrow(men)/2),] 
men_test = men[seq(nrow(men)/2 + 1, nrow(men)),]
women_train = women[seq(1, nrow(women)/2),] 
women_test = women[seq(nrow(women)/2 + 1, nrow(women)),]




# LiblineaR(men_train, men_train["dec_o"], type=6, cost=1, epsilon = 0.01, bias = TRUE, wi = NULL, cross = 0, verbose = FALSE)
# names(men_train)




 
mylogit <- glm(dec_o.x ~ attr_oOthersAvg.x + expnum.x + fun_oOthersAvg.x + date.x, data = men_train, family = "binomial")
men_test$predicted_decision <- predict(mylogit, newdata = men_test, type = "response")
men_test["predicted_decision"] <- ifelse(men_test["predicted_decision"]>= 0.50,1, 0)
s = nrow(men_test[men_test["dec_o.x"] == 0,])/nrow(men_test)
t = nrow(men_test[men_test[["predicted_decision"]] == men_test[["dec_o.x"]],])/nrow(men_test)
print(c(s,t,(1-s)/(1 - t),t-s))


#  mylogit <- glm(dec_o ~ dec_oAvg + attr_oOthersAvg, data = women_train, family = "binomial")
mylogit <- glm(dec_o.x ~ dec_oOthersAvg.x + attr_oOthersAvg.x + expnum.x + fun_oOthersAvg.x + date.x + sports.x, data = women_train, family = "binomial")
women_test$predicted_decision <- predict(mylogit, newdata = women_test, type = "response")
women_test["predicted_decision"] <- ifelse(women_test["predicted_decision"]>= 0.5,1, 0)
s = nrow(women_test[women_test["dec_o.x"] == 1,])/nrow(women_test)
t = nrow(men_test[women_test[["predicted_decision"]] == women_test[["dec_o.x"]],])/nrow(men_test)

print(c(s,t,((1 - s)/(1 - t)),t-s))

# fittedRandomForest <- randomForest(data=stripped, x=men_train2[features], y=men_train["dec_o"])


fit <- randomForest(as.factor(dec_o.x) ~ int_corr.x +samerace.x +age_o.x +race_o.x + age.x +imprace.x +imprelig.x +date.x 
                    +go_out.x +sports.x +tvsports.x +exercise.x +dining.x +museums.x +art.x 
                    +hiking.x +gaming.x +clubbing.x +reading.x +tv.x +theater.x +movies.x 
                    +concerts.x +music.x +shopping.x +yoga.x +exphappy.x +met.x +
                      +dec_oOthersAvg.x +attr_oOthersAvg.x +sinc_oOthersAvg.x +intel_oOthersAvg.x 
                    +fun_oOthersAvg.x +amb_oOthersAvg.x +shar_oOthersAvg.x +like_oOthersAvg.x 
                    +other_attr_inter.x +other_sinc_inter.x +other_intel_inter.x 
                    +other_fun_inter.x +other_amb_inter.x +other_shar_inter.x,
                    data=men_train, importance=TRUE, ntree=2000)
men_test["predicted_decision"] <- predict(fit, men_test)
s = nrow(men_test[men_test[["dec_o.x"]] == 0,])/nrow(men_test)
t = nrow(men_test[men_test[["predicted_decision"]] == men_test[["dec_o.x"]],])/nrow(men_test)
print(c(s,t,(1-s)/(1 - t),t-s))

w_fit <- randomForest(as.factor(dec_o.x) ~ int_corr.x +samerace.x +age_o.x +race_o.x + age.x +imprace.x +imprelig.x +date.x 
                    +go_out.x +sports.x +tvsports.x +exercise.x +dining.x +museums.x +art.x 
                    +hiking.x +gaming.x +clubbing.x +reading.x +tv.x +theater.x +movies.x 
                    +concerts.x +music.x +shopping.x +yoga.x +exphappy.x +met.x +
                      +dec_oOthersAvg.x +attr_oOthersAvg.x +sinc_oOthersAvg.x +intel_oOthersAvg.x 
                    +fun_oOthersAvg.x +amb_oOthersAvg.x +shar_oOthersAvg.x +like_oOthersAvg.x 
                    +other_attr_inter.x +other_sinc_inter.x +other_intel_inter.x 
                    +other_fun_inter.x +other_amb_inter.x +other_shar_inter.x,
                    data=women_train, importance=TRUE, ntree=2000)
women_test["predicted_decision"] <- predict(fit, women_test)
s = nrow(women_test[women_test[["dec_o.x"]] == 0,])/nrow(women_test)
t = nrow(women_test[women_test[["predicted_decision"]] == women_test[["dec_o.x"]],])/nrow(women_test)
print(c(s,t,(1-s)/(1 - t),t-s))
