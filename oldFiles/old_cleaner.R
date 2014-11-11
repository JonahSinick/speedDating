
df <- read.csv('~/Desktop/df.csv')
f=function(x){
  x<-as.numeric(as.character(x)) #first convert each column into numeric if it is from factor
  x[is.na(x)] =median(x, na.rm=TRUE) #convert the item with NA to median value from the column
  x #display the column
}
drops = c("income", "zipcode", "career",  "field", "field_cd", "undergra", "mn_sat", "tuition", "X" ,  "idg" , "condtn" ,"wave" ,    "round" ,   "position" ,"positin1" ,"order" , "match", "attr_o",  "sinc_o",  "intel_o", "fun_o",   "amb_o",  "shar_o",  "like_o",  "prob_o", "attr_inter",      "sinc_inter", "intel_inter",     "fun_inter",       "amb_inter",       "shar_inter",      "dec_oSum",        "dec_oCount",     "attr_oSum",       "attr_oCount",     "sinc_oSum",       "sinc_oCount",     "intel_oSum",      "intel_oCount",   "fun_oSum",        "fun_oCount",      "amb_oSum",        "amb_oCount",      "shar_oSum",       "shar_oCount", "like_oSum",       "like_oCount",     "prob_oSum",       "prob_oCount",      "dec_oAvg",        "attr_oAvg",       "sinc_oAvg",       "intel_oAvg",      "fun_oAvg", "amb_oAvg",        "shar_oAvg",       "like_oAvg", "pf_o_att"      ,     "pf_o_sin"    ,       "pf_o_int",           "pf_o_fun"        ,   "pf_o_amb"    ,       "pf_o_sha", "career_c", "from")
df = df[,!(names(df) %in% drops)]
df = data.frame(apply(df,2,f))



for(i in 1:7){
  df[paste("goal",toString(i),sep="_")] = 0
  df[paste("race",toString(i),sep="_")] = 0  
  df[paste("go_out",toString(i),sep="_")] = 0  
  df[paste("date",toString(i),sep="_")] = 0  
}

for(i in 1:nrow(df)){
  for(j in 1:6){
    if(df[i,"goal"] == j){
      df[i,paste("goal",toString(j),sep="_")] = 1
    }
    if(df[i,"race"] == j){
      df[i,paste("race",toString(j),sep="_")] = 1
    }
    if(df[i,"go_out"] == j){
      df[i,paste("go_out",toString(j),sep="_")] = 1
    }
  }
}
for(i in 1:nrow(df)){
  for(j in 1:7){
    if(df[i,"goal"] == j){
      df[i,paste("goal",toString(j),sep="_")] = 1
    }
    if(df[i,"race"] == j){
      df[i,paste("race",toString(j),sep="_")] = 1
    }
    if(df[i,"go_out"] == j){
      df[i,paste("go_out",toString(j),sep="_")] = 1
    }
    if(df[i,"date"] == j){
      df[i,paste("date",toString(j),sep="_")] = 1
    }
  }
}

for(i in 1:7){
  for(j in 1:7){
    df[paste(go_out_date,paste(toString(i),toString(j),sep="_"),sep="_")] = df[i,paste("go_out",toString(i),sep="_")]* df[i,paste("date",toString(j),sep="_")]
  }
}

# for(i in 1:length(activities)){
#   men[paste(activities[i],"xy",sep="_")] = men[paste(activities[i],".x",sep="")]*men[paste(activities[i],".y",sep="")]
#   women[paste(activities[i],"xy",sep="_")] = women[paste(activities[i],".x",sep="")]*women[paste(activities[i],".y",sep="")]
# }
# 
# df["ageDiff"] = df["age"] - df["age_o"]
# df["racialInter"] = df["samerace"]*df["imprace"]
# 
# men = df[df["gender"] == 1,]
# women = df[df["gender"] == 0,]
# men_temp = merge(men, women, by.x = c("iid", "pid", "id", "partner"), by.y = c("pid", "iid", "partner", "id"), all = TRUE)
# women = merge(women, men, by.x = c("iid", "pid", "id", "partner"), by.y = c("pid", "iid", "partner", "id"), all = TRUE)
# men = men_temp
# activities = c("sports",            "tvsports",          "exercise",         "dining",            "museums",           "art",               "hiking",            "gaming",            "clubbing",         "reading",           "tv",                "theater",           "movies",            "concerts",          "music",            
#   "shopping",          "yoga")
# 
# ownActs = c("sports.x",            "tvsports.x",          "exercise.x",         "dining.x",            "museums.x",           "art.x",               "hiking.x",            "gaming.x",            "clubbing.x",         "reading.x",           "tv.x",                "theater.x",           "movies.x",            "concerts.x",          "music.x",            
#   "shopping.x",          "yoga.x")
# 
# otherActs = c("sports.y",            "tvsports.y",          "exercise.y",         "dining.y",            "museums.y",           "art.y",               "hiking.y",            "gaming.y",            "clubbing.y",         "reading.y",           "tv.y",                "theater.y",           "movies.y",            "concerts.y",          "music.y",            
#   "shopping.y",          "yoga.y")
# 
# for(i in 1:length(activities)){
#   men[paste(activities[i],"xy",sep="_")] = men[paste(activities[i],".x",sep="")]*men[paste(activities[i],".y",sep="")]
#   women[paste(activities[i],"xy",sep="_")] = women[paste(activities[i],".x",sep="")]*women[paste(activities[i],".y",sep="")]
# }
# 
# for(i in 1:6){
#   men[paste("race","i",sep="_")] = men[paste(activities[i],".x",sep="")]*men[paste(activities[i],".y",sep="")]
#   women[paste(activities[i],"xy",sep="_")] = women[paste(activities[i],".x",sep="")]*women[paste(activities[i],".y",sep="")]
# }
# 
# 
# all = c("iid", "pid", "dec_o.x", "dec_o.y", "int_corr.x",          "samerace.x",          "age_o.x",             "race_o.x",            "met_o.x",            
#        "age.x",               "race.x",              "imprace.x",           "imprelig.x",              
#        "date.x",              "go_out.x",                     "sports.x",           
#        "tvsports.x",          "exercise.x",          "dining.x",            "museums.x",           "art.x",              
#        "hiking.x",            "gaming.x",            "clubbing.x",          "reading.x",           "tv.x",               
#        "theater.x",           "movies.x",            "concerts.x",          "music.x",             "shopping.x",         
#        "yoga.x",              "exphappy.x",          "expnum.x",            "attr1_1.x",           "sinc1_1.x",          
#        "intel1_1.x",          "fun1_1.x",            "amb1_1.x",            "shar1_1.x",    "attr3_1.x",           "sinc3_1.x",           "fun3_1.x",            "intel3_1.x",         
#        "amb3_1.x",            "attr5_1.x",           "sinc5_1.x",           "intel5_1.x",          "fun5_1.x",           
#        "amb5_1.x",              
#        "met.x",                          "dec_oOthersAvg.x",   
#        "attr_oOthersAvg.x",   "sinc_oOthersAvg.x",   "intel_oOthersAvg.x",  "fun_oOthersAvg.x",    "amb_oOthersAvg.x",   
#        "shar_oOthersAvg.x",   "like_oOthersAvg.x",   "prob_oOthersAvg.x",         "other_attr_inter.x", 
#        "other_sinc_inter.x",  "other_intel_inter.x", "other_fun_inter.x",   "other_amb_inter.x",   "other_shar_inter.x", 
#        "goal_1.x",            "goal_2.x",            "goal_3.x",            "goal_4.x",            "goal_5.x",           
#                 "ageDiff.x",           "int_corr.y",          "samerace.y",         
#        "age_o.y",             "race_o.y",            "met_o.y",             "age.y",               "race.y",             
#        "imprace.y",           "imprelig.y",                                 "date.y",             
#        "go_out.y",            "sports.y",            "tvsports.y",          "exercise.y",         
#        "dining.y",            "museums.y",           "art.y",               "hiking.y",            "gaming.y",           
#        "clubbing.y",          "reading.y",           "tv.y",                "theater.y",           "movies.y",           
#        "concerts.y",          "music.y",             "shopping.y",          "yoga.y",              "exphappy.y",         
#        "expnum.y",                       "attr3_1.y",          
#        "sinc3_1.y",           "fun3_1.y",            "intel3_1.y",          "amb3_1.y",            "attr5_1.y",          
#        "sinc5_1.y",           "intel5_1.y",          "fun5_1.y",            "amb5_1.y",               
#        "met.y",                                
#        "fun3_3.y",            "amb3_3.y",                        "dec_oOthersAvg.y",    "attr_oOthersAvg.y",   "sinc_oOthersAvg.y",  
#        "intel_oOthersAvg.y",  "fun_oOthersAvg.y",    "amb_oOthersAvg.y",    "shar_oOthersAvg.y",   "like_oOthersAvg.y",  
#        "prob_oOthersAvg.y", "other_attr_inter.y",  "other_sinc_inter.y",  "other_intel_inter.y",
#        "other_fun_inter.y",   "other_amb_inter.y",   "other_shar_inter.y",  "goal_1.y",            "goal_2.y",           
#        "goal_3.y",            "goal_4.y",            "goal_5.y",          "ageDiff.y",          
#        "sports_xy",           "tvsports_xy",         "exercise_xy",         "dining_xy",           "museums_xy",         
#        "art_xy",              "hiking_xy",           "gaming_xy",           "clubbing_xy",         "reading_xy",         
#        "tv_xy",               "theater_xy",          "movies_xy",           "concerts_xy",         "music_xy", "racialInter.x", "racialInter.y", "race_1.x", "race_2.x", "race_3.x", "race_4.x", "race_5.x", "race_6.x", "race_1.y", "race_2.y", "race_3.y", "race_4.y", "race_5.y", "race_6.y")
# 
# 
# men = men[all]
# women = women[all]
# 
# write.csv(men, "~/Desktop/men_cleaned.csv")
# write.csv(women, "~/Desktop/women_cleaned.csv")
