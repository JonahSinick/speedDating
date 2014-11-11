


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

df <- read.csv('~/Desktop/speedDatingData.csv')
f=function(x){
  x<-as.numeric(as.character(x)) #first convert each column into numeric if it is from factor
  x[is.na(x)] =median(x, na.rm=TRUE) #convert the item with NA to median value from the column
  x #display the column
}


keeps =  c("iid",      "id",       "gender",    "wave",     "round", "partner",  "pid",      "match",    "int_corr", "samerace","race",  "age", "imprace",  "imprelig", "goal",     "date",     "go_out",   
           "sports",   "tvsports", "exercise", "dining",   "museums",  "art",      "hiking",   "gaming",   "clubbing", 
           "reading",  "tv", "theater",  "movies",   "concerts",
           "music",    "shopping", "yoga",     "exphappy", "expnum",   "attr1_1",  "sinc1_1",  "intel1_1", "fun1_1",
           "amb1_1",   "shar1_1", "attr3_1",  "sinc3_1",  "fun3_1",   "intel3_1", "amb3_1",   "attr5_1",  "sinc5_1",
           "intel5_1", "fun5_1", "amb5_1",   
           "dec",      "attr",     "sinc",     "intel",    "fun",      "amb",      "shar",     "like",     "prob",     "met")

df = df[keeps]
keeps_to_fix =  c("match",    "int_corr", "samerace","race",  "age", "imprace",  "imprelig", "goal",     "date",     "go_out",   
                  "sports",   "tvsports", "exercise", "dining",   "museums",  "art",      "hiking",   "gaming",   "clubbing", 
                  "reading",  "tv", "theater",  "movies",   "concerts",
                  "music",    "shopping", "yoga",     "exphappy", "expnum",   "attr1_1",  "sinc1_1",  "intel1_1", "fun1_1",
                  "amb1_1",   "shar1_1", "attr3_1",  "sinc3_1",  "fun3_1",   "intel3_1", "amb3_1",   "attr5_1",  "sinc5_1",
                  "intel5_1", "fun5_1", "amb5_1",   
                  "dec",      "attr",     "sinc",     "intel",    "fun",      "amb",      "shar",     "like",     "prob",     "met")

df_rm_na = df[keeps_to_fix]
df_rm_na = data.frame(apply(df_rm_na,2,f))
df[keeps_to_fix] = df_rm_na[keeps_to_fix]
df = na.omit(df)

classifiers = c("go_out", "date", "race")

for(bin in classifiers){
  for(i in 1:7){
    df[paste(bin,toString(i),sep="_")] <- ifelse(df[bin] == i,1,0)  
  }
}


things_to_average =  c("age", "imprace",  "imprelig",
                       "sports",   "tvsports", "exercise", "dining",   "museums",  "art",      "hiking",   "gaming",   "clubbing", 
                       "reading",  "tv", "theater",  "movies",   "concerts",
                       "music",    "shopping", "yoga",     "exphappy", "expnum",   "attr1_1",  "sinc1_1",  "intel1_1", "fun1_1",
                       "amb1_1",   "shar1_1", "attr3_1",  "sinc3_1",  "fun3_1",   "intel3_1", "amb3_1",   "attr5_1",  "sinc5_1",
                       "intel5_1", "fun5_1", "amb5_1",   
                       "dec",      "attr",     "sinc",     "intel",    "fun",      "amb",      "shar",     "like",     "prob")

drops = c(".id")
df = df[,!(names(df) %in% drops)]







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

df <- read.csv('~/Desktop/speedDatingData.csv')
f=function(x){
  x<-as.numeric(as.character(x)) #first convert each column into numeric if it is from factor
  x[is.na(x)] =median(x, na.rm=TRUE) #convert the item with NA to median value from the column
  x #display the column
}


keeps =  c("iid",      "id",       "gender",    "wave",     "round", "partner",  "pid",      "match",    "int_corr", "samerace","race",  "age", "imprace",  "imprelig", "goal",     "date",     "go_out",   
           "sports",   "tvsports", "exercise", "dining",   "museums",  "art",      "hiking",   "gaming",   "clubbing", 
           "reading",  "tv", "theater",  "movies",   "concerts",
           "music",    "shopping", "yoga",     "exphappy", "expnum",   "attr1_1",  "sinc1_1",  "intel1_1", "fun1_1",
           "amb1_1",   "shar1_1", "attr3_1",  "sinc3_1",  "fun3_1",   "intel3_1", "amb3_1",   "attr5_1",  "sinc5_1",
           "intel5_1", "fun5_1", "amb5_1",   
           "dec",      "attr",     "sinc",     "intel",    "fun",      "amb",      "shar",     "like",     "prob",     "met")

df = df[keeps]
keeps_to_fix =  c("match",    "int_corr", "samerace","race",  "age", "imprace",  "imprelig", "goal",     "date",     "go_out",   
                  "sports",   "tvsports", "exercise", "dining",   "museums",  "art",      "hiking",   "gaming",   "clubbing", 
                  "reading",  "tv", "theater",  "movies",   "concerts",
                  "music",    "shopping", "yoga",     "exphappy", "expnum",   "attr1_1",  "sinc1_1",  "intel1_1", "fun1_1",
                  "amb1_1",   "shar1_1", "attr3_1",  "sinc3_1",  "fun3_1",   "intel3_1", "amb3_1",   "attr5_1",  "sinc5_1",
                  "intel5_1", "fun5_1", "amb5_1",   
                  "dec",      "attr",     "sinc",     "intel",    "fun",      "amb",      "shar",     "like",     "prob",     "met")

df_rm_na = df[keeps_to_fix]
df_rm_na = data.frame(apply(df_rm_na,2,f))
df[keeps_to_fix] = df_rm_na[keeps_to_fix]
df = na.omit(df)

classifiers = c("go_out", "date", "race")

for(bin in classifiers){
  for(i in 1:7){
    df[paste(bin,toString(i),sep="_")] <- ifelse(df[bin] == i,1,0)  
  }
}


things_to_average =  c("age", "imprace",  "imprelig",
                       "sports",   "tvsports", "exercise", "dining",   "museums",  "art",      "hiking",   "gaming",   "clubbing", 
                       "reading",  "tv", "theater",  "movies",   "concerts",
                       "music",    "shopping", "yoga",     "exphappy", "expnum",   "attr1_1",  "sinc1_1",  "intel1_1", "fun1_1",
                       "amb1_1",   "shar1_1", "attr3_1",  "sinc3_1",  "fun3_1",   "intel3_1", "amb3_1",   "attr5_1",  "sinc5_1",
                       "intel5_1", "fun5_1", "amb5_1",   
                       "dec",      "attr",     "sinc",     "intel",    "fun",      "amb",      "shar",     "like",     "prob")

drops = c(".id")
df = df[,!(names(df) %in% drops)]




men = df[df["gender"] == 1,]



men = ddply(men, .() ,transform ,ageMean = mean(age))
men = ddply(men, .() ,transform ,impraceMean = mean(imprace))
men = ddply(men, .() ,transform ,impreligMean = mean(imprelig))
men = ddply(men, .() ,transform ,sportsMean = mean(sports))
men = ddply(men, .() ,transform ,tvsportsMean = mean(tvsports))
men = ddply(men, .() ,transform ,exerciseMean = mean(exercise))
men = ddply(men, .() ,transform ,diningMean = mean(dining))
men = ddply(men, .() ,transform ,museumsMean = mean(museums))
men = ddply(men, .() ,transform ,artMean = mean(art))
men = ddply(men, .() ,transform ,hikingMean = mean(hiking))
men = ddply(men, .() ,transform ,gamingMean = mean(gaming))
men = ddply(men, .() ,transform ,clubbingMean = mean(clubbing))
men = ddply(men, .() ,transform ,readingMean = mean(reading))
men = ddply(men, .() ,transform ,tvMean = mean(tv))
men = ddply(men, .() ,transform ,theaterMean = mean(theater))
men = ddply(men, .() ,transform ,moviesMean = mean(movies))
men = ddply(men, .() ,transform ,concertsMean = mean(concerts))
men = ddply(men, .() ,transform ,musicMean = mean(music))
men = ddply(men, .() ,transform ,shoppingMean = mean(shopping))
men = ddply(men, .() ,transform ,yogaMean = mean(yoga))
men = ddply(men, .() ,transform ,exphappyMean  = mean(exphappy))
men = ddply(men, .() ,transform ,expnumMean = mean(expnum))
men = ddply(men, .() ,transform ,attr1_1Mean = mean(attr1_1))
men = ddply(men, .() ,transform ,sinc1_1Mean = mean(sinc1_1))
men = ddply(men, .() ,transform ,intel1_1Mean = mean(intel1_1))
men = ddply(men, .() ,transform ,fun1_1Mean = mean(fun1_1))
men = ddply(men, .() ,transform ,amb1_1Mean = mean(amb1_1))
men = ddply(men, .() ,transform ,shar1_1Mean = mean(shar1_1))
men = ddply(men, .() ,transform ,attr3_1Mean = mean(attr3_1))
men = ddply(men, .() ,transform ,sinc3_1Mean = mean(sinc3_1))
men = ddply(men, .() ,transform ,fun3_1Mean = mean(fun3_1))
men = ddply(men, .() ,transform ,intel3_1Mean = mean(intel3_1))
men = ddply(men, .() ,transform ,amb3_1Mean = mean(amb3_1))
men = ddply(men, .() ,transform ,attr5_1Mean = mean(attr5_1))
men = ddply(men, .() ,transform ,sinc5_1Mean = mean(sinc5_1))
men = ddply(men, .() ,transform ,intel5_1Mean = mean(intel5_1))
men = ddply(men, .() ,transform ,fun5_1Mean = mean(fun5_1))
men = ddply(men, .() ,transform ,amb5_1Mean = mean(amb5_1))
men = ddply(men, .() ,transform ,decMean = mean(dec))
men = ddply(men, .() ,transform ,attrMean = mean(attr))
men = ddply(men, .() ,transform ,sincMean = mean(sinc))
men = ddply(men, .() ,transform ,intelMean = mean(intel))
men = ddply(men, .() ,transform ,funMean = mean(fun))
men = ddply(men, .() ,transform ,ambMean = mean(amb))
men = ddply(men, .() ,transform ,sharMean = mean(shar))
men = ddply(men, .() ,transform ,likeMean = mean(like))
men = ddply(men, .() ,transform ,probMean = mean(prob))


for(thing in things_to_average){
  men[thing] = men[thing] - men[paste(thing,"Mean",sep="")]
}

men = ddply(men, .(iid) ,transform ,dec.ownRating = mean(dec))
men = ddply(men, .(iid) ,transform ,attr.ownRating = mean(attr))
men = ddply(men, .(iid) ,transform ,sinc.ownRating = mean(sinc))
men = ddply(men, .(iid) ,transform ,intel.ownRating = mean(intel))
men = ddply(men, .(iid) ,transform ,fun.ownRating = mean(fun))
men = ddply(men, .(iid) ,transform ,amb.ownRating = mean(amb))
men = ddply(men, .(iid) ,transform ,shar.ownRating = mean(shar))
men = ddply(men, .(iid) ,transform ,like.ownRating = mean(like))
men = ddply(men, .(iid) ,transform ,prob.ownRating = mean(prob))

ratings = c("dec", "attr", "sinc", "intel", "fun", "amb", "shar", "like", "prob")
for(r in ratings){
  men[paste(r,"Normalized",sep="")] = men[r] - men[paste(r,"ownRating",sep=".")]
}

ratings2 = c("attr", "sinc", "intel", "fun", "amb", "shar")
for(r in ratings2){
  men[paste(r,"OwnInter",sep="")] = men[r]*men[paste(r,"1_1",sep="")]
}

men["racialInter"] = men["samerace"]*men["imprace"]
activities = c("sports",            "tvsports",          "exercise",         "dining",            "museums",           "art",               "hiking",            "gaming",            "clubbing",         "reading",           "tv",                "theater",           "movies",            "concerts",          "music",  "shopping", "yoga")
men["avgActInterLevel"] = 0
for(act in activities){
  men["avgActInterLevel"] =  men["avgActInterLevel"] + men[act]
}
men["avgActInterLevel"] = men["avgActInterLevel"]/length(activities)
for(act in activities){
  men[paste(act,"Normalized",sep="")] = men[act] -   men["avgActInterLevel"]
}







women = df[df["gender"] == 0,]



women = ddply(women, .() ,transform ,ageMean = mean(age))
women = ddply(women, .() ,transform ,impraceMean = mean(imprace))
women = ddply(women, .() ,transform ,impreligMean = mean(imprelig))
women = ddply(women, .() ,transform ,sportsMean = mean(sports))
women = ddply(women, .() ,transform ,tvsportsMean = mean(tvsports))
women = ddply(women, .() ,transform ,exerciseMean = mean(exercise))
women = ddply(women, .() ,transform ,diningMean = mean(dining))
women = ddply(women, .() ,transform ,museumsMean = mean(museums))
women = ddply(women, .() ,transform ,artMean = mean(art))
women = ddply(women, .() ,transform ,hikingMean = mean(hiking))
women = ddply(women, .() ,transform ,gamingMean = mean(gaming))
women = ddply(women, .() ,transform ,clubbingMean = mean(clubbing))
women = ddply(women, .() ,transform ,readingMean = mean(reading))
women = ddply(women, .() ,transform ,tvMean = mean(tv))
women = ddply(women, .() ,transform ,theaterMean = mean(theater))
women = ddply(women, .() ,transform ,moviesMean = mean(movies))
women = ddply(women, .() ,transform ,concertsMean = mean(concerts))
women = ddply(women, .() ,transform ,musicMean = mean(music))
women = ddply(women, .() ,transform ,shoppingMean = mean(shopping))
women = ddply(women, .() ,transform ,yogaMean = mean(yoga))
women = ddply(women, .() ,transform ,exphappyMean  = mean(exphappy))
women = ddply(women, .() ,transform ,expnumMean = mean(expnum))
women = ddply(women, .() ,transform ,attr1_1Mean = mean(attr1_1))
women = ddply(women, .() ,transform ,sinc1_1Mean = mean(sinc1_1))
women = ddply(women, .() ,transform ,intel1_1Mean = mean(intel1_1))
women = ddply(women, .() ,transform ,fun1_1Mean = mean(fun1_1))
women = ddply(women, .() ,transform ,amb1_1Mean = mean(amb1_1))
women = ddply(women, .() ,transform ,shar1_1Mean = mean(shar1_1))
women = ddply(women, .() ,transform ,attr3_1Mean = mean(attr3_1))
women = ddply(women, .() ,transform ,sinc3_1Mean = mean(sinc3_1))
women = ddply(women, .() ,transform ,fun3_1Mean = mean(fun3_1))
women = ddply(women, .() ,transform ,intel3_1Mean = mean(intel3_1))
women = ddply(women, .() ,transform ,amb3_1Mean = mean(amb3_1))
women = ddply(women, .() ,transform ,attr5_1Mean = mean(attr5_1))
women = ddply(women, .() ,transform ,sinc5_1Mean = mean(sinc5_1))
women = ddply(women, .() ,transform ,intel5_1Mean = mean(intel5_1))
women = ddply(women, .() ,transform ,fun5_1Mean = mean(fun5_1))
women = ddply(women, .() ,transform ,amb5_1Mean = mean(amb5_1))
women = ddply(women, .() ,transform ,decMean = mean(dec))
women = ddply(women, .() ,transform ,attrMean = mean(attr))
women = ddply(women, .() ,transform ,sincMean = mean(sinc))
women = ddply(women, .() ,transform ,intelMean = mean(intel))
women = ddply(women, .() ,transform ,funMean = mean(fun))
women = ddply(women, .() ,transform ,ambMean = mean(amb))
women = ddply(women, .() ,transform ,sharMean = mean(shar))
women = ddply(women, .() ,transform ,likeMean = mean(like))
women = ddply(women, .() ,transform ,probMean = mean(prob))


for(thing in things_to_average){
  women[thing] = women[thing] - women[paste(thing,"Mean",sep="")]
}

# women = ddply(women, .(iid) ,transform ,dec.ownRating = mean(dec))
# women = ddply(women, .(iid) ,transform ,attr.ownRating = mean(attr))
# women = ddply(women, .(iid) ,transform ,sinc.ownRating = mean(sinc))
# women = ddply(women, .(iid) ,transform ,intel.ownRating = mean(intel))
# women = ddply(women, .(iid) ,transform ,fun.ownRating = mean(fun))
# women = ddply(women, .(iid) ,transform ,amb.ownRating = mean(amb))
# women = ddply(women, .(iid) ,transform ,shar.ownRating = mean(shar))
# women = ddply(women, .(iid) ,transform ,like.ownRating = mean(like))
# women = ddply(women, .(iid) ,transform ,prob.ownRating = mean(prob))

# ratings = c("dec", "attr", "sinc", "intel", "fun", "amb", "shar", "like", "prob")
# for(r in ratings){
#   women[paste(r,"Normalized",sep="")] = women[r] - women[paste(r,"ownRating",sep=".")]
# }

# ratings2 = c("attr", "sinc", "intel", "fun", "amb", "shar")
# for(r in ratings2){
#   women[paste(r,"OwnInter",sep="")] = women[r]*women[paste(r,"1_1",sep="")]
# }

women["racialInter"] = women["samerace"]*women["imprace"]
activities = c("sports",            "tvsports",          "exercise",         "dining",            "museums",           "art",               "hiking",            "gaming",            "clubbing",         "reading",           "tv",                "theater",           "movies",            "concerts",          "music",  "shopping", "yoga")
women["avgActInterLevel"] = 0
for(act in activities){
  women["avgActInterLevel"] =  women["avgActInterLevel"] + women[act]
}
women["avgActInterLevel"] = women["avgActInterLevel"]/length(activities)
for(act in activities){
  women[paste(act,"Normalized",sep="")] = women[act] -   women["avgActInterLevel"]
}

own_id_info = c("iid", "pid", "id", "partner")
partner_id_info = c("pid", "iid", "partner", "id")
merged_men = merge(men, women, by.x = own_id_info, by.y = partner_id_info, all = FALSE)
merged_women = merge(women, men, by.x = own_id_info, by.y = partner_id_info, all = FALSE)

write.csv(merged_men, "~/Desktop/menSuper.csv")
write.csv(merged_women, "~/Desktop/womenSuper.csv")
