


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
                       "intel5_1", "fun5_1", "amb5_1", "attr",     "sinc",     "intel",    "fun",      "amb",      "shar",     "like",     "prob")

drops = c(".id")
df = df[,!(names(df) %in% drops)]


classifiers = c("go_out", "date", "race")

for(bin in classifiers){
  for(i in 1:7){
    df[paste(bin,toString(i),sep="_")] <- ifelse(df[bin] == i,1,0)  
  }
}
df["numRated"] = 0
df["numRaters"] = 0

tab = table(df["iid"])
for(i in unique(df[["iid"]])){
  print(i)
  if(i < 118){
    df[df["iid"] == i,][["numRated"]] = tab[[i]]
  }
  else{
    df[df["iid"] == i,][["numRated"]] = tab[[i - 1]]
  }
}
for(i in unique(df[["pid"]])){
  if(i < 118){
    df[df["pid"] == i,][["numRaters"]] = tab[[i]]
  }
  else{
    df[df["pid"] == i,][["numRaters"]] = tab[[i - 1]]
  }
}

df["selfPercTotal"] = 0
df["antPercTotal"] = 0
ratings = c("dec", "attr",     "sinc",     "intel",    "fun",      "amb",      "shar",     "like",     "prob")



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
  men[thing] = men[thing] - men[paste(thing,"Mean",sep="")]
  women[thing] = women[thing] - women[paste(thing,"Mean",sep="")]
}



men["racialInter"] = men["samerace"]*men["imprace"]
women["racialInter"] = women["samerace"]*women["imprace"]
activities = c("sports",            "tvsports",          "exercise",         "dining",            "museums",           "art",               "hiking",            "gaming",            "clubbing",         "reading",           "tv",                "theater",           "movies",            "concerts",          "music",  "shopping", "yoga")
men["avgAct"] = 0
women["avgAct"] = 0
for(act in activities){
  women["avgAct"] =  women["avgAct"] + women[act]
}
men["avgAct"] = men["avgAct"]/length(activities)
women["avgAct"] = women["avgAct"]/length(activities)
for(act in activities){
  men[paste(act,"Norm",sep="")] = men[act] -   men["avgAct"]
  women[paste(act,"Norm",sep="")] = women[act] -   women["avgAct"]
}


men = ddply(men, .(iid) ,transform ,decSum = sum(dec))
men = ddply(men, .(iid) ,transform ,attrSum = sum(attr))
men = ddply(men, .(iid) ,transform ,sincSum = sum(sinc))
men = ddply(men, .(iid) ,transform ,intelSum = sum(intel))
men = ddply(men, .(iid) ,transform ,funSum = sum(fun))
men = ddply(men, .(iid) ,transform ,ambSum = sum(amb))
men = ddply(men, .(iid) ,transform ,sharSum = sum(shar))
men = ddply(men, .(iid) ,transform ,likeSum = sum(like))
men = ddply(men, .(iid) ,transform ,probSum = sum(prob))

women = ddply(women, .(iid) ,transform ,decSum = sum(dec))
women = ddply(women, .(iid) ,transform ,attrSum = sum(attr))
women = ddply(women, .(iid) ,transform ,sincSum = sum(sinc))
women = ddply(women, .(iid) ,transform ,intelSum = sum(intel))
women = ddply(women, .(iid) ,transform ,funSum = sum(fun))
women = ddply(women, .(iid) ,transform ,ambSum = sum(amb))
women = ddply(women, .(iid) ,transform ,sharSum = sum(shar))
women = ddply(women, .(iid) ,transform ,likeSum = sum(like))
women = ddply(women, .(iid) ,transform ,probSum = sum(prob))

ratings = c("dec", "attr", "sinc", "intel", "fun", "amb", "shar", "like", "prob")

for(r in ratings){
  men[paste(r,"Avg",sep="")] = men[paste(r,"Sum",sep="")]/men["numRated"]
  women[paste(r,"Avg",sep="")] = women[paste(r,"Sum",sep="")]/women["numRated"]
}

for(r in ratings){
  men[paste(r,"Norm",sep="")] = men[r] - men[paste(r,"Avg",sep="")]
  women[paste(r,"Norm",sep="")] = women[r] - women[paste(r,"Avg",sep="")]
}



own_id_info = c("iid", "pid", "id", "partner")
partner_id_info = c("pid", "iid", "partner", "id")
merged_men = merge(men, women, by.x = own_id_info, by.y = partner_id_info, all = FALSE)
merged_women = merge(women, men, by.x = own_id_info, by.y = partner_id_info, all = FALSE)
drops = c(".id.x", ".id.y")
merged_men = merged_men[,!(names(merged_men) %in% drops)]
merged_women = merged_women[,!(names(merged_women) %in% drops)]

ratings = c("iid", "pid","id", "partner", "dec",   "attr",  "sinc",  "intel", "fun",   "amb", "like",
            "decSum",   "attrSum",  "sincSum",  "intelSum", "funSum",   "ambSum", "likeSum", 
            "decAvg",   "attrAvg",  "sincAvg",  "intelAvg", "funAvg",   "ambAvg", "likeAvg", 
            "decNorm",   "attrNorm",  "sincNorm",  "intelNorm", "funNorm",   "ambNorm", "likeNorm")
write.csv(men, paste("~/Desktop/reallyNewMen.csv")

# 
for(i in 1:21){
  wave = men[men["wave"] == i,]
  wave = wave[ratings]
  write.csv(wave, paste("~/Desktop/waves/wave",paste(toString(i),".csv",sep=""),sep=""))
}

waveReader = function(i){
  return(read.csv(paste("~/Desktop/waves/wave",paste(toString(i),".csv",sep=""),sep="")))
}

addCols = function(wave,rat){
  ids = unique(wave[["id"]])
  oldColNum = ncol(wave)
  for(i in ids){
    str = paste(rat,paste("Rater",toString(i),sep="_"),sep="_")
    wave[str] = 0
    for(j in ids){
      if(i != j){
        wave[wave["id"] == j,][[str]] = wave[wave["id"] == i,][[rat]]        
      }
      else{
        wave[wave["id"] == j,][[str]] = sample(wave[wave["id"] == i,][[rat]])
      }
    }
  }
  str = paste(rat,"PartnersAvg",sep="")
  print(names(wave)[0:-oldColNum])
  wave[str] = rowSums(wave[0:-oldColNum])/length(ids)
  return(wave)
}


for(i in 2:2){
  wave = waveReader(i)  
  origRatings = c("attr",  "sinc",  "intel", "fun",   "amb", "like")
  normedRatings = c("attrNorm",  "sincNorm",  "intelNorm", "funNorm",   "ambNorm", "likeNorm")
  features = c("dec","decNorm", origRatings, normedRatings)
  for(rat in features){
    wave = addCols(wave, rat)
  }
  write.csv(wave, paste("~/Desktop/waves/wave",paste(toString(i),".csv",sep=""),sep=""))
}
