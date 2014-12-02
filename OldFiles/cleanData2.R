df <- read.csv('~/Desktop/speedDating/speedDatingData.csv')
f=function(x){
  x<-as.numeric(as.character(x)) #first convert each column into numeric if it is from factor
  x[is.na(x)] =median(x, na.rm=TRUE) #convert the item with NA to median value from the column
  x #display the column
}

n = names(df)
m = as.matrix(df[["income"]])
fixed = ifelse(m[,1] == "", NA, substring(m[,1], 1, 2))
fixed2 = ifelse(fixed == "", NA, as.integer(fixed))
median(fixed2, na.rm = TRUE)
fixed3 = ifelse(is.na(fixed2), median(fixed2, na.rm = TRUE), fixed2)
df[["income"]] = fixed3

m = as.matrix(df[["tuition"]])
fixed = ifelse(m[,1] == "", NA, substring(m[,1], 1, 2))
fixed2 = ifelse(fixed == "", NA, as.integer(fixed))
median(fixed2, na.rm = TRUE)
fixed3 = ifelse(is.na(fixed2), median(fixed2, na.rm = TRUE), fixed2)
df[["tuition"]] = fixed3

drops = n[grep("7|5|4|_s$|_o$|pf|2_|3_|1_3|1_2|num",n)]
df["numRatees"] = df["round"]

drops = c(drops, "positin1", "field", "undergra", "from", "zipcode", "career", "you_call", "them_cal", "dat_3", "numdat_3", "num_in_3", "idg", "condtn", "position", "match_es", "date_3", "length", "mn_sat", "satis_2", "round", "int_corr")
#Dropped fields for which the waves with lots of people are NA 
df = df[, !(names(df) %in% drops)]
df[df["pid"] == 530 & df["iid"] == 552,][["id"]] = 22



df = data.frame(apply(df,2,f))
df = df[!(df[["wave"]] %in% c(6, 16, 18, 20)),]

for(w in unique(df[["wave"]])){
  slice = df[df["wave"] == w,]
  print(w)
  men = slice[slice["gender"] == 1,]
  len = length(unique(men[["id"]]))
  len2 = length(unique(men[["partner"]]))
  numrows = nrow(slice)
  print(c(len, len2, len*len2, numrows/2))
}



for(str in c("goal", "date", "race", "go_out", "field_cd", "career_c")){
  codes = unique(df[[str]])
  for(code in codes){
    df[paste(str,toString(code))] = ifelse(df[[str]] == code, 1, 0)
  }
}
df["activityAvg"] = 0
for(activity in unique(names(df)[25:40])){
  df["activityAvg"] = df["activityAvg"] + df[activity]
}
df["activityAvg"] = df["activityAvg"]/(40 - 24 + 1)



df["raterCount"] = 0
df["rateeCount"] = 0
for(w in unique(df[["wave"]])){
  df[df["wave"] == w,][["raterCount"]] = length(unique(df[df["wave"] == w,][["id"]]))
  df[df["wave"] == w,][["rateeCount"]] = length(unique(df[df["wave"] == w,][["partner"]]))
}

for(activity in unique(names(df)[25:40])){
  df[paste(activity,"Adj",sep="")] =  df[activity] - df["activityAvg"] 
}
men = df[df["gender"] == 1,]
women = df[df["gender"] == 0,]
men = ddply(men, .(iid) ,transform ,raterDecSum = sum(dec))
men = ddply(men, .(iid) ,transform ,raterAttrSum = sum(attr))
men = ddply(men, .(iid) ,transform ,raterSincSum = sum(sinc))
men = ddply(men, .(iid) ,transform ,raterIntelSum = sum(intel))
men = ddply(men, .(iid) ,transform ,raterFunSum = sum(fun))
men = ddply(men, .(iid) ,transform ,raterAmbSum = sum(amb))
men = ddply(men, .(iid) ,transform ,raterSharSum = sum(shar))
men = ddply(men, .(iid) ,transform ,raterLikeSum = sum(like))
men = ddply(men, .(iid) ,transform ,raterProbSum = sum(prob))


men = ddply(men, .(pid) ,transform ,rateeDecSum = sum(dec))
men = ddply(men, .(pid) ,transform ,rateeAttrSum = sum(attr))
men = ddply(men, .(pid) ,transform ,rateeSincSum = sum(sinc))
men = ddply(men, .(pid) ,transform ,rateeIntelSum = sum(intel))
men = ddply(men, .(pid) ,transform ,rateeFunSum = sum(fun))
men = ddply(men, .(pid) ,transform ,rateeAmbSum = sum(amb))
men = ddply(men, .(pid) ,transform ,rateeSharSum = sum(shar))
men = ddply(men, .(pid) ,transform ,rateeLikeSum = sum(like))
men = ddply(men, .(pid) ,transform ,rateeProbSum = sum(prob))

women = ddply(women, .(iid) ,transform ,raterDecSum = sum(dec))
women = ddply(women, .(iid) ,transform ,raterAttrSum = sum(attr))
women = ddply(women, .(iid) ,transform ,raterSincSum = sum(sinc))
women = ddply(women, .(iid) ,transform ,raterIntelSum = sum(intel))
women = ddply(women, .(iid) ,transform ,raterFunSum = sum(fun))
women = ddply(women, .(iid) ,transform ,raterAmbSum = sum(amb))
women = ddply(women, .(iid) ,transform ,raterSharSum = sum(shar))
women = ddply(women, .(iid) ,transform ,raterLikeSum = sum(like))
women = ddply(women, .(iid) ,transform ,raterProbSum = sum(prob))


women = ddply(women, .(pid) ,transform ,rateeDecSum = sum(dec))
women = ddply(women, .(pid) ,transform ,rateeAttrSum = sum(attr))
women = ddply(women, .(pid) ,transform ,rateeSincSum = sum(sinc))
women = ddply(women, .(pid) ,transform ,rateeIntelSum = sum(intel))
women = ddply(women, .(pid) ,transform ,rateeFunSum = sum(fun))
women = ddply(women, .(pid) ,transform ,rateeAmbSum = sum(amb))
women = ddply(women, .(pid) ,transform ,rateeSharSum = sum(shar))
women = ddply(women, .(pid) ,transform ,rateeLikeSum = sum(like))
women = ddply(women, .(pid) ,transform ,rateeProbSum = sum(prob))

n = names(men)
attrs = c("dec", "attr", "sinc", "intel", "fun", "amb", "shar", "like", "prob")
rateeSums = c("rateeDecSum", "rateeAttrSum", "rateeSincSum", "rateeIntelSum", "rateeFunSum", "rateeAmbSum", "rateeSharSum", "rateeLikeSum", "rateeProbSum")
raterSums = c("raterDecSum", "raterAttrSum", "raterSincSum", "raterIntelSum", "raterFunSum", "raterAmbSum", "raterSharSum", "raterLikeSum", "raterProbSum")
for(i in 1:9){
  string3 = paste(attrs[i], "RaterAvg", sep="")
  string4 = paste(attrs[i], "RateeAvg", sep="")
  string5 = paste(attrs[i], "RaterAvgExc", sep="")
  string6 = paste(attrs[i], "RateeAvgExc", sep="")
  string7 = paste(attrs[i], "RaterAdj", sep="")
  string8 = paste(attrs[i], "RateeAdj", sep="")
  
  men[string3] = men[raterSums[i]]/men["rateeCount"]
  men[string4] = men[rateeSums[i]]/men["raterCount"]
  men[string5] = (men[raterSums[i]] - men[attrs[i]])/(men["rateeCount"] - 1)
  men[string6] = (men[rateeSums[i]] - men[attrs[i]])/(men["raterCount"] - 1)
  men[string7] = men[attrs[i]] - men[string5]
  men[string8] = men[attrs[i]] - men[string6]
  
  
  women[string3] = women[raterSums[i]]/women["rateeCount"]
  women[string4] = women[rateeSums[i]]/women["raterCount"]
  women[string5] = (women[raterSums[i]] - women[attrs[i]])/(women["rateeCount"] - 1)
  women[string6] = (women[rateeSums[i]] - women[attrs[i]])/(women["raterCount"] - 1)
  women[string7] = women[attrs[i]] - women[string5]
  women[string8] = women[attrs[i]] - women[string6]
}

men = round(men, 2)
women = round(women, 2)


women_id_joins = c("iid",  "pid", "wave")
men_id_joins = c("pid", "iid", "wave")


merged = merge(women, men, by.x = women_id_joins, by.y = men_id_joins)


write.csv(men , '~/Desktop/speedDating/dataMen.csv')
write.csv(women , '~/Desktop/speedDating/dataWomen.csv')
write.csv(merged , '~/Desktop/speedDating/dataMerged.csv')

