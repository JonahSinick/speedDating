df <- read.csv('~/Desktop/speedDating/speedDatingData.csv')
f=function(x){
  x<-as.numeric(as.character(x)) #first convert each column into numeric if it is from factor
  x[is.na(x)] =median(x, na.rm=TRUE) #convert the item with NA to median value from the column
  x #display the column
}

# df = df[df[["wave"]] %in% c(2,4,7,9,11,14,15,19),]

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


indexes = c(c(1:92), c(98:108))
df = df[indexes]
drops = c("positin1", "field", "undergra", "from", "zipcode","you_call", "them_cal", "dat_3", "numdat_3", "num_in_3", "attr5_1", "sinc5_1", "intel5_1", "attr4_1",  "sinc4_1", "intel4_1", "fun4_1", "amb4_1", "attr1_s", "amb3_1",  "fun5_1", " amb5_1", "shar4_1", "amb_5", "idg", "condtn", "position", "match_es", "date_3", "amb5_1", "mn_sat")
#Dropped fields for which the waves with lots of people are NA 
df = df[, !(names(df) %in% drops)]

keeps_to_fix = names(df)[10:83]
df_rm_na = df[keeps_to_fix]
df_rm_na = data.frame(apply(df_rm_na,2,f))
df[keeps_to_fix] = df_rm_na[keeps_to_fix]
df = na.omit(df)
more_drops = names(df)[12:28]
df = df[, !(names(df) %in% more_drops)]
names(df)

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

# men = df[df["gender"] == 1,]
# women = df[df["gender"] == 0,]
df["raterCount"] = 0
df["rateeCount"] = 0
for(w in unique(df[["wave"]])){
  df[df["wave"] == w,][["raterCount"]] = length(unique(df[df["wave"] == w,][["id"]]))
  df[df["wave"] == w,][["rateeCount"]] = length(unique(df[df["wave"] == w,][["partner"]]))
}




for(str in c("match", names(df)[59:68])){
  own_sum = paste(str,"OwnSum",sep="")
  partner_sum =  paste(str,"PartnerSum",sep="")
  df[own_sum] = 0
  df[partner_sum] = 0
}

for(i in unique(df[["iid"]])){
  print(i)
  own_sum = paste(str,"OwnSum",sep="")
  partner_sum =  paste(str,"PartnerSum",sep="")
  slice1 = df[df["iid"] == i,]
  slice2 = df[df["pid"] == i,]
  for(str in c("match", names(df)[59:68])){
    df[df["iid"] == i,][[own_sum]] = sum(slice1[[str]])
    df[df["pid"] == i,][[partner_sum]] = sum(slice2[[str]])
  }
}



for(str in c("match", names(df)[60:68])){
  own_sum = paste(str,"OwnSum",sep="")
  partner_sum =  paste(str,"PartnerSum",sep="")
  df[paste(str,"OwnMean",sep="")] = df[own_sum]/df["rateeCount"]
  df[paste(str,"PartnerMean",sep="")] = df[partner_sum]/df["raterCount"]
  df[paste(str,"OwnMeanExc",sep="")] = (df[own_sum] - df[str])/(df["rateeCount"] - 1)
  df[paste(str,"PartnerMeanExc",sep="")] = (df[partner_sum] - df[str])/(df["raterCount"] - 1)
}





for(activity in unique(names(df)[25:40])){
  df[paste(activity,"Adj",sep="")] =  df[activity] - df["activityAvg"] 
}


df = round(df, 2)
men = df[df["gender"] == 1,]
women = df[df["gender"] == 0,]


women_id_joins = c("iid",  "pid", "wave")
men_id_joins = c("pid", "iid", "wave")


merged = merge(women, men, by.x = women_id_joins, by.y = men_id_joins)


write.csv(men , '~/Desktop/speedDating/dataMen.csv')
write.csv(women , '~/Desktop/speedDating/dataWomen.csv')
write.csv(merged , '~/Desktop/speedDating/dataMerged.csv')

