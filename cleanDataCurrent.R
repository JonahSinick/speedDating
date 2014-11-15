df <- read.csv('~/Desktop/speedDating/speedDatingData.csv')


#converts tuition and income from strings to integers representing thousands of dollars
featuresToIntegers = function(df, features){
  for(feature in features){
    m = as.matrix(df[[feature]])
    fixed = ifelse(m[,1] == "", NA, substring(m[,1], 1, 2))
    fixed = ifelse(fixed == "", NA, as.integer(fixed))
    fixed = ifelse(is.na(fixed), median(fixed, na.rm = TRUE), fixed)
    df[[feature]] = fixed      
  }
  return(df)
}
df = featuresToIntegers(df, c("income", "tuition"))


#drops columns with lots of missing entries and/or that seem inessential
n = names(df)
drops = n[grep("7|5|4|_s$|_o$|pf|2_|3_|1_3|1_2|num",n)]
drops = c(drops, "positin1", "field", "undergra", "from", "zipcode", "career", "you_call", "them_cal", "dat_3", "numdat_3", "num_in_3", "idg", "condtn", "position", "match_es", "date_3", "length", "mn_sat", "satis_2", "round", "int_corr")


df = df[, !(names(df) %in% drops)]

#fillings in the single missing id using information from another row
df[df["pid"] == 530 & df["iid"] == 552,][["id"]] = 22


#Fill in missing entries of remaining features
na_fixer=function(x){
  x<-as.numeric(as.character(x))
  x[is.na(x)] =median(x, na.rm=TRUE)
  return(x)
}

df = data.frame(apply(df,2,na_fixer))

#Removing waves with < 100 speed dates and one with uneven numbers of dates within same gender

df = df[!(df[["wave"]] %in% c(5, 6, 16, 18, 20)),]

n = names(df)
#Changes activity names for later reference
colnames(df)[21:37] = gsub("$", "Act" ,n[21:37])
colnames(df)[grep("1_1", n)] = gsub("1_1", "Pref" ,n[grep("1_1", n)])
colnames(df)[46:52] = gsub("$", "Rating" ,n[46:52])
/
#Wave 5 has missing rows

write.csv(df , '~/Desktop/speedDating/cleanedData.csv')
