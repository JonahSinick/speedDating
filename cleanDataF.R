df <- read.csv('~/Desktop/speedDating/speedDatingData.csv')

#drops columns with lots of missing entries and/or that seem inessential
n = names(df)
drops = n[grep("7|5|4|_s$|_o$|pf|2_|3_|1_3|1_2|zipcode|from|positin1|undergra|numdat_2|num_in_3",n)]
drops = c(drops, "positin1", "field", "undergra", "career", "field", 
          "idg", "condtn", "position", "match_es", "length",
          "mn_sat", "satis_2", "int_corr", "dat", "income", "tuition")


df = df[, !(names(df) %in% drops)]

#fillings in the single missing id using information from another row
df[df["pid"] == 530 & df["iid"] == 552,][["id"]] = 22


#Fill in missing entries of remaining features
na_fixer=function(x){
  x<-as.numeric(as.character(x))
  x[is.na(x)] =median(x, na.rm=TRUE)
  return(x)
}
fixedDF = data.frame(apply(df,2,na_fixer))
df[1:54] = fixedDF[c(1:54)]

n = names(df)

df = df[c(1:19,37:58)]
#Removing waves with < 100 speed dates and one with uneven numbers of dates within same gender

for(wave in unique(df[["wave"]])){
  lenM = nrow(df[df["wave"] == wave,])
  print(c(wave,lenM))  
}

df = df[!(df[["wave"]] %in% c(1, 3, 5, 6, 8, 10, 13, 14, 16, 17, 18, 20)),]

colnames(df)[c(10,12,18,19,38,39,40,41)] = c("sameRace","fieldCD", "goOut", "careerCD", "youCall", "themCall", "dateAfter", "numDatesAfter") 
n = names(df)
colnames(df)[grep("1_1", n)] = gsub("1_1", "Pref" ,n[grep("1_1", n)])
colnames(df)[28:36] = gsub("$", "Rating" ,n[28:36])

write.csv(df , '~/Desktop/speedDating/cleanedData.csv')
