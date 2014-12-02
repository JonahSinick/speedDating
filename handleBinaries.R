df = read.csv('~/Desktop/speedDating/cleanedData.csv')


fields = c("Law", "Math", "SocialSci",
           "Med", "Engin",
           "Eng", "Hum",
           "BusEconFin", "EducAca", 
           "Sci", "SocWork", "Und",
           "PoliSci", "Film", "Arts", "Lang",
           "Arch", "Other")

careers = c("Law", "Acad", "Psych",
            "Med", "Engin", "Creative",
            "Corp", "RealEstate",
            "IntAffairs", "Und",
            "Speech", "Politics",
            "Sports", "Other", "Journ", "Arch")

races = c("Black", "White", "Latino",
          "Asian", "NativeAmer", "Other")

goals = c("funNight", "MeetNew", "GetDate",
          "SerRel", "SayDid", "Other")


dates = c("1","2","3", "4", "5", "6", "7")
goOuts = c("1","2","3", "4", "5", "6", "7")
fields = gsub("^", "field", fields)
careers = gsub("^", "career", careers)
races =  gsub("^", "race", races)
goals =  gsub("^", "goal", goals)
dates = gsub("^", "date", dates)
goOuts = gsub("^", "goOut", goOuts)

for(i in c(1:length(races))){
  df[[races[i]]] = ifelse(df[["race"]] == i, 1, 0)
}

for(i in 1:length(careers)){
  df[[careers[i]]] = ifelse(df[["careerCD"]] == i, 1, 0)
}
for(i in 1:length(fields)){
  df[[fields[i]]] = ifelse(df[["fieldCD"]] == i, 1, 0)
}

for(i in 1:length(goals)){
  df[[goals[i]]] = ifelse(df[["goal"]] == i, 1, 0)
}


for(i in 1:length(goOuts)){
  df[[goOuts[i]]] = ifelse(df[["goOut"]] == i, 1, 0)
}

for(i in 1:length(dates)){
  df[[dates[i]]] = ifelse(df[["date"]] == i, 1, 0)
}



write.csv(df, '~/Desktop/speedDating/handledBinaries.csv')

