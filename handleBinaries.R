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
df[["date"]] = ifelse(df[["date"]] <3, 3, df[["date"]])
df[["goOut"]] = ifelse(df[["goOut"]] >4, 4, df[["goOut"]])
binHash = hash()
binHash[["date"]] = gsub( "^", "date", c("3", "4", "5", "6", "7"))
binHash[["goOut"]] = gsub( "^", "goOut", c("1","2","3", "4"))
binHash[["fieldCD"]] = gsub("^", "field", fields)
binHash[["careerCD"]] = gsub("^", "career", careers)
binHash[["race"]] =  gsub("^", "race", races)
binHash[["goal"]] =  gsub("^", "goal", goals)

for(binName in keys(binHash)){
  bins = binHash[[binName]]
  for(idx in 1:length(bins)){
    df[[bins[idx]]] = ifelse(df[[binName]] == idx, 1, 0)
  }
}

write.csv(df, '~/Desktop/speedDating/handledBinaries.csv')

