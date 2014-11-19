merged = read.csv( '~/Desktop/speedDating/merged.csv')
n = names(merged)

formCrosses = function(df, features1, features2){
  for(f1 in features1){
    for(f2 in features2){
      p = df[[f1]]*df[[f2]]
      if(sum(p) >= 40){
        crossedName = paste(f1,f2,sep="")
        df[[crossedName]] = p
      }
    }
  }
  return(df)
}

races = n[grep("^race", n)]


for(name in races){
  merged[paste(name,"impRaceM",sep="")] = merged[name]*merged["impraceM"]
}
for(name in races){
  merged[paste(name,"impRaceW",sep="")] = merged[name]*merged["impraceW"]
}

menRaces = races[grep("M$",races)] 
womenRaces = races[grep("W$",races)] 
merged = formCrosses(merged, menRaces, womenRaces)
n = names(merged)


for(i in raceCrosses){
  colnames(merged)[i] = gsub("Mrace","MRace",  names(merged)[i])
}
n = names(merged)
raceCrosses = grep("Mrace", n)

for(name in n[raceCrosses]){
  merged[paste(name,"impRaceM",sep="")] = merged[name]*merged["impraceM"]
}
for(name in n[raceCrosses]){
  merged[paste(name,"impRaceW",sep="")] = merged[name]*merged["impraceW"]
}



n = names(merged)



careers = n[grep("career",n)]
menCareers = careers[grep("M", careers)]
womenCareers = careers[grep("W", careers)]
merged = formCrosses(merged, menCareers, womenCareers)
careerCrosses = grep("Mcareer", n)

for(i in careerCrosses){
  colnames(merged)[i] = gsub("Mcareer","MCareer",  names(merged)[i])
}

n = names(merged)
fields = n[grep("field",n)]
menFields = fields[grep("M", fields)]
womenFields = fields[grep("W", fields)]
merged = formCrosses(merged, menFields, womenFields)

n = names(merged)
fieldCrosses = grep("Mfield", n)




for(i in fieldCrosses){
  colnames(merged)[i] = gsub("Mfield","MField",  names(merged)[i])
}




write.csv(merged, '~/Desktop/speedDating/mergedCrossFeaturesAdded.csv')
