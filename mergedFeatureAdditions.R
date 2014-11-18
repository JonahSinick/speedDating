merged = read.csv( '~/Desktop/speedDating/merged.csv')
n = names(merged)
n[grep("race", n)]

formCrosses = function(df, features1, features2){
  for(f1 in features1){
    for(f2 in features2){
      p = df[[f1]]*df[[f2]]
      if(sum(p) >= 40){
        crossed_name = paste(f1,"Cross",sep="")
        merged[paste(crossed_name,f2,sep="_")] = p
      }
    }
  }
  return(merged)
}
races = n[grep("^race", n)]
menRaces = races[grep("M$"),races] 
womenRaces = races[grep("W$"),races] 

merged = formCrosses(merged, menRaces, womenRaces)

careers = n[grep("career",n)]
m_careers = careers[grep("_M", careers)]
w_careers = careers[grep("_W", careers)]
merged = formCrosses(merged, m_careers, w_careers)

fields = n[grep("field",n)]
m_fields = fields[grep("_M", fields)]
w_fields = fields[grep("_W", fields)]
merged = formCrosses(merged, m_fields, w_fields)


write.csv(merged, '~/Desktop/speedDating/mergedCrossFeaturesAdded.csv')
