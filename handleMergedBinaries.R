merged = read.csv('~/Desktop/speedDating/ratingMetricsAdded.csv')




n = names(merged)
goOutWDrops = c("goOut5W", "goOut6W", "goOut7W")
goOutMDrops = c("goOut6M", "goOut7M")
dateMDrops = c("date1M", "date2M")
dateWDrops = c("date1W", "date2W")


merged["goOut4W"] = rowSums(merged[c("goOut4W",goOutWDrops)])
merged["goOut5M"] = rowSums(merged[c("goOut5M",goOutMDrops)])
merged["date3M"] = rowSums(merged[c("date3M",goOutMDrops)])
merged["date3W"] = merged["date3W"] + merged["date2W"] + merged["date1W"]
merged = merged[!(names(merged) %in% c(goOutWDrops,goOutMDrops,dateMDrops,dateWDrops))]
aggedByMen = aggregate(merged, by=merged["iidM"], FUN=mean)
aggedByWomen = aggregate(merged, by=merged["iidW"], FUN=mean)
n = names(merged)
binaries = n[grep("career|goOut|field|date|goal|race", n)]

menBinaries = binaries[grep("M$", binaries)][-8:0]
womenBinaries = binaries[grep("W$", binaries)][-8:0]
colSums(aggedByMen[menBinaries])
colSums(aggedByWomen[womenBinaries])
badMenBinaries = menBinaries[colSums(aggedByMen[menBinaries]) <= 4]
badWomenBinaries = womenBinaries[colSums(aggedByWomen[womenBinaries]) <= 4]
badBinaries = c(badMenBinaries, badWomenBinaries)
merged = merged[!(n %in% badBinaries)]
n = names(merged)

binaries = n[grep("career|goOut|field|date|goal|race", n)]
menBinaries = binaries[grep("M$", binaries)][-8:0]
womenBinaries = binaries[grep("W$", binaries)][-8:0]

createCrosses = function(df,menBinaries,womenBinaries, codeWords){
  for(codeWord in codeWords){
    mens = menBinaries[grep(codeWord,menBinaries)]
    womens = womenBinaries[grep(codeWord,womenBinaries)]
    for(m in mens){
      for(w in womens){
        newName = paste(paste(m,w,sep=""),"Cross",sep="")
        df[newName] = df[m]*df[w]
      }
    }    
  }
  return(df)
}
merged = createCrosses(merged,menBinaries,womenBinaries, c("field","goOut","race", "career", "date"))
crosses = names(merged)[grep("Cross", names(merged))]
unkeptCrosses = crosses[colSums(merged[crosses]) <= 50]
merged = merged[!(names(merged) %in% unkeptCrosses)]
write.csv(merged, '~/Desktop/speedDating/mergedBinariesHandled.csv')
