
source("libraries.R")
merged = read.csv('~/Desktop/speedDating/ratingMetricsAdded.csv')

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
        df[newName] = 0 
        df[df[w] == 1 & df[m] == 1,newName] = 1
      }
    }
  }
  return(df)
}

merged = createCrosses(merged,menBinaries,womenBinaries, c("field","goOut","race", "career", "date", "goal"))

n = names(merged)
crosses= n[grep("Cross",n)]
badCrosses = crosses[colSums(merged[crosses]) < 100]
merged = merged[!(n %in% badCrosses)]




write.csv(merged , '~/Desktop/speedDating/binaryCrossesHandled.csv')
