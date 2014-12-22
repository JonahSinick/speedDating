
source("~/Desktop/speedDating/auxFunctions/corers.R")
source("~/Desktop/speedDating/auxFunctions/displayers.R")
source("~/Desktop/speedDating/auxFunctions/probLORConverter.R")

source("~/Desktop/speedDating/dataProcessor/dataCleaner.R")
  source("~/Desktop/speedDating/dataProcessor/ratingMetrics/ratingAvgs.R")
  source("~/Desktop/speedDating/dataProcessor/ratingMetrics/collabFilt.R")
  source("~/Desktop/speedDating/dataProcessor/binaries/basicBinaries.R")
source("~/Desktop/speedDating/dataProcessor/merger.R")
  source("~/Desktop/speedDating/dataProcessor/binaries/crossMaker.R")


source("~/Desktop/speedDating/recommendationSystem/eventScheduler.R")
source("~/Desktop/speedDating/recommendationSystem/topNLists.R")




df  = read.csv("~/Desktop/speedDating/speedDatingData.csv")
oldDF = df
na_fixer=function(x){
  x<-as.numeric(as.character(x))
  x[is.na(x)] =median(x, na.rm=TRUE)
  return(x)
}
oldDF = data.frame(apply(oldDF,2,na_fixer))

df = processData(df)
n = names(df)
df = df[!(n %in% n[grep("Act|Ind|Pref|sharRating$|probRating$|imprace$|samerace$",n)])]
df = basicBinaries(df)
df["order"] = oldDF["order"]


df = df[df[["wave"]] %in% c(2,4,7,9,11,12,15,19,21),] 


answer = makeCrossHash(df, c("race", "goal", "field", "career"))
df = answer[["df"]]
crossHash = answer[["crossHash"]]

men = df[df["gender"] == 1,]
women = df[df["gender"] == 0,]

merged = mergeDF(men, women)
merged = makeCrossesAndFreqs(merged, crossHash)
raterAvgs = n[grep("Rater|Wave",n)]
bads = raterAvgs[!(raterAvgs %in% c("decRaterAvgW", "decRaterAvgM"))]
n = names(merged)
merged = merged[!(n %in% bads)]

n[grep("RatingM|AvgM",n)[-8]
colnames(merged)[grep("RatingM|AvgM",n)[-8]] = gsub("M$","W",n[grep("RatingM|AvgM",n)[-8]])
colnames(merged)[grep("RatingW|AvgW",n)[-8]] = gsub("W$","M",n[grep("RatingW|AvgW",n)[-8]])
colnames(merged)[grep("AvgW",n)[-1:0]] = gsub("W$","M",n[grep("AvgM",n)[-1:0]])
n[grep("RatingM|AvgM",n)]
merged["decM"] = merged["decRatingM"]
niceCors(merged, n[grep("AvgM",n)], "decW")


write.csv(merged, "~/Desktop/speedDating/speedDatingDataProcessed.csv")








