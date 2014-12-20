
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
df = processData(df)
df = basicBinaries(df)
answer = makeCrossHash(df, c("race", "goal", "field", "career"))
df = answer[["df"]]
crossHash = answer[["crossHash"]]
crossHash[["race"]] = c("raceAsian", "raceWhite", "raceOther")
df = df[df[["wave"]] %in% c(2,4,7,9,11,12,15,19,21),] 
men = df[df["gender"] == 1,]
women = df[df["gender"] == 0,]

merged = mergeDF(men, women)
merged = makeCrossesAndFreqs(merged, crossHash)
raterAvgs = n[grep("Rater",n)]
bads = raterAvgs[!(raterAvgs %in% c("decRaterAvgW", "decRaterAvgM"))]
merged = merged[!(n %in% bads)]

n = names(merged)
colnames(merged)[grep("RatingM|AvgM",n)[-10]] = gsub("M$","W",n[grep("RatingM|AvgM",n)[-10]])
colnames(merged)[grep("RatingW|AvgW",n)[-10]] = gsub("W$","M",n[grep("RatingW|AvgW",n)[-10]])
colnames(merged)[grep("AvgW",n)[-1:0]] = gsub("W$","M",n[grep("AvgM",n)[-1:0]])





write.csv(merged, "~/Desktop/speedDating/speedDatingDataProcessed.csv")








