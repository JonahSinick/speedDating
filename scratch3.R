source("~/Desktop/speedDating/auxFunctions/corers.R")
source("~/Desktop/speedDating/auxFunctions/displayers.R")
source("~/Desktop/speedDating/auxFunctions/probLORConverter.R")



source("~/Desktop/speedDating/recommendationSystem/eventScheduler.R")
source("~/Desktop/speedDating/recommendationSystem/topNLists.R")


merged = read.csv("~/Desktop/speedDating/probsAdded.csv")
df = merged
scoreSlice = merged[c("iidM", "iidW", "wave",  "match","orderM", "orderW", "basic", "final")]

df = addScoreCols(df, "M", "W", "basic")
df = addScoreCols(df, "W", "M", "basic")
df = addScoreCols(df, "M", "W", "final")
df = addScoreCols(df, "W", "M", "final")
topMetrics = createTopMetrics(df, c("basic", "final"))
n = names(df)
df[n[grep("FracP",n)]]
