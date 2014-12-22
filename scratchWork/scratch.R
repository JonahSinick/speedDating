source("~/Desktop/speedDating/auxFunctions/corers.R")
source("~/Desktop/speedDating/auxFunctions/displayers.R")
source("~/Desktop/speedDating/auxFunctions/probLORConverter.R")
source("~/Desktop/speedDating/machineLearning/models.R")
source("~/Desktop/speedDating/machineLearning/monteCarloTCB.R")
source("~/Desktop/speedDating/machineLearning/crossValidation.R")

merged = read.csv("~/Desktop/speedDating/speedDatingDataProcessed.csv")


merged[["financeMCombW"]] = rowSums(merged[c("careerFinanceMcareerFinanceWCross", "careerFinanceMcareerLawWCross")])
merged[["ambMambW"]] = ifelse(merged[["ambAvgM"]] > 0 & merged[["ambAvgW"]] != 0, merged[["ambAvgM"]]*merged[["ambAvgW"]], 0)
merged[["attrMattrW"]] = ifelse(merged[["attrAvgM"]] < 0.5 & merged[["attrAvgW"]] > 0,0, merged[["attrDecAvgM"]]*merged[["attrDecAvgW"]])
merged[["attrMsincW"]] = ifelse(merged[["attrAvgM"]] > 0, merged[["attrAvgM"]]*merged[["sincAvgW"]], 0)
merged[["attrLowM"]] = ifelse(merged[["attrAvgM"]] < 0, merged[["attrAvgM"]], 0)



n = names(merged)
wCors = n[grep("CorW$",n)]
mCors = n[grep("CorM$",n)]
wAvgs = n[grep("AvgW$",n)]
mAvgs = n[grep("AvgM$",n)]
merged[c(wCors, mCors, wAvgs, mAvgs)] = scale(merged[c(wCors, mCors, wAvgs, mAvgs)])
wAvgs = wAvgs[2:10]
mAvgs = mAvgs[2:10]

for(i in 1:9){
  merged[paste(wCors[i],"Cross",sep="")] = merged[wCors[i]]*merged[wAvgs[i]]
  merged[paste(mCors[i],"Cross",sep="")] = merged[mCors[i]]*merged[mAvgs[i]]
}
niceCors(merged, names(merged)[297:314], "decW")

for(nf in names(merged)[297:314]){
  features = c("decRaterAvgM", "combAvgW", "financeMCombW", "ambMambW", "attrMattrW", "attrMsincW")  
  eventCV(merged, features, nf, "decM")
}

eventCV(merged, features, c(),"decM")  
s=scale(merged[features],center=TRUE,scale=TRUE)
co = c(heuristicC(s))
m=LiblineaR(data=s,labels=factor(merged[,"decM"]),type=0,cost=co,bias=TRUE,verbose=FALSE)
m$W


merged[["funMfunW"]] = ifelse(merged[["funSharAvgM"]] > 0, merged[["funSharAvgM"]]*merged[["funSharAvgW"]], 0)


merged["funSharAvgW"] = rowSums(scale(merged[c("funAvgW", "sharAvgW")]))/2
merged["ambAvgW"] = scale(merged["ambAvgW"])
merged["highAmbW"] = ifelse(merged[["ambAvgW"]] >= 0.5, merged[["ambAvgW"]],0.5)
slice = merged[merged["fieldScienceM"] == 1 & merged["fieldScienceW"] == 1,]
slice = merged[merged["fieldScienceM"] != 1 & merged["fieldScienceW"] == 1,]
mean(slice[["decM"]])
slice = merged[merged["fieldScienceM"] == 1,]


features = c("decRaterAvgW", "attrDecAvgM", "attrLowW", "raceAsianMraceWhiteWCross", 
             "raceAsianMraceAsianWCross", "raceWhiteMraceWhiteWCross", "funSharM", "intelAvgM", "sincAvgM")
eventCV(merged, features[c(1:3,7,8,9)], features[c(4,6)] ,"decW")  
niceCors(merged, c("raceAsianM", "raceWhiteM"), "sincAvgM")
slice = merged[merged["raceAsianM"] != 1,]
niceCors(slice, "sincAvgM", "decW")
n = names(merged)
slice = merged[merged["fieldEnginW"] == 1 & merged["fieldEnginM"] != 1,]
nrow(slice)
mean(slice[["decW"]])
colSums(merged[n[grep("career.*M$",n)]])
niceCors(slice, n[grep("field.*W$",n)], "decW")


nrow(slice)

length(unique(slice[["iidM"]]))
length(unique(slice[["iidW"]]))
nrow(slice)
length(unique(slice[["iidW"]]))
length(unique(slice[["iidM"]]))
unique(slice[["wave"]])
slice = merged[merged["careerCreativeM"] == 1,]

length(unique(slice[["iidW"]]))
mean(slice[["decW"]])
slice = merged[merged["fieldEnginW"] == 1,]




