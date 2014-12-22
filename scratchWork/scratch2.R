source("~/Desktop/speedDating/auxFunctions/corers.R")
source("~/Desktop/speedDating/auxFunctions/displayers.R")
source("~/Desktop/speedDating/auxFunctions/probLORConverter.R")
source("~/Desktop/speedDating/machineLearning/models.R")
source("~/Desktop/speedDating/machineLearning/monteCarloTCB.R")
source("~/Desktop/speedDating/machineLearning/crossValidation.R")

merged = read.csv("~/Desktop/speedDating/speedDatingDataProcessed.csv")


merged[["combAvgM"]] = rowSums(scale(merged[c("decAvgM", "likeAvgM", "attrAvgM")]))/3
merged[["combAvgW"]] = rowSums(scale(merged[c("decAvgW", "likeAvgW", "attrAvgW")]))/3
merged[["attrDecAvgW"]] = rowSums(scale(merged[c("decAvgW", "attrAvgW")]))/2
merged[["attrDecAvgM"]] = rowSums(scale(merged[c("decAvgM", "attrAvgM")]))/2
merged[["financeMCombW"]] = rowSums(merged[c("careerFinanceMcareerFinanceWCross", "careerFinanceMcareerLawWCross")])
merged[["ambMambW"]] = ifelse(merged[["ambAvgM"]] > 0 & merged[["ambAvgW"]] != 0, merged[["ambAvgM"]]*merged[["ambAvgW"]], 0)
merged[["attrMattrW"]] = ifelse(merged[["attrAvgM"]] < 0.5 & merged[["attrAvgW"]] > 0,0, merged[["attrDecAvgM"]]*merged[["attrDecAvgW"]])
merged[["attrMsincW"]] = ifelse(merged[["attrAvgM"]] > 0, merged[["attrAvgM"]]*merged[["sincAvgW"]], 0)
merged[["attrLowM"]] = ifelse(merged[["attrAvgM"]] < 0, merged[["attrAvgM"]], 0)
merged[["attrDecLowM"]] = ifelse(merged[["attrDecAvgM"]] < 0, merged[["attrDecAvgM"]], 0)
merged[["attrDecLowW"]] = ifelse(merged[["attrDecAvgW"]] < 0, merged[["attrDecAvgW"]], 0)
merged[["attrMattrWDecM"]] = ifelse(merged[["attrAvgM"]] < 0 & merged[["attrAvgW"]] > 0,0, merged[["attrDecAvgM"]]*merged[["attrDecAvgW"]])
merged[["attrDecAvgNewW"]] = rowSums(scale(merged[c("attrAvgNewW", "decAvgW")]))
merged[["combAvgNewW"]] = rowSums(scale(merged[c("attrAvgNewW", "decAvgW", "likeAvgW")]))
merged["attrAvgNewW"] = merged["attrAvgW"] + merged["attrRatingCorMCross"]*0.9
merged["decAvgNewW"] = merged["decAvgW"] + merged["decRatingCorMCross"]*0.9
merged["funAvgNewW"] = merged["funAvgW"] + merged["funRatingCorMCross"]*0.8
merged["funAmbM"] = rowSums(scale(merged[c("funAvgM", "ambAvgM")]))
merged["funAmbW"] = rowSums(scale(merged[c("funAvgW", "ambAvgW")]))
merged[["attrMsincWDecM"]] = ifelse(merged[["attrAvgM"]] > 0, merged[["attrAvgM"]]*merged[["sincAvgW"]], 0)
merged[["attrWsincMDecW"]] = ifelse(merged[["attrAvgW"]] > 0.5, merged[["attrAvgW"]]*merged[["sincAvgM"]], 0)
merged[["ambDecM"]] = ifelse(merged[["ambAvgM"]] > 0, merged[["ambAvgM"]]*merged[["ambAvgW"]], 0)

merged["funAvgMNew"] = merged["funAvgM"] + merged["funRatingCorWCross"]
merged[["funCrossDecW"]] = ifelse(merged[["funAvgW"]] >0, merged[["funAvgMNew"]]*merged[["funAvgW"]], 0)

merged[["ambAvgMNew"]] = merged[["ambRatingCorWCross"]]+ merged[["ambAvgM"]]
merged[["ambMambWDecW"]] = ifelse(abs(merged[["ambRatingCorW"]] - mean(merged[["ambRatingCorW"]])) > 0.2, merged[["ambAvgMNew"]], 0)



s=scale(merged[mFeatures],center=TRUE,scale=TRUE)
co = c(heuristicC(s))
m=LiblineaR(data=s,labels=factor(merged[,"decM"]),type=0,cost=co,bias=TRUE,verbose=FALSE)
m$W











basicMFeatures = c("decRaterAvgM", "combAvgW", "sincAvgW", "attrLowM")  
merged = makeProbs(merged, basicMFeatures, "decM", "basicDecM", "linear", thres=10)

mFeatures = c("sincRatingCorCrossW","decRaterAvgM", "combAvgNewW", "attrMattrWDecM","attrMsincWDecM","funAmbDecM")  
merged = makeProbs(merged, mFeatures, "decM", "probDecM", "linear", thres=10)


basicWFeatures = c("decRaterAvgW", "attrDecAvgM",  "funAvgM","sincAvgM", "intelAvgM","attrLowW")  
merged = makeProbs(merged, basicWFeatures, "decW", "basicDecW", "linear", thres=10)

wFeatures = c("ambMambWDecW", "decRaterAvgW", "attrDecAvgM",  "funCrossDecW","raceAsianMraceWhiteWCross", "attrLowW")  
merged = makeProbs(merged, wFeatures, "decW", "probDecW", "linear", thres=10)


merged[["basicProb"]] = merged[["basicDecW"]]*merged[["basicDecM"]]
merged[["matchProb"]] = merged[["probDecW"]]*merged[["probDecM"]]
merged[["basicProb"]] = probsToLORs(merged[["basicProb"]])
merged[["matchProb"]] = probsToLORs(merged[["matchProb"]])


matchFeatures = c("matchProb", "careerAcademicMcareerAcademicWCross")


merged = makeProbs(merged, matchFeatures, "match", "final", "linear", thres=10)
merged = makeProbs(merged, "basicProb", "match", "basic", "linear", thres=10)

printMetrics(merged[["match"]], merged[["basic"]])
printMetrics(merged[["match"]], merged[["final"]])

write.csv(merged, "~/Desktop/speedDating/probsAdded.csv")

