source("~/Desktop/speedDating/auxFunctions/corers.R")
source("~/Desktop/speedDating/auxFunctions/displayers.R")
source("~/Desktop/speedDating/auxFunctions/probLORConverter.R")
source("~/Desktop/speedDating/machineLearning/models.R")
source("~/Desktop/speedDating/machineLearning/monteCarloTCB.R")
source("~/Desktop/speedDating/machineLearning/crossValidation.R")

merged = read.csv("~/Desktop/speedDating/speedDatingDataProcessed.csv")
n = names(merged)
wCors = n[grep("CorSDW",n)]
mCors = n[grep("CorSDM",n)]
niceCors(merged, mCors, "decAvgM")
niceCors(merged, wCors, "decAvgW")
merged["intelAmb"] = rowSums(scale(merged[c("intelAvgM", "ambAvgM")]))
merged[["attrLowWOld"]] = ifelse(merged[["attrAvgW"]] < 0 , merged[["attrAvgW"]], 0)
slice = merged[merged["attrLowW"] != 0 ,]
nrow(slice)               
merged[["attrPrefLowDecW"]] = ifelse(merged[["attrRatingCorSDW"]] < -0.8, 1, 0)
merged[["attrAvgNewM"]] = (1 - merged[["attrPrefLowDecW"]])*merged[["attrAvgM"]]

merged["decRaterAvgMScaled"] = scale(merged["decRaterAvgM"])
merged["decRaterAvgMHigh"] = ifelse(merged[["decRaterAvgMScaled"]] !=0, merged[["decRaterAvgMScaled"]], 0)
merged[["newComboDecW"]] = scale(merged["attrAvgNewM"]) + scale(merged["decAvgM"]) - 0.2*scale(merged["decRaterAvgMHigh"])
newWFeatures = c("decRaterAvgW", "newComboDecW", "newFunCrossDecW", "attrLowW", "attrWsincMCross")
merged[["attrWsincMCross"]] = ifelse(merged[["attrAvgW"]] > 0.5  & merged[["sincAvgM"]]  > -1, merged[["attrAvgW"]]*merged[["sincAvgM"]], 0)


uniquesCV(merged, newWFeatures, c(), "decW", "wave")

merged[["funPrefHighW"]] = ifelse(merged[["funRatingCorSDW"]] > 1.4, 1, 0)
merged[["funPrefLowW"]] = ifelse(merged[["funRatingCorSDW"]] < -1.4, 1, 0)


merged["funAvgMNew"] = (1 + 2*merged["funPrefHighW"] - merged["funPrefLowW"])*merged["funAvgM"]

merged[["newFunCrossDecW"]] = ifelse(merged[["funAvgW"]] > 0 & merged[["funAvgMNew"]], merged[["funAvgMNew"]]*merged[["funAvgW"]], 0)



table(merged[["attrWsincMCross"]] > 0)
slice = merged[merged["attrWsincMCross"] != 0, ]
unique(slice[["iidW"]])


unique(slice[["iidM"]])


merged[["attrLowW"]] = ifelse(merged[["attrAvgW"]] < 0 & merged[["attrAvgM"]]  < 0, merged[["attrAvgW"]]/merged[["attrAvgM"]], 0)


for(i in 1:length(newWFeatures)){
  uniquesCV(merged, newWFeatures[-i], newWFeatures[i], "decW", "iidM")
  uniquesCV(merged, newWFeatures[-i], newWFeatures[i], "decW", "iidW")
  uniquesCV(merged, newWFeatures[-i], newWFeatures[i], "decW", "wave")
}

features = matchFeatures
g = function(){
  for(i in c(1:2)){
    print(i)
    for(wave in unique(merged[["wave"]])){
      slice = merged[merged["wave"] != wave,][c(features, "decM", "decW", "match", "wave", "iidM", "iidW")]
      uniquesCV(slice, features[-i], features[i], "match", "wave")
    }
  }  
}
g()
uniquesCV(merged, features[-i], features[i], "decM", "iidM")
uniquesCV(merged, features[-i], features[i], "decM", "iidW")
uniquesCV(merged, features[-i], features[i], "decM", "wave")



basicWFeatures = c("decRaterAvgW", "attrDecAvgM",  "funAvgM","sincAvgM", "intelAvgM","attrLowWOld")  
s=scale(merged[newWFeatures],center=TRUE,scale=TRUE)
co = c(heuristicC(s))
m=LiblineaR(data=s,labels=factor(merged[,"decW"]),type=0,cost=co,bias=TRUE,verbose=FALSE)
m$W


merged[["highAttrPrefM"]] = ifelse(merged[["attrRatingCorSDM"]]  > 1.53,1, 0 )
merged[["lowAttrPrefM"]] = ifelse(merged[["attrRatingCorSDM"]]  < -1.35, 1, 0 )
merged[["mediumAttrPrefM"]] = 1 - merged[["lowAttrPrefM"]] - merged[["highAttrPrefM"]]
merged["decRaterAvgWScaled"] = scale(merged["decRaterAvgW"])
merged[["decRaterAvgWHigh"]] = ifelse(merged[["decRaterAvgWScaled"]] > 0.5, merged[["decRaterAvgWScaled"]], 0)



merged[["attrMsincWDecM"]] = ifelse(merged[["attrAvgM"]] > 0.5, merged[["attrAvgM"]]*merged[["sincAvgW"]], 0)

merged["comCorSDW"] = rowSums(scale(merged[c("attrRatingCorSDW", "funRatingCorSDW", "ambRatingCorSDW")]))/3
merged[["funAmbDecM"]] = ifelse(merged[["ambAvgM"]] > 0 & abs(merged[["ambAvgW"]]) > 0.5, merged[["ambAvgM"]]*merged[["ambAvgW"]], 0)



merged["attrAvgNewW"] = (0*merged["lowAttrPrefM"] + merged["mediumAttrPrefM"] + 4*merged["highAttrPrefM"])*merged["attrAvgW"]
merged["combAvgNewW"] = (scale(merged["attrAvgNewW"]) + scale(merged["decAvgW"]) + scale(merged["likeAvgW"]) - 0.2*scale(merged["decRaterAvgWHigh"]))


merged[["attrCrossNewDecM"]] = ifelse(merged[["attrAvgW"]] < 0, merged[["attrAvgW"]]*merged[["attrAvgM"]], 0)


newMFeatures = c("decRaterAvgM", "combAvgNewW", "attrCrossNewDecM", "attrMsincWDecM", "funAmbDecM")  
uniquesCV(merged, newMFeatures, c(), "decM", "wave")

for(i in 1:)
slice = merged[merged["highAttrPrefM"] != 1 & merged["attrAvgM"] > 0.77,]
cor(slice[["decM"]],slice[["attrAvgW"]])
nrow(slice)
mean(slice[["attrAvgM"]])
mean(slice[["decAvgM"]])
hist(slice[["decAvgM"]])
newestMFeatures = c("decRaterAvgM", "combAvgW", "attrCrossNewDecM", "funAmbDecM")

niceCors(merged, newMFeature, "decM")
mFeatures = c("sincRatingCorCrossW","decRaterAvgM", "combAvgNewW", "attrMattrWDecM","attrMsincWDecM","funAmbDecM")  



merged = makeProbs(merged, newMFeatures, "decM", "probDecM", "linear", thres=10)
# 
# 
merged = makeProbs(merged, newWFeatures, "decW", "probDecW", "linear", thres=10)
min(merged[["probDecM"]])
min(merged[["probDecW"]])
slice = merged[merged["probDecW"] < 0.02,]
nrow(slice)
mean(slice[["probDecW"]])
mean(slice[["decW"]])
# 
merged[["matchProb"]] = merged[["probDecW"]]*merged[["probDecM"]]
mean(merged[merged["matchProb"] < 0.05,][["match"]])
length(unique(merged[merged["final"] < 0.01,][["iidM"]]))
slice = merged[merged[["iidM"]] %in% unique(merged[merged["final"] < 0.01,][["iidM"]]),]
nrow(slice)
length(unique(slice[slice["match"] == 1,][["iidM"]]))
sum(slice[["match"]])
merged[["matchProb"]] = probsToLORs(merged[["matchProb"]])

merged[["careerAcademicCross"]] = merged[["careerAcademicM"]]*merged[["careerAcademicW"]]
matchFeatures = c("matchProb", "careerAcademicCross")

for(name in matchFeatures){
  uniquesCV(merged, c("matchProb"), c(name), "match", "wave")  
}
n = names(merged)
merged[n[grep("Avg",n)]] = scale(merged[n[grep("Avg",n)]])
agged = aggregate(merged, by=merged["iidW"], FUN=mean)
agged[n[grep("Rating",n)]] = scale(agged[n[grep("Rating",n)]])
ggplot(agged, aes(x = attrRatingW)) + geom_histogram(binwidth = 0.4)  + coord_cartesian(xlim = c(-2.5, 2.5), ylim = c(0, 60))

merged = makeProbs(merged, matchFeatures, "match", "final", "linear", thres=10)
length(merged[merged["final"] > 0.3,][["match"]])
mean(merged[merged["final"] > 0.3,][["match"]])
length(unique(merged[merged["final"] > 0.3,][["iidW"]]))
printMetrics(merged[["match"]],merged[["final"]],cutoff=0.33)


# 
# decRaterAvgM
# 
# 
# 
# merged = makeProbs(merged, "basicProb", "match", "basic", "linear", thres=10)
# 
# printMetrics(merged[["match"]], merged[["basic"]])
# printMetrics(merged[["match"]], merged[["final"]])
# 
# write.csv(merged, "~/Desktop/speedDating/probsAdded.csv")

sum(merged[["highAttrPrefM"]])
merged[["combAvgNewW"]] = rowSums(scale(merged[c("attrAvgNewW", "decAvgW", "likeAvgW")]))

merged["attrAvgNewW2"] = (1*merged["lowAttrPrefM"] + merged["mediumAttrPrefM"] + 2*merged["highAttrPrefM"])*merged["attrAvgW"]



hist(merged[["attrAvgW"]])
# 
# 
slice = merged
merged["ambAvgW"] = scale(merged["ambAvgW"])
merged["ambAvgM"] = scale(merged["ambAvgM"])
sd(merged[["ambAvgW"]])
hist(merged[["funAvgM"]])
agged = aggregate(merged, merged["iidW"], FUN=mean)
agged2 = aggregate(merged, merged["iidM"], FUN=mean)
hist(agged[["ambAvgW"]])
hist(scale(merged["intelRatingM"]))
slice = merged[merged["attrRatingM"] == 9 ,]
nrow(slice)

mean(slice[["attrAvgM"]])
length(unique(slice[["iidM"]]))
slice = merged[merged["ambRatingW"] > merged["ambRatingM"] & merged["attrAvgM"] > 0,]
mean(slice[["decM"]])
specials = agged2[agged2["ambAvgM"] < -3,]
mean(specials[["sincAvgM"]])
slice = agged2[agged2["ambAvgM"] > 1,]
nrow(slice)
names(slice)
mean(slice[["attrAvgW"]])
hist(agged[["attrAvgW"]])
ordered = slice[order(slice["intelAvgM"]),]
len = floor(nrow(ordered)/10)
i = 9
slice2 = ordered[(1 + i*len):((i+1)*len),]
mean(slice2[["decM"]])
mean(slice)
cor(slice2[["decM"]], slice2[["intelAvgW"]])
slice3 = slice2[slice2["attrAvgW"] < -1,]
nrow(slice3)
slice3[["intelAvgW"]]
cor(slice3[["decW"]], slice3[["intelAvgM"]])
 slice = merged[merged["attrAvgW"] > 1.5 & merged["decAvgW"] > 1.5 & merged["likeAvgW"] > 1.5,]
mean(slice[["funAvgW"]])
mean(slice2[["attrAvgW"]])
mean(slice[["decW"]])
# 
# 
# # nrow(slice)/nrow(ordered)
# # mean(slice[["decM"]])
# # mean(slice[["combAvgM"]])
# 
# mean(slice[["decM"]])
# cor(slice[["decM"]], slice[["attrAvgW"]])
# 
f = function(lower,upper){
  vec0 = c()
  vec1 = c()
  vec2 = c()
  vec3 = c()
  for(i in seq(lower, upper, by=0.1)){
    slice = merged[merged["funRatingCorSDW"] >i & merged["funRatingCorSDW"] < i + 0.5,]
    print(nrow(slice))
    vec0 = c(vec0, i)
    vec1 = c(vec1, (cor(slice[["decW"]],slice[["funAvgM"]])))
    vec2 = c(vec2, mean(slice[["funRatingCorW"]]))
    vec3 = c(vec3, mean(slice[["decW"]]))
  }
  # 
  plot(vec1, type="line", col="blue",xlim=c(0, 25), ylim=c(0,0.7))
  
  title(main="Fun correlations", col.main="red", font.main=4)
  lines(vec2, type="line", col="red")
}
f(-1.5,1.5)

cor(vec1, vec2)
# 
# merged[["combAvgM"]] = rowSums(scale(merged[c("decAvgM", "likeAvgM", "attrAvgM")]))/3
# merged[["combAvgW"]] = rowSums(scale(merged[c("decAvgW", "likeAvgW", "attrAvgW")]))/3
# merged[["attrDecAvgW"]] = rowSums(scale(merged[c("decAvgW", "attrAvgW")]))/2
# merged[["attrDecAvgM"]] = rowSums(scale(merged[c("decAvgM", "attrAvgM")]))/2
# merged[["attrMattrW"]] = ifelse(merged[["attrAvgM"]] < 0.5 & merged[["attrAvgW"]] > 0,0, merged[["attrDecAvgM"]]*merged[["attrDecAvgW"]])
# merged[["attrMsincW"]] = ifelse(merged[["attrAvgM"]] > 0, merged[["attrAvgM"]]*merged[["sincAvgW"]], 0)
# merged[["attrLowM"]] = ifelse(merged[["attrAvgM"]] < 0, merged[["attrAvgM"]], 0)
# merged[["attrDecLowM"]] = ifelse(merged[["attrDecAvgM"]] < 0, merged[["attrDecAvgM"]], 0)
# merged[["attrDecLowW"]] = ifelse(merged[["attrDecAvgW"]] < 0, merged[["attrDecAvgW"]], 0)
# merged[["attrMattrWDecM"]] = ifelse(merged[["attrAvgM"]] < 0 & merged[["attrAvgW"]] > 0,0, merged[["attrDecAvgM"]]*merged[["attrDecAvgW"]])
# merged[["attrDecAvgNewW"]] = rowSums(scale(merged[c("attrAvgNewW", "decAvgW")]))
# merged[["combAvgNewW"]] = rowSums(scale(merged[c("attrAvgNewW", "decAvgW", "likeAvgW")]))
# merged["attrRatingCorMCross"] = merged["attrRatingCorM"]*merged["attrAvgW"]
# merged["decAvgNewW"] = merged["decAvgW"] + merged["decRatingCorMCross"]*0.9
# merged["funAvgNewW"] = merged["funAvgW"] + merged["funRatingCorMCross"]*0.8
# merged["funAmbM"] = rowSums(scale(merged[c("funAvgM", "ambAvgM")]))
# merged["funAmbW"] = rowSums(scale(merged[c("funAvgW", "ambAvgW")]))
# 
# merged[["attrWsincMDecW"]] = ifelse(merged[["attrAvgW"]] > 0.5, merged[["attrAvgW"]]*merged[["sincAvgM"]], 0)
# merged[["ambDecM"]] = ifelse(merged[["ambAvgM"]] > 0, merged[["ambAvgM"]]*merged[["ambAvgW"]], 0)
# 
# merged["funAvgMNew"] = merged["funAvgM"] + merged["funRatingCorWCross"]
# 
# merged[["ambAvgMNew"]] = merged[["ambRatingCorWCross"]]+ merged[["ambAvgM"]]
# 
# merged["attrAvgNewM"]
# basicWFeatures = c("decRaterAvgW", "attrDecAvgM",  "funAvgM","sincAvgM", "intelAvgM","attrLowW")  
# merged = makeProbs(merged, basicWFeatures, "decW", "basicDecW", "linear", thres=10)
# basicMFeatures = c("decRaterAvgM", "combAvgW", "sincAvgW", "attrLowM")  
# merged[["lowAttrPrefM"]] = ifelse(merged[["attrRatingCorSDM"]]  < -1.35, 1, 0 )
# merged[["highAttrPrefM"]] = ifelse(merged[["attrRatingCorSDM"]]  > 1.53,1, 0 )
# merged[["mediumAttrPrefM"]] = 1 - merged[["lowAttrPrefM"]] - merged[["highAttrPrefM"]]
# 
# sum(merged[["highAttrPrefM"]])
# merged["attrAvgNewW"] = (-0.1*merged["lowAttrPrefM"] + merged["mediumAttrPrefM"] + 2*merged["highAttrPrefM"])*merged["attrAvgW"]
# merged[["combAvgNewW"]] = rowSums(scale(merged[c("attrAvgNewW", "decAvgW", "likeAvgW")]))
# 
# merged["attrAvgNewW2"] = (1*merged["lowAttrPrefM"] + merged["mediumAttrPrefM"] + 2*merged["highAttrPrefM"])*merged["attrAvgW"]
# 
# 
# mFeatures = c("sincRatingCorCrossW","decRaterAvgM", "combAvgNewW", "attrMattrWDecM","attrMsincWDecM","funAmbDecM")  
# 
# merged[["attrMsincWDecM"]] = ifelse(merged[["attrAvgM"]] > 0, merged[["attrAvgM"]]*merged[["sincAvgW"]], 0)
# 
# merged[["attrMattrWDecM"]] = ifelse(merged[["attrAvgM"]] < 0 & merged[["attrAvgW"]] > 0,0, merged[["attrDecAvgM"]]*merged[["attrDecAvgW"]])
# merged["comCorSDW"] = rowSums(scale(merged[c("attrRatingCorSDW", "funRatingCorSDW", "ambRatingCorSDW")]))/3
# niceCors(merged, names(merged)[grep("CorSD", names(merged))], "decM")
# 
# uniquesCV(merged, mFeatures[-i], mFeatures[i], "decM", "iidM")
# uniquesCV(merged, mFeatures[-i], mFeatures[i], "decM", "iidW")
# uniquesCV(merged, mFeatures[-i], mFeatures[i], "decM", "wave")
# 
# mFeatures = c("decRaterAvgM", "combAvgNewW", "attrMattrWDecM", "attrMsincWDecM", "comCorSDW", "funAmbDecM")
# 
# merged[["funAmbDecM"]] = ifelse(merged[["ambAvgM"]] > 0 , merged[["ambAvgM"]]*merged[["ambAvgW"]], 0)
# 
# uniquesCV(merged, mFeatures[-i], mFeatures[i], "decM", "iidM")
# uniquesCV(merged, mFeatures[-i], mFeatures[i], "decM", "iidW")
# uniquesCV(merged, mFeatures[-i], mFeatures[i], "decM", "wave")
# for(i in 1:length(mFeatures)){
#   i = length(mFeatures)
#   uniquesCV(merged, mFeatures[-i], mFeatures[i], "decM", "iidM")
#   uniquesCV(merged, mFeatures[-i], mFeatures[i], "decM", "iidW")
#   uniquesCV(merged, mFeatures[-i], mFeatures[i], "decM", "wave")
# }
# 
# 
# 
# 
# merged = makeProbs(merged, basicMFeatures, "decM", "basicDecM", "linear", thres=10)
# 
# merged = makeProbs(merged, mFeatures, "decM", "probDecM", "linear", thres=10)
# 
# printMetrics(merged[["decM"]], merged[["probDecM"]])
# 
# merged[["funCrossDecW"]] = ifelse(merged[["funAvgW"]] >0, merged[["funAvgM"]]*merged[["funAvgW"]], 0)
# 
# 
# basicWFeatures = c("decRaterAvgW", "attrDecAvgM",  "funAvgM","sincAvgM", "intelAvgM","attrLowW")  
# merged = makeProbs(merged, basicWFeatures, "decW", "basicDecW", "linear", thres=10)
# 
# merged["raceAsianMraceWhiteWCross"] = merged["raceAsianM"]*merged["raceAsianW"]
# merged["careerAcademicMcareerAcademicWCross"] = merged["careerAcademicM"]*merged["careerAcademicW"]
# 
# 
# merged[["attrLowW"]] = ifelse(merged[["attrAvgW"]] < 0, merged[["attrAvgW"]], 0)
# merged[["ambMambWDecW"]] = ifelse(merged[["ambRatingCorSDW"]] > 1.4, merged[["ambAvgM"]], 0)
# merged[["funPrefHighW"]] = ifelse(merged[["funRatingCorSDW"]] > 1.4, 1, 0)
# merged[["funPrefLowW"]] = ifelse(merged[["funRatingCorSDW"]] < -1.4, 1, 0)
# sum(merged[["funPrefLowW"]])
# 
# merged["funAvgMNew"] = (1 + 2*merged["funPrefHighW"] - merged["funPrefLowW"])*merged["funAvgM"]
# 
# merged[["ambMambWDecW"]] = ifelse(merged[["ambRatingCorSDW"]] > 1.4 & merged[["ambAvgM"]] > 1, merged[["ambAvgM"]], 0)
# merged[["attrWsincMDecW"]] = ifelse(merged[["attrAvgW"]] > 0.5 & merged[["sincAvgM"]] > 0, merged[["attrAvgW"]]*merged[["sincAvgM"]], 0)
# 
# 
# merged[["attrDecW"]] = ifelse(merged[["attrAvgW"]] >0, merged[["attrAvgW"]]*merged[["funAvgM"]], 0)
# 
# merged[["funCrossDecW"]] = ifelse(merged[["funAvgW"]] >0, merged[["funAvgMNew"]]*merged[["funAvgW"]], 0)
# wFeatures = c("decRaterAvgW", "attrDecAvgM", "attrLowW", "funCrossDecW", "decRaterAvgM")  
# niceCors(merged, n[grep("Cor",n)], "decW")
# merged[["newCorDecW"]] = rowSums(scale(merged[c("attrRatingCorSDM", "ambRatingCorSDM", "funRatingCorSDM")]))
# uniquesCV(merged, wFeatures, c("funRatingCorSDW"), "decW", "wave")
# 
# grep("goalMeetNewW", names(merged))
# for(f in names(merged)[22:0]){
#   uniquesCV(merged, wFeatures, f, "decW", "iidW")
# }
# 
# for(i in 1:length(wFeatures)){
#   i = 4
#   uniquesCV(merged, wFeatures[-i], wFeatures[i], "decW", "iidM")
#   uniquesCV(merged, wFeatures[-i], wFeatures[i], "decW", "iidW")
#   uniquesCV(merged, wFeatures[-i], wFeatures[i], "decW", "wave")
# }
# 
# uniquesCV(merged, features[-i], features[i], "decM", "iidM")
# merged = makeProbs(merged, mFeatures, "decM", "probDecM", "linear", thres=10)
# 
# 
merged = makeProbs(merged, newWFeatures, "decW", "probDecW", "linear", thres=10)
# 
# 
# merged[["basicProb"]] = merged[["basicDecW"]]*merged[["basicDecM"]]
# merged[["matchProb"]] = merged[["probDecW"]]*merged[["probDecM"]]
# merged[["basicProb"]] = probsToLORs(merged[["basicProb"]])
# merged[["matchProb"]] = probsToLORs(merged[["matchProb"]])
# n = names(merged)
# n = n[!(n %in% n[grep("goal|RatingM$|RatingW$|career|field",n)])]
# matchFeatures = c("matchProb", "careerAcademicMcareerAcademicWCross")
# for(name in n){
#   uniquesCV(merged, matchFeatures, c(name), "match", "iidM")  
# }
# 
# decRaterAvgM
# 
# 
# 
# merged = makeProbs(merged, matchFeatures, "match", "final", "linear", thres=10)
# merged = makeProbs(merged, "basicProb", "match", "basic", "linear", thres=10)
# 
# printMetrics(merged[["match"]], merged[["basic"]])
# printMetrics(merged[["match"]], merged[["final"]])
# 
# write.csv(merged, "~/Desktop/speedDating/probsAdded.csv")
# 
printMetrics(merged[["decW"]], merged[["probDecW"]])

