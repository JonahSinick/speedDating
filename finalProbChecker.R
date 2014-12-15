source("~/Desktop/speedDatingFinal/libraries.R")
source("~/Desktop/speedDatingFinal/machineLearning2.R")
source("~/Desktop/speedDatingFinal/Corer.R")


merged = read.csv('~/Desktop/speedDatingFinal/crossesAdded.csv')
n = names(merged)
prefSDs = n[grep("PrefWSD|PrefMSD",n)]
wBase  = c("decRaterAvgW", "combAvgLowW", "combAvgM", "sincAvgM", "decRaterAvgHighM")
merged = results(merged, wBase[c(1:5)], "decW", "basicDecW", "linear")  

s=scale(merged[wBase],center=TRUE,scale=TRUE)
co = c(heuristicC(s))
m=LiblineaR(data=s,labels=factor(merged[,"decW"]),type=0,cost=co,bias=TRUE,verbose=FALSE)
m$W

menCorer(merged, "attrAvgM")
merged[["combAvgLowW"]] = ifelse(merged[["combAvgW"]] < 0 , -merged[["combAvgW"]], 0)
merged[["decRaterAvgHighM"]] = ifelse(merged[["decRaterAvgM"]] > 0 , merged[["decRaterAvgM"]], 0)

merged[["combAvgLowM"]] = ifelse(merged[["combAvgM"]] < 0 , -merged[["combAvgM"]], 0)
merged[["decRaterAvgHighW"]] = ifelse(merged[["decRaterAvgW"]] > 0 , -merged[["decRaterAvgW"]], 0)

merged = results(merged, mBase[c(1:4)], "decM", "basicDecM", "linear")

s=scale(merged[mBase],center=TRUE,scale=TRUE)
co = c(heuristicC(s))
m=LiblineaR(data=s,labels=factor(merged[,"decM"]),type=0,cost=co,bias=TRUE,verbose=FALSE)
m$W


mBase = c("decRaterAvgM","combAvgLowM","combAvgW", "sincAvgW")  
wBase = c("decRaterAvgW","combAvgM", "combAvgLowW", "decRaterAvgHighM")
merged = results(merged, c("decAvgW", "decRaterAvgM"), "decM", "basicDecM", "linear", thres=10)

merged = results(merged, c("decAvgM", "decRaterAvgW"), "decW", "basicDecW", "linear", thres=10)


merged["basicMatchProb"] = merged["basicDecW"]*merged["basicDecM"]
merged[["basicMatchProb"]] = probsToLORs(merged[["basicMatchProb"]])
merged = results(merged, c('basicMatchProb'), "match", "basic", "linear")  

merged[["asianMWhiteW"]] =  ifelse(merged[["raceM"]] == 4 & merged[["raceW"]] == 2, 1, 0)

merged[["combAvgHighM"]] = ifelse(merged[["combAvgM"]] > 0 , merged[["combAvgM"]]*merged[["combAvgW"]], 0)
merged[["combAvgLowM"]] = ifelse(merged[["combAvgM"]] < 0 , -merged[["combAvgM"]], 0)
merged[["combAvgCross"]] = ifelse(merged[["combAvgM"]] < 0 & merged[["combAvgW"]] >0 , 0,  merged[["combAvgM"]]*merged[["combAvgW"]])

merged[c("combAvgM", "combAvgW")] = scale(merged[c("combAvgM", "combAvgW")])


merged[["attrAvgCross"]] = ifelse(merged[["attrAvgM"]] < 0 & merged[["attrAvgW"]] > 0 , 0,  merged[["attrAvgM"]]*merged[["attrAvgW"]])

merged[["sincAttrIntM"]] = ifelse(merged[["attrAvgM"]] > 0.5, merged[["attrAvgM"]]*merged[["sincAvgW"]], 0)
merged["funAmbW"] = rowSums(scale(merged[c("funAvgW", "ambAvgW")]))/2
merged["funAmbM"] = rowSums(scale(merged[c("funAvgM", "ambAvgM")]))/2

womanTrait = "funAmbW"

merged[["funAmbTraitM"]] = ifelse(merged[["funAmbM"]] > 0 & abs(merged[["funAmbW"]]) > 0.5, merged[["funAmbW"]]*merged[["funAmbM"]], 0)

mBase = c("decRaterAvgM","combAvgW","attrAvgCross", "sincAttrIntM", "funAmbTraitM")  
merged = results(merged, mBase, "decM", "finalDecM", "linear", thres=10)

manTrait= "funAvgM"
womanTrait = "funAmbW"
merged[["funAvgCrossW"]] = ifelse(merged[[womanTrait]] > 0, merged[[manTrait]]*merged[[womanTrait]], 0)

merged[["sincAttrW"]] = ifelse(merged[["attrAvgW"]] > 0.5, merged[["sincAvgM"]]*merged[["attrAvgW"]], 0)

merged[["combLowStuffW"]] = ifelse(merged[["combAvgW"]] < 0, merged[["combAvgM"]]*merged[["combAvgW"]], 0)

wBase = c("decRaterAvgW","combAvgM", "funAvgCrossW", "combAvgLowW", "decRaterAvgHighM", "sincAttrW")

merged = results(merged, wBase, "decW", "finalDecW", "linear", thres=10)


merged["finalMatchProb"] = merged["finalDecW"]*merged["finalDecM"]
merged[["finalMatchLOR"]] = probsToLORs(merged[["finalMatchProb"]])

features = c("finalMatchLOR")
merged[["intelFeature"]] = ifelse(merged[["intelAvgM"]] != 0 & merged[["intelAvgW"]] > 0, merged[["intelAvgM"]]*merged[["intelAvgW"]], 0)
merged[["sincFeature"]] = ifelse(merged[["sincAvgM"]] != 0 & merged[["attrAvgW"]] > 0, merged[["sincAvgM"]]*merged[["attrAvgW"]], 0)



features = c("finalMatchLOR")
merged = results(merged, features, "match", "final", "linear")

printMetrics(merged[["match"]], merged[["final"]],cutoff=0.35)

s=scale(merged[features],center=TRUE,scale=TRUE)
co = c(heuristicC(s))
m=LiblineaR(data=s,labels=factor(merged[,"match"]),type=0,cost=co,bias=TRUE,verbose=FALSE)
m$W

features = wBase
waves = unique(merged[["wave"]])
llHashW = hash()
featureHash = hash()

for(wave in waves){
  llHashW[[toString(wave)]] = 10
}

for(i in 1:length(features)){
  print(features[i])
  fArr = c()
  for(wave in waves){
    train = merged[merged["wave"] != wave,]  
    test = merged[merged["wave"] == wave,] 
    test[["newProbs"]] = getProbs(train, test,features[1:i], "decW", "linear")
    newLL = logLoss(test[["decW"]], test[["newProbs"]])
    if(newLL < llHashW[[toString(wave)]]){
      llHashW[[toString(wave)]] = newLL
      fArr[length(fArr) + 1] = wave
    }
  }
  featureHash[[features[i]]] = fArr
}

newFeatureHash = hash()
for(f in keys(featureHash)){
  newFeatureHash[[f]] = length(featureHash[[f]])
}

newFeatureHash

s=scale(merged[mBase[c(1,2,3,4)]],center=TRUE,scale=TRUE)
co = c(heuristicC(s))
m=LiblineaR(data=s,labels=factor(merged[,"decM"]),type=0,cost=co,bias=TRUE,verbose=FALSE)
s2= scale(merged[wBase],attr(s,"scaled:center"),attr(s,"scaled:scale"))    
predictions =predict(m,s2,prob=TRUE)
probs = predictions$probabilities[,"1"]  
m$W

i = 4
highs = merged[merged[wAvgsH[i]] == 1 ,]

middles = merged[merged[wAvgsL[i]] != 1 & wAvgsH[i] != 1  ,]

lows = merged[merged[wAvgsL[i]] == 1 ,]

niceCors(highs, mAvgs[i] , "decW")
100*(mean(highs[["decW"]]) - mean(merged[["decW"]]))
niceCors(middles, mAvgs[i] , "decW")
100*(mean(middles[["decW"]]) - mean(merged[["decW"]]))
niceCors(lows, mAvgs[i], "decW")
100*(mean(lows[["decW"]]) - mean(merged[["decW"]]))
newFeatureHash
mAgged = aggregate(merged, merged["iidM"], FUN=mean)

mAgged["attrAvgM"] = scale(mAgged["attrAvgM"])
plot(mAgged[["decRaterAvgM"]], mAgged[["attrAvgM"]])
merged = results(merged, matchBase, "match", "probMatchFinal", "linear", thres=10)

write.csv(merged ,'~/Desktop/speedDatingFinal/matchProbAdded.csv')
