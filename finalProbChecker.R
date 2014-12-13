source("~/Desktop/speedDatingFinal/libraries.R")
source("~/Desktop/speedDatingFinal/machineLearning2.R")

merged = read.csv('~/Desktop/speedDatingFinal/crossesAdded.csv')
mBase = c("decRaterAvgM", "attrLikeDecAvgM", "attrLikeDecAvgW")
merged = results(merged, mBase, "decM", "probDecM", "linear", thres=10)
wBase = c("decRaterAvgW", "attrLikeDecAvgW", "attrLikeDecAvgM", "raceM4raceW2", "attrPrefM")
merged = results(merged, wBase, "decW", "probDecW", "linear", thres=10)
merged["baseMatchProb"] = merged["probDecW"]*merged["probDecM"]
merged[["baseMatchLOR"]] = probsToLORs(merged[["baseMatchProb"]])

merged[["funAvgCross"]] = merged[["funAvgW"]]*merged[["funAvgM"]]
merged[["funAvgMPrefWCross"]] = merged[["funPrefW"]]*merged[["funAvgM"]]
merged[["funAvgWPrefMCross"]] = merged[["funPrefM"]]*merged[["funAvgW"]]

merged[["funPrefCross"]] = merged[["funPrefW"]]*merged[["funPrefM"]]
merged[["intelAvgCross"]] = merged[["intelAvgW"]]*merged[["intelAvgM"]]
merged[["ambAvgCross"]] = merged[["ambAvgW"]]*merged[["ambAvgM"]]
merged[["ambAvgCross"]] = ifelse( merged[["ambAvgW"]] > 0 & merged[["ambAvgM"]] > 0, merged[["ambAvgW"]]*merged[["ambAvgM"]], 0)

merged[["attrAvgCross"]] = ifelse( merged[["attrAvgW"]] > 0 & merged[["attrAvgM"]] > 0, merged[["attrAvgW"]]*merged[["attrAvgM"]], 0)
merged[["attrAvgCross"]] = ifelse( merged[["attrAvgW"]] > 0 & merged[["attrAvgM"]] > 0, merged[["attrAvgW"]]*merged[["attrAvgM"]], 0)
merged[["attrAvgCross"]] =merged[["attrAvgW"]]*merged[["attrAvgM"]]
merged[["attrLikeDecCross"]] =merged[["attrLikeDecAvgM"]]*merged[["attrLikeDecAvgW"]]
merged[["sincAvgCross"]] =merged[["sincAvgM"]]*merged[["sincAvgW"]]

sincs = n[grep("sinc",n)]
niceCors(merged, sincs, "match")
avgSincs = sincs[grep("Avg.*Avg",sincs)]
merged[["sincAvgCross"]] = ifelse( merged[["sincAvgW"]] > 0 & merged[["sincAvgM"]] > 0, merged[["sincAvgW"]]*merged[["sincAvgM"]], 0)
merged[["sincAvgCross2"]] = ifelse( merged[["sincAvgW"]] > 0 & merged[["sincAvgM"]] < 0 |  merged[["sincAvgW"]] < 0 & merged[["sincAvgM"]] > 0 , merged[["sincAvgW"]]*merged[["sincAvgM"]], 0)
cors = n[grep("Cor",n)]
c("raceM4raceW2", "sincAvgM","intelAvgM", "goOutM", "decRaterAvgM", "funRatingGuessW", "goalM4", "raceM1raceW1", "raceM1raceW2",
  "raceM1raceW6", "raceM4raceW2", "raceM2raceW4")

for(f in n[-25:0]){
  print(f)
  matchBase = c("baseMatchLOR",f)
  merged = results(merged, matchBase, "match", "probMatch", "linear", thres=0.3453)  
}



features = c("baseMatchLOR", "funAvgCross", "ambAvgCross","attrAvgCross", "sincAvgM")

merged[["funAvgMHWL"]] = ifelse( merged[["funAvgW"]] < 0 & merged[["funAvgM"]] > 0, -merged[["funAvgW"]]*merged[["funAvgM"]], 0)
merged[["funAvgMLWL"]] = ifelse( merged[["funAvgW"]] < 0 & merged[["funAvgM"]] < 0, merged[["funAvgW"]]*merged[["funAvgM"]], 0)
merged[["funAvgMLWH"]] = ifelse( merged[["funAvgW"]] > 0 & merged[["funAvgM"]] < 0, -merged[["funAvgW"]]*merged[["funAvgM"]], 0) 
merged[["funAvgMHWH"]] = ifelse( merged[["funAvgW"]] > 0 & merged[["funAvgM"]] > 0, merged[["funAvgW"]]*merged[["funAvgM"]], 0) 

niceCors(merged, c("funAvgMHWL","funAvgMLWL", "funAvgMLWH", "funAvgMHWH" ), "match")
merged[["ambAvgMHWL"]] = ifelse( merged[["ambAvgW"]] < 0 & merged[["ambAvgM"]] > 0, merged[["ambAvgW"]]*merged[["ambAvgM"]], 0)
merged[["ambAvgMLWL"]] = ifelse( merged[["ambAvgW"]] < 0 & merged[["ambAvgM"]] < 0, merged[["ambAvgW"]]*merged[["ambAvgM"]], 0)
merged[["ambAvgMLWH"]] = ifelse( merged[["ambAvgW"]] > 0 & merged[["ambAvgM"]] < 0, merged[["ambAvgW"]]*merged[["ambAvgM"]], 0) 
merged[["ambAvgMHWH"]] = ifelse( merged[["ambAvgW"]] > 0 & merged[["ambAvgM"]] > 0, merged[["ambAvgW"]]*merged[["ambAvgM"]], 0) 
merged[["funAvgWH"]] = ifelse( merged[["funAvgW"]] > 0 , merged[["funAvgW"]]*merged[["funAvgM"]], 0) 
merged[["funAvgMH"]] = ifelse( merged[["funAvgM"]] > 0 , merged[["funAvgW"]]*merged[["funAvgM"]], 0) 

merged[["ambAvgWH"]] = ifelse( merged[["ambAvgW"]] > 0 , merged[["ambAvgW"]]*merged[["ambAvgM"]], 0) 
merged[["ambAvgMH"]] = ifelse( merged[["ambAvgM"]] > 0 , merged[["ambAvgW"]]*merged[["ambAvgM"]], 0) 



merged[["attrAvgMHWL"]] = ifelse( merged[["attrAvgW"]] < 0 & merged[["attrAvgM"]] > 0, -merged[["attrAvgW"]]*merged[["attrAvgM"]], 0)
merged[["attrAvgMLWL"]] = ifelse( merged[["attrAvgW"]] < 0 & merged[["attrAvgM"]] < 0, merged[["attrAvgW"]]*merged[["attrAvgM"]], 0)
merged[["attrAvgMLWH"]] = ifelse( merged[["attrAvgW"]] > 0 & merged[["attrAvgM"]] < 0, -merged[["attrAvgW"]]*merged[["attrAvgM"]], 0) 
merged[["attrAvgMHWH"]] = ifelse( merged[["attrAvgW"]] > 0 & merged[["attrAvgM"]] > 0, merged[["attrAvgW"]]*merged[["attrAvgM"]], 0) 
merged[["attrAvgMH"]] = ifelse( merged[["attrAvgM"]] > 0 , merged[["attrAvgW"]]*merged[["attrAvgM"]], 0) 
merged[["attrAvgWH"]] = ifelse( merged[["attrAvgW"]] > 0 , merged[["attrAvgW"]]*merged[["attrAvgM"]], 0) 

merged[["attrAvgWH"]] = ifelse( merged[["attrAvgW"]] > 0 , merged[["attrAvgW"]]*merged[["attrAvgM"]], 0) 
merged[["attrAvgNewCross"]] = ifelse( merged[["attrAvgW"]] > 0 & merged[["attrAvgM"]] < 0, 0, merged[["attrAvgW"]]*merged[["attrAvgM"]]) 
, "attrAvgMHWL", "attrAvgMLWH", "attrAvgMHWH", "attrAvgMLWL"
merged = results(merged, c(features), "match", "final", "linear")  
wBase = c("decRaterAvgW", "attrLikeDecAvgW", "attrLikeDecAvgM", "sincAvgM", "attrPrefM", "raceM4raceW2", "funAvgWH",
          "ambAvgWH", "attrAvgCross")

merged = results(merged, wBase, "decW", "probDecW", "linear")  

merged[["attrAvgMH"]] = ifelse( merged[["attrAvgM"]] > 0 , merged[["attrAvgW"]]*merged[["attrAvgM"]], 0) 
merged[["attrAvgML"]] = ifelse( merged[["attrAvgM"]] < 0 , merged[["attrAvgW"]]*merged[["attrAvgM"]], 0) 

merged[["attrAvgWH"]] = ifelse( merged[["attrAvgW"]] > 0 , merged[["attrAvgW"]]*merged[["attrAvgM"]], 0) 
merged[["attrLikeDecAvgMH"]] = ifelse( merged[["attrLikeDecAvgM"]] > 0 , merged[["attrLikeDecAvgM"]]*merged[["attrLikeDecAvgW"]], 0) 

mBase = c("decRaterAvgM", "attrLikeDecAvgM", "attrLikeDecAvgW", "funAvgMH", "attrAvgCross")
merged = results(merged, mBase[1:3], "decM", "basicDecM", "linear", thres=10)

merged = results(merged, mBase, "decM", "probDecM", "linear", thres=10)
wBase = c("decRaterAvgW", "attrLikeDecAvgW", "attrLikeDecAvgM", 
            "attrPrefM", "raceM4raceW2", "funAvgWH")
merged = results(merged, wBase[1:4], "decW", "basicDecW", "linear", thres=10)

merged = results(merged, wBase, "decW", "probDecW", "linear")
merged["basicMatchProb"] = merged["basicDecW"]*merged["basicDecM"]
merged["baseMatchProb"] = merged["probDecW"]*merged["probDecM"]
merged[["baseMatchLOR"]] = probsToLORs(merged[["baseMatchProb"]])
features = c("baseMatchLOR", "sincAvgM", "ambAvgWH")

merged = results(merged, features, "match", "final", "linear")

merged = results(merged, c('basicMatchProb'), "match", "basic", "linear")  
merged = results(merged, c("baseMatchLOR", "sincAvgM", "ambAvgWH"), "match", "final", "linear")  


printMetrics(merged[["match"]],merged[["basic"]], cutoff = 0.37)
printMetrics(merged[["match"]],merged[["final"]], cutoff = 0.3135)

wBase = c("decRaterAvgW", "attrLikeDecAvgW", "attrLikeDecAvgM", "attrPrefM", "raceM4raceW2")




features = mBase
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
    test[["newProbs"]] = getProbs(train, test,features[1:i], "decM", "linear")
    newLL = logLoss(test[["decM"]], test[["newProbs"]])
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
