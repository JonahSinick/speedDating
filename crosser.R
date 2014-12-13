source("~/Desktop/speedDatingFinal/libraries.R")
source("~/Desktop/speedDatingFinal/machineLearning2.R")
merged = read.csv('~/Desktop/speedDatingFinal/ratingMetricsAdded.csv')
n = names(merged)
wAvgs = n[grep("AvgW|GuessW",n)]
mAvgs = n[grep("AvgM|GuessM",n)]
niceCors(merged, wAvgs, "decM")
merged["attrLikeDecAvgW"] = rowSums(scale(merged[c("decAvgW", "likeAvgW", "attrAvgW")]))
merged["combRatingEstW"] = rowSums(scale(merged[c("decAvgW", "likeAvgW", "attrAvgW", "attrLikeFunRatingGuessW")]))
niceCors(merged, c(wAvgs, "attrLikeDecAvgW", "combRatingEstW"), "decM")

niceCors(merged, mAvgs, "decW")
merged["attrLikeDecAvgM"] = rowSums(scale(merged[c("decAvgM", "likeAvgM", "attrAvgM")]))
merged["combRatingEstM"] = rowSums(scale(merged[c("decAvgM", "attrAvgM", "likeAvgM", "attrRatingGuessM")]))
niceCors(merged, c(mAvgs, "attrLikeDecAvgM", "combRatingEstM"), "decW")

for(race in unique(merged[["raceW"]])){
  merged[paste("raceW",race,sep="")] = ifelse(merged[["raceW"]] == race, 1, 0)
}
for(race in unique(merged[["raceM"]])){
  merged[paste("raceM",race,sep="")] = ifelse(merged[["raceM"]] == race, 1, 0)
}
for(race in unique(merged[["goalW"]])){
  merged[paste("goalW",race,sep="")] = ifelse(merged[["goalW"]] == race, 1, 0)
}
for(race in unique(merged[["goalM"]])){
  merged[paste("goalM",race,sep="")] = ifelse(merged[["goalM"]] == race, 1, 0)
}


for(i in 1:2){
  for(j in 1:2){
    mGoal = paste("goalM",toString(i),sep="")
    wGoal = paste("goalW",toString(j),sep="")
    merged[paste(mGoal,wGoal,sep="")] = merged[mGoal]*merged[wGoal]
  }
}

for(i in c(1,2,3,4,6)){
  for(j in c(1,2,3,4,6)){
    mRace = paste("raceM",toString(i),sep="")
    wRace = paste("raceW",toString(j),sep="")
    merged[paste(mRace,wRace,sep="")] = merged[mRace]*merged[wRace]
  }
}
highLowCreator = function(df, traits){
  for(trait in traits){
    df[trait] = scale(df[trait])
    df[[paste(trait,"High",sep="")]] = ifelse(df[[trait]] > 0.5, 1, 0)
    df[[paste(trait,"Low",sep="")]] = ifelse(df[[trait]] > 0.5, 1, 0)
  }
  return(df)
}
merged = highLowCreator(merged, mTraits)
n = names(merged)
colnames(merged)[grep("MHigh",n)] = gsub("MHigh","HighM" ,n[grep("MHigh",n)])
colnames(merged)[grep("MLow",n)] = gsub("MLow","LowM" ,n[grep("MLow",n)])
merged = highLowCreator(merged, wTraits)
colnames(merged)[grep("WHigh$",n)] = gsub("WHigh$","HighW" ,n[grep("WHigh",n)])
colnames(merged)[grep("WLow$",n)] = gsub("WLow$","LowW" ,n[grep("WLow",n)])
n = names(merged)
m = n[!(n %in% n[grep("Rater",n)])]
traits = m[grep("Pref|Avg",m)]
wTraits = traits[grep("W$",traits)]
mTraits = traits[grep("M$",traits)]
makeCrossTraits = function(df, traitNames1, traitNames2){
  for(i in 1:length(traitNames1)){
    mH = paste(traitNames1[i], "HighM",sep="")
    mL = paste(traitNames1[i], "LowM",sep="")
    wH = paste(traitNames2[i], "HighW",sep="")
    wL = paste(traitNames2[i], "LowW",sep="")
    for(t1 in c(mL,mH)){
      for(t2 in c(wL, wH)){
        df[paste(t1,t2,sep="")] = df[t1]*df[t2]
      }
    }
  }  
  return(df)
}

prefs = gsub("$", "Pref", c("attr", "sinc", "intel", "fun", "amb", "shar"))
avgs = gsub("$", "Avg", c("attr", "sinc", "intel", "fun", "amb", "shar"))
n = names(merged)
merged = makeCrossTraits(merged, avgs ,avgs)
merged = makeCrossTraits(merged, prefs ,avgs)
merged = makeCrossTraits(merged, avgs ,prefs)
merged = makeCrossTraits(merged, prefs ,prefs)
merged = makeCrossTraits(merged, c("likeAvg") ,c("likeAvg"))
merged = makeCrossTraits(merged, c("decAvg") ,c("decAvg"))
merged = makeCrossTraits(merged, c("attrLikeDecAvg") ,c("attrLikeDecAvg"))




merged[c("prefCor", "prefCorNoAttr", "prefCorNoAttrFun", "prefCorNoFun","avgCor","avgCorNoAttr","avgCorNoAttrFun" , "avgCorNoFun", "avgPrefCor", "avgMPrefWCor", "avgWPrefMCor")] = 0
for(i in 1:nrow(merged)){
  slice = merged[i,"prefCor"]
  prefVecM = c(merged[i,"attrPrefM"], merged[i,"sincPrefM"], merged[i,"intelPrefM"], merged[i,"funPrefM"], merged[i,"ambPrefM"], merged[i,"sharPrefM"])
  prefVecW = c(merged[i,"attrPrefW"], merged[i,"sincPrefW"], merged[i,"intelPrefW"], merged[i,"funPrefW"], merged[i,"ambPrefW"], merged[i,"sharPrefW"])
  avgVecM = c(merged[i,"attrAvgM"], merged[i,"sincAvgM"], merged[i,"intelAvgM"], merged[i,"funAvgM"], merged[i,"ambAvgM"])
  avgVecW = c(merged[i,"attrAvgW"], merged[i,"sincAvgW"], merged[i,"intelAvgW"], merged[i,"funAvgW"], merged[i,"ambAvgW"])
  merged[i,"prefCor"] = cor(prefVecM, prefVecW)
  merged[i,"prefCorNoAttr"] = cor(prefVecM[c(2,3,4,5,6)], prefVecW[c(2,3,4,5,6)])
  merged[i,"prefCorNoAttrFun"] = cor(prefVecM[c(2,3,5,6)], prefVecW[c(2,3,5,6)])
  merged[i,"prefCorNoFun"] = cor(prefVecM[c(1, 2,3,5,6)], prefVecW[c(1, 2,3,5,6)])
  merged[i,"avgCor"] = cor(avgVecM, avgVecW)
  merged[i,"avgCorNoAttr"] = cor(avgVecM[c(2,3,4,5)], avgVecW[c(2,3,4,5)])
  merged[i,"avgCorNoAttrFun"] = cor(avgVecM[c(2,3,5)], avgVecW[c(2,3,5)])
  merged[i,"avgCorNoFun"] = cor(avgVecM[c(1,2,3,5)], avgVecW[c(1,2,3,5)])
  merged[i,"avgPrefCor"] = cor(c(prefVecM, avgVecM), c(prefVecW, avgVecW))
  merged[i,"avgMPrefWCor"] = cor(c(avgVecM), c(prefVecW[1:5]))
  merged[i,"avgWPrefMCor"] = cor(c(avgVecW), c(prefVecM[1:5]))  
}

merged[["prefCor"]] = ifelse(is.na(merged[["prefCor"]]), 0, merged[["prefCor"]])
merged[["avgCor"]] = ifelse(is.na(merged[["avgCor"]]), 0, merged[["avgCor"]])
merged[["prefCorNoFun"]] = ifelse(is.na(merged[["prefCorNoFun"]]), 0, merged[["prefCorNoFun"]])
traits = c("attrAvg", "sincAvg", "intelAvg", "funAvg", "ambAvg", "sharAvg")
n = names(merged)
niceCors(merged, n[grep("Cor",n)], c("decM", "decW", "match"))


write.csv(merged, '~/Desktop/speedDatingFinal/crossesAdded.csv')
