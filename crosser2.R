source("~/Desktop/speedDatingFinal/libraries.R")
source("~/Desktop/speedDatingFinal/machineLearning2.R")
merged = read.csv('~/Desktop/speedDatingFinal/ratingMetricsAdded.csv')

n = names(merged)
mAgged = aggregate(merged, merged["iidM"],FUN=mean)
wAgged = aggregate(merged, merged["iidW"],FUN=mean)
mPrefs = n[grep("PrefM",n)]
wPrefs = n[grep("PrefW",n)]
for(pref in mPrefs){
  print(pref)
  colName = paste(pref,"SD",sep="")
  merged[colName] = 0
  for(i in 1:nrow(mAgged)){
    iid = mAgged[i,"iidM"]
    prefScore = mAgged[i,pref]
    percentile = nrow(mAgged[mAgged[pref] <= prefScore,])/nrow(mAgged)
    mAgged[i,colName] = percentile
  }
  for(iid in unique(merged[["iidM"]])){
    merged[merged[["iidM"]] == iid,][[colName]] = mAgged[mAgged[["iidM"]] == iid,][[colName]]
  }
  merged[colName] = scale(merged[colName])
}


for(pref in wPrefs){
  print(pref)
  colName = paste(pref,"SD",sep="")
  merged[colName] = 0
  for(i in 1:nrow(wAgged)){
    iid = wAgged[i,"iidW"]
    prefScore = mAgged[i,pref]
    percentile = nrow(wAgged[wAgged[pref] <= prefScore,])/nrow(wAgged)
    wAgged[i,colName] = percentile
  }
  for(iid in unique(merged[["iidW"]])){
    merged[merged[["iidW"]] == iid,][[colName]] = wAgged[wAgged[["iidW"]] == iid,][[colName]]
  }
  merged[colName] = scale(merged[colName])
}

merged["prefCor"] = 0
for(i in 1:nrow(merged)){
  slice = merged[i,"prefCor"]
  prefVecM = c(merged[i,"attrPrefMSD"], merged[i,"sincPrefMSD"], merged[i,"intelPrefMSD"], merged[i,"funPrefMSD"], merged[i,"ambPrefMSD"], merged[i,"sharPrefMSD"])
  prefVecW = c(merged[i,"attrPrefWSD"], merged[i,"sincPrefWSD"], merged[i,"intelPrefWSD"], merged[i,"funPrefWSD"], merged[i,"ambPrefWSD"], merged[i,"sharPrefWSD"])
  
  merged[i,"prefCor"] = cor(prefVecM, prefVecW)
}


merged["combAvgM"] = rowSums(scale(merged[c("decAvgM", "attrAvgM", "likeAvgM")]))
merged["combAvgW"] = rowSums(scale(merged[c("decAvgW", "attrAvgW", "likeAvgW")]))

write.csv(merged, '~/Desktop/speedDatingFinal/crossesAdded.csv')
