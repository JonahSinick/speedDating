source("~/Desktop/speedDatingFinal/libraries.R")
source("~/Desktop/speedDatingFinal/machineLearning2.R")

merged = read.csv('~/Desktop/speedDatingFinal/matchProbAdded.csv')

addScoreCols = function(df, genType, opGenType, probType){
  df["tempIdx"] = 0
  for(i in 1:nrow(df)){
    df[i,"tempIdx"] = i
  }
  recType = paste(probType,"Rec",sep="")
  orderType = paste(probType,"Order",sep="")
  matchType = paste(probType,"Match",sep="")
  names = gsub("$", genType, c(recType,orderType,matchType))
  df[names] = 0
  iidType = paste("iid",genType,sep="")
  for(iid in unique(df[[iidType]])){
    print(iid)
    slice = df[df[iidType] ==iid, ]
    ordered = slice[order(-slice[probType]),]
    ordered[names[1]]= ordered[paste("iid",opGenType,sep="")]
    for(i in 1:nrow(ordered)){
      ordered[i,names[2]] = i
    }
    ordered[names[3]] = ordered["match"]
    ordered = ordered[order(ordered["tempIdx"]),]
    df[df[iidType] ==iid,][[names[1]]] = ordered[[names[1]]]
    df[df[iidType] ==iid,][[names[2]]] = ordered[[names[2]]]
    df[df[iidType] ==iid,][[names[3]]] = ordered[[names[3]]]
  }
  df = df[!(names(df) == "tempIdx")]
  return(df)
}
scoreSlice = merged[c("iidM", "iidW", "wave",  "match","orderM", "orderW","decM","decW", "basicProbDecM", "probDecM" ,"basicProbDecW", "probDecW", "basicProbMatch", "probMatchFinal")]
set.seed = 1; scoreSlice[["random"]] = sample(1:nrow(scoreSlice))
scoreSlice["basic"] = scoreSlice["basicProbMatch"]
scoreSlice["final"] = scoreSlice["probMatchFinal"]
genders = c("M","W")
for(name in c("basic", "final")){
  scoreSlice = addScoreCols(scoreSlice, genders[1], genders[2], name)
  scoreSlice = addScoreCols(scoreSlice, genders[2], genders[1], name)
}
ss = scoreSlice

ws = ss[ss["wave"] == 4,]
iidMs = unique(ws[["iidM"]])
iidWs = unique(ws[["iidW"]])

seen = hash()
origSlice = ss[((ss[["orderM"]] %in% 1:3) & (ss[["orderW"]] %in% 1:3)) | ((ss[["orderM"]] %in% 1:3) & (ss[["orderW"]] %in% 1:3)),]
basicSlice = ss[(ss[["basicOrderM"]] %in% 1:3 & (ss[["basicOrderW"]] %in% 1:3)) | ((ss[["basicOrderM"]] %in% 1:3) & (ss[["basicOrderW"]] %in% 1:3)),]
finalSlice = ss[((ss[["finalOrderM"]] %in% 1:3) & (ss[["finalOrderW"]] %in% 1:3)) | ((ss[["finalOrderM"]] %in% 1:3) & (ss[["finalOrderW"]] %in% 1:3)),]
nrow(origSlice)
nrow(basicSlice)
nrow(finalSlice)
sum(origSlice[["match"]])
sum(basicSlice[["match"]])
sum(finalSlice[["match"]])
topMetrics = data.frame()
topMetrics[1:14,] = 0
probNames = c("random", "basic", "final")
for(probName in probNames){
  for(i in 1:14){
    WTops = scoreSlice[scoreSlice[[paste(probName,"OrderW",sep="")]] %in% (1:i),]
    MTops = scoreSlice[scoreSlice[[paste(probName,"OrderM",sep="")]] %in% (1:i),]
    wAgged = aggregate(WTops, WTops["iidW"], FUN=sum)
    mAgged = aggregate(MTops, MTops["iidM"], FUN=sum)
    mAtLeastOne = paste(probName,"AtLeastOneM",sep="")
    wAtLeastOne = paste(probName,"AtLeastOneW",sep="")
    mAvgNum = paste(probName,"AvgNumM",sep="")
    wAvgNum = paste(probName,"AvgNumW",sep="")
    inTopNM =  paste(probName,"MInTopN",sep="")
    inTopNW =  paste(probName,"WInTopN",sep="")
    matchesFoundM =  paste(probName,"FracFoundM",sep="")
    matchesFoundW =  paste(probName,"FracFoundW",sep="")
    inTopNW =  paste(probName,"WInTopN",sep="")
    topMetrics[i,mAtLeastOne] = nrow(mAgged[mAgged["match"] > 0,])/nrow(mAgged)
    topMetrics[i,wAtLeastOne] = nrow(wAgged[wAgged["match"] > 0,])/nrow(wAgged)
    topMetrics[i,mAvgNum] = sum(mAgged[["match"]])/nrow(mAgged)
    topMetrics[i,wAvgNum] = sum(wAgged[["match"]])/nrow(wAgged)
    topMetrics[i,inTopNM] = length(unique(WTops[["iidM"]]))/nrow(mAgged)
    topMetrics[i,inTopNW] = length(unique(MTops[["iidW"]]))/nrow(wAgged)
    topMetrics[i,matchesFoundM] = sum(mAgged[["match"]])/sum(scoreSlice[["match"]])
    topMetrics[i,matchesFoundW] = sum(wAgged[["match"]])/sum(scoreSlice[["match"]])
  }
}
n = names(topMetrics)
changes = n[grep("AtLeast|InTopN|FracFound",n)]
topMetrics = round(topMetrics,2)
topMetrics[changes] = 100*topMetrics[changes]



topMetrics[n[grep("AvgNumW",n)]]
topMetrics[n[grep("AtLeastOneW",n)]]
topMetrics[n[grep("FracFoundW",n)]]
topMetrics[n[grep("MInTopN",n)]]

topMetrics[n[grep("AvgNumM",n)]]
topMetrics[n[grep("AtLeastOneM",n)]]
topMetrics[n[grep("FracFoundM",n)]]
topMetrics[n[grep("MInTopM",n)]]

write.csv(scoreSlice, '~/Desktop/speedDatingFinal/scoreSlice.csv')
write.csv(topMetrics, '~/Desktop/speedDatingFinal/topMetrics.csv')

