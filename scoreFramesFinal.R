source("~/Desktop/speedDatingFinal/libraries.R")
source("~/Desktop/speedDatingFinal/machineLearning2.R")
library(reshape2)

merged = read.csv('~/Desktop/speedDatingFinal/matchProbAdded.csv')


n = names(merged)


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
scoreSlice = merged[c("iidM", "iidW", "wave",  "match", "basic", "final")]
# set.seed = 1; scoreSlice[["random"]] = sample(1:nrow(scoreSlice))
scoreSlice["orig"] = merged["orderM"]


genders = c("M","W")
for(name in c("basic", "final")){
  scoreSlice = addScoreCols(scoreSlice, genders[1], genders[2], name)
  scoreSlice = addScoreCols(scoreSlice, genders[2], genders[1], name)
}
ss = scoreSlice
ss = ss[!(ss[["wave"]] %in% c(2,15)),]

ss["mergedBasic"] = rowSums(ss[c("basicOrderM", "basicOrderW")])/2
ss["mergedFinal"] = rowSums(ss[c("finalOrderM", "finalOrderW")])/2

waves = unique(ss[["wave"]])
ss[c("basicOrder", "finalOrder")] = 0
for(wave in waves){
  slice = ss[ss["wave"] == wave,]
  len = length(unique(ss[ss["wave"] == wave,][["iidM"]]))
  for(i in 1:len){
    print(c(wave,i))
    arr = slice[slice["basicOrderM"] <= i &  slice["basicOrderW"] <= i,][["basicOrder"]] 
    slice[slice["basicOrderM"] <= i & slice["basicOrderW"] <= i,][["basicOrder"]] = ifelse(arr == 0, i, arr)
    arr = slice[slice["finalOrderM"]<= i & slice["finalOrderW"] <= i,][["finalOrder"]] 
    slice[slice["finalOrderM"]  <= i & slice["finalOrderW"] <= i,][["finalOrder"]] = ifelse(arr == 0, i, arr)
  }
  ss[ss["wave"] == wave,c("basicOrder", "finalOrder")] = slice[,c("basicOrder", "finalOrder")] 
}

ss = scoreSlice[scoreSlice[["wave"]] %in% c(9,11,21),] 

length(unique(ss[ss["match"] ==1,][["iidM"]])) + length(unique(ss[ss["match"] ==1,][["iidW"]]))
sum(ss["match"])
topMetrics = data.frame()
topMetrics[1:22,] = 0
ss["origOrderW"] = ss["orig"]
ss["origOrderM"] = ss["orig"]
probNames = c("orig","basic", "final")
for(probName in probNames){
  for(i in 1:22){
    tops = ss[ss[paste(probName,"OrderW",sep="")] <= i  & ss[paste(probName,"OrderM",sep="")] <= i,]
    topMetrics[i,paste(probName,"NumDates",sep="")] = nrow(tops)
    matches = tops[tops["match"] ==1,]
    topMetrics[i,paste(probName,"NumMatches",sep="")] = nrow(matches)
    topMetrics[i,paste(probName,"AtLeastOne",sep="")] = length(unique(matches[["iidM"]])) + length(unique(matches[["iidW"]]))
  }
  topMetrics[paste(probName,"PercentMatch",sep="")] = topMetrics[paste(probName,"NumMatches",sep="")]/topMetrics[paste(probName,"NumDates",sep="")]
  topMetrics[paste(probName,"FracFound",sep="")] = topMetrics[paste(probName,"NumMatches",sep="")]/nrow(ss[ss["match"] == 1,])
  topMetrics[paste(probName,"FracPFound",sep="")] =  topMetrics[paste(probName,"AtLeastOne",sep="")]/103
}
n = names(topMetrics)
changes = n[grep("PercentMatch|FracFound|FracPFound",n)]
topMetrics = round(topMetrics,2)
topMetrics[changes] = 100*topMetrics[changes]
topMetrics[n[grep("NumDates",n)]]
topMetrics[n[grep("AtLeastOne",n)]]
topMetrics[n[grep("NumMatches",n)]]
topMetrics[n[grep("FracFound",n)]]


basicArr = 100*topMetrics[["basicNumDates"]]/topMetrics[["origNumDates"]]
finalArr = 100*topMetrics[["finalNumDates"]]/topMetrics[["origNumDates"]]
plot(seq(100,100,length.out=20)[c(4,8,12,16,20)], type="l", col="blue",xlim=c(1,5), ylim=c(0,100))
lines(basicArr[c(4,8,12,16,20)], type="l", col="red",xlim=c(1,5), ylim=c(0,100))
lines(finalArr[c(4,8,12,16,20)], type="l", col="green",xlim=c(1,5), ylim=c(0,100))


plot(topMetrics[["origFracFound"]][c(4,8,12,16,20)], type="l", col="blue",xlim=c(1,5), ylim=c(0,100))
lines(topMetrics[["finalFracFound"]][c(4,8,12,16,20)], type="l", col="green",xlim=c(1,5), ylim=c(0,100))



plot(topMetrics[["origFracPFound"]][c(4,8,12,16,20)], type="l", col="blue",xlim=c(1,5), ylim=c(0,100), xlab=c(20,40,60,80,100))
lines(topMetrics[["basicFracPFound"]][c(4,8,12,16,20)], type="l", col="red",xlim=c(1,5), ylim=c(0,100))
lines(topMetrics[["finalFracPFound"]][c(4,8,12,16,20)], type="l", col="green",xlim=c(1,5), ylim=c(0,100))

plot(topMetrics[["origAtLeastOne"]], type="l", col="blue",xlim=c(2,10), ylim=c(0,105))
lines(topMetrics[["basicAtLeastOne"]], type="l", col="red",xlim=c(2,10), ylim=c(0,105))
lines(topMetrics[["finalAtLeastOne"]], type="l", col="green",xlim=c(2,10), ylim=c(0,105))



ss["basicOrder"] = 0
ss["finalOrder"] = 0
ss = ss[ss[["wave"]] %in% c(2,15),]
for(wave in unique(ss[["wave"]])){
  slice = ss[ss["wave"] == wave,]
  
  for(k in 1){
    slice[slice["basicOrderM"] + slice["basicOrderW"] <= k,][["basicOrder"]] = ifelse
  }
}


slice = ss[ss["wave"] != 1,]
for(wave in unique(ss[["wave"]])){
  slice = ss[ss["wave"] == wave,]
  print(wave)
  print(nrow(slice))
  print(mean(slice[["match"]]))
  print(logLoss(slice[["match"]],slice[["basic"]]))
  print(logLoss(slice[["match"]],slice[["final"]]))
}
slice = ss[ss["wave"] == 21,]

df = data.frame()
df[1:22,] = 0
for(i in 1:22){
  oSlice = slice[slice["orig"] <= i & slice["orig"] <= i ,]
  bSlice = slice[slice["basicOrderM"] <= i & slice["basicOrderW"] <= i ,]
  fSlice = slice[slice["finalOrderM"] <= i & slice["finalOrderW"] <= i ,]
  df[i,"nOrig"] = nrow(oSlice)
  df[i,"nBas"] = nrow(bSlice)
  df[i,"nFin"] = nrow(fSlice)
  moSlice = oSlice[oSlice["match"] == 1,]
  mbSlice = bSlice[bSlice["match"] == 1,]
  mfSlice = fSlice[fSlice["match"] == 1,]
  df[i,"origMatch"] = nrow(moSlice)
  df[i,"baseMatch"] = nrow(mbSlice)
  df[i,"finMatch"] = nrow(mfSlice)
  df[i,"numPeopOrig"] = length(unique(moSlice[["iidM"]])) + length(unique(moSlice[["iidW"]]))
  
  df[i,"numPeopBas"] = length(unique(mbSlice[["iidM"]])) + length(unique(mbSlice[["iidW"]]))
  df[i,"numPeopFin"] = length(unique(mfSlice[["iidM"]])) + length(unique(mfSlice[["iidW"]]))
}

plot(df[["origMatch"]], xlim=c(0,22), ylim=c(0,70), type="l", col="black")
lines(df[["baseMatch"]], type="l", col="red",xlim=c(0,22), ylim=c(0,70))
lines(df[["finMatch"]], type="l", col="green",xlim=c(0,22), ylim=c(0,70))


plot(df[["origMatch"]], xlim=c(0,22), ylim=c(0,70), type="l", col="black")
lines(df[["baseMatch"]], type="l", col="red",xlim=c(0,22), ylim=c(0,70))
lines(df[["finMatch"]], type="l", col="green",xlim=c(0,22), ylim=c(0,70))


plot(df[["numPeopOrig"]], xlim=c(0,22), ylim=c(0,40), type="l", col="black")
lines(df[["numPeopBas"]], type="l", col="red",xlim=c(0,22), ylim=c(0,40))
lines(df[["numPeopFin"]], type="l", col="green",xlim=c(0,22), ylim=c(0,40))


lines(df[["nBas"]], xlim=c(0,22), ylim=c(0,500) col="red")
logLoss(slice[["match"]], slice[["basic"]])

logLoss(slice[["match"]], slice[["final"]])
hist(slice[["orig"]])



# Or if you want to be fancy, maybe even this:
ggplot(slice, aes("mergedBasic")) +
  geom_density(alpha = 0.2)
data = melt(slice[c("mergedFinal")])
ggplot(data,aes(x=value, fill=variable)) + geom_density(alpha=0.3)

ggplot(data,aes(x=value, fill=variable)) + geom_histogram(alpha=0.5, bin=2)
data = melt(slice[c("mergedBasic")])

ggplot(data,aes(x=value, fill=variable)) + geom_histogram(alpha=0.5, bin=2)

ns1 = slice[order(slice["orig"]),]
ns2 = slice[order(slice["mergedBasic"]),]
ns3 = slice[order(slice["mergedFinal"]),]

nrow(ns1)

k = 2
types = c("orig", "mergedBasic", "mergedFinal")
type = types[1]

ns1 = slice[order(slice[type]),]
ns1 = ns1[1:floor(nrow(ns1)/k),]
ns1 = ns1[ns1["match"] ==1, ]
nrow(ns1)
length(unique(ns1[["iidM"]])) + length(unique(ns1[["iidW"]]))



# write.csv(scoreSlice, '~/Desktop/speedDatingFinal/scoreSlice.csv')
# write.csv(topMetrics, '~/Desktop/speedDatingFinal/topMetrics.csv')
# 
