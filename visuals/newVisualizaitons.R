source("~/Desktop/speedDatingFinal/libraries.R")
source("~/Desktop/speedDatingFinal/machineLearning2.R")
library(reshape2)

merged = read.csv('~/Desktop/speedDatingFinal/matchProbAdded.csv')


n = names(merged)
n[grep("Guess",n)]


slice = merged[merged["wave"] == 21,]

slice = slice[c("iidW", "iidM", "decW","attrRatingM", "likeRatingM", "attrAvgM", "attrRaterAvgW", "likeAvgM", "likeRaterAvgW", "decAvgM","decRaterAvgW")]
slice = slice[order(slice["iidW"]),]
slice["iid"] = slice["iidW"]
slice["pid"] = slice["iidM"]
slice["decRatingM"] = slice["decW"]
slice = addGuessToWave(slice, "attrRatingM", "attrRatingUBCF", "UBCF")
slice = addGuessToWave(slice, "likeRatingM", "likeRatingUBCF", "UBCF")
slice = addGuessToWave(slice, "decRatingM", "decRatingUBCF", "UBCF")
slice[["attrRatingUBCF"]] = ifelse(slice[["attrRatingUBCF"]] <1, 1, slice[["attrRatingUBCF"]])
slice[["attrRatingUBCF"]] = ifelse(slice[["attrRatingUBCF"]] >10, 10, slice[["attrRatingUBCF"]])
slice[["likeRatingUBCF"]] = ifelse(slice[["likeRatingUBCF"]] <1, 1, slice[["likeRatingUBCF"]])
slice[["likeRatingUBCF"]] = ifelse(slice[["likeRatingUBCF"]] >10, 10, slice[["likeRatingUBCF"]])
slice[["decRatingUBCF"]] = ifelse(slice[["decRatingUBCF"]] >1, 1, slice[["decRatingUBCF"]])
slice[["decRatingUBCF"]] = ifelse(slice[["decRatingUBCF"]] <0 , 0, slice[["decRatingUBCF"]])
arr =  c("attrRatingM", "likeRatingM", "decRatingM")
arr2 = c("attrRatingUCBF", "likeRatingUBCF", "decRatingUBCF", "attrAvgM", "likeAvgM")
n = names(slice)
niceCors(slice, n[grep("attr",n)],"decRatingM")
niceCors(slice, n[grep("like",n)],"decRatingM")
niceCors(slice, n[grep("attr",n)],"decW")
merged["attrGuessAdjM"] = merged["attrRatingGuessM"] - merged["attrRaterAvgW"]
merged["likeGuessAdjM"] = merged["likeRatingGuessM"] - merged["likeRaterAvgW"]
merged["decGuessAdjM"] = merged["decRatingGuessM"] - merged["decRaterAvgW"]
merged["guessesM"] = rowSums(scale(merged[c("attrGuessAdjM", "likeGuessAdjM", "decGuessAdjM")]))
niceCors(merged, c("attrGuessAdjM", "likeGuessAdjM", 'decGuessAdjM', "guessesM"), "decW")
attrs = c("attrRatingM", "attrAvgM", "attrRaterAvgW","attrRatingGuessM", "attrGuessAdjM")
for(i in unique(merged[["wave"]])){
  print(i)
  print(nrow(merged[merged["wave"] ==i,]))
  print(niceCors(merged[merged["wave"] == i,], gsub("attr","attr",attrs),c("attrRatingM", "decW")))
}
merged["attrComboM"] = rowSums(scale(merged[c("attrAvgM", "attrRatingGuessM")]))
merged["likeComboM"] = rowSums(scale(merged[c("likeAvgM", "likeRatingGuessM")]))
merged["decAttrLikeAvgM"] = rowSums(scale(merged[c("decAvgM", "attrAvgM", "likeAvgM")]))
merged["attrLikeRaterAvgW"] = rowSums(scale(merged[c("attrRaterAvgW", "likeRaterAvgW")]))

features = c("decRaterAvgW", "decAttrLikeAvgM", "attrRatingGuessM")
sto = results(merged[merged[["wave"]] %in% c(21,11,9),], features, "attrRatingM", "newColName", "linear", thres=10)
results
slicey = merged[merged[["wave"]] %in% c(21,11,9),]
niceCors(slicey, c("attrRatingM", "attrAvgM", "attrRaterAvgW", "attrComboM", "attrRatingGuessM","attrGuessAdjM",
                   "decRaterAvgW", "decAvgM", "decRatingGuessW"),c("decW"))
ratingMatrix = matrix(slice[["attrRating"]], nrow = 22, ncol = 22)
r <- as(ratingMatrix, "realRatingMatrix")

image(r)
normed = normalize(r)
image(normed)

ratingMatrix = matrix(slice[["attrRatingUBCF"]], nrow = 22, ncol = 22)
r <- as(ratingMatrix, "realRatingMatrix")
image(r)
normed = normalize(r)
image(normed)


addGuessToWave = function(slice, rating, guessName, type="UBCF"){
  slice["temp"] = 0
  raters = unique(slice[["iid"]])
  ratees = unique(slice[["pid"]])
  slice = slice[order(slice[c("pid")]),]
  for(i in 1:nrow(slice)){
    rater = slice[i,"iid"]
    ratee = slice[i,"pid"]
    slice[-i,"temp"] = slice[-i,rating]
    slice[i,"temp"] = NA
    ratingMatrix = matrix(slice[["temp"]], nrow = length(raters), ncol = length(ratees))
    r <- as(ratingMatrix, "realRatingMatrix")
    recommender = Recommender(r, method = type)
    recom <- predict(recommender, r, type="ratings") 
    slice[["temp"]] = c(as(recom, "matrix"))
    print(slice[i,"temp"])
    slice[i,guessName] = slice[i,"temp"]
  }
  return(slice)
}


slice[["menProbs"]]  = getProbs(merged[merged["wave"] != 4,], slice, c("likeRatingGuessM"), "likeRatingM", "linear")

niceCors(slice, c('likeRatingGuessM',"likeAvgM", "likeRaterAvgM"), "decM")
ratingMatrix = matrix(slice[["likeRatingM"]], nrow = 18, ncol = 18)
r <- as(ratingMatrix, "realRatingMatrix")
normed = normalize(r)
image(normed)
recommender = Recommender(r, method = "UBCF")





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
scoreSlice = merged[c("iidM", "iidW", "wave",  "match","decM","decW")]
# set.seed = 1; scoreSlice[["random"]] = sample(1:nrow(scoreSlice))
scoreSlice["orig"] = merged["orderM"]
scoreSlice["basic"] = merged["basicProbMatch"]
scoreSlice["final"] = merged["probMatchFinal"]
scoreSlice = scoreSlice
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
    arr = slice[slice["basicOrderM"] + slice["basicOrderW"] <= 2*i,][["basicOrder"]] 
    slice[slice["basicOrderM"] + slice["basicOrderW"] <= 2*i,][["basicOrder"]] = ifelse(arr == 0, i, arr)
    arr = slice[slice["finalOrderM"] + slice["finalOrderW"] <= 2*i,][["finalOrder"]] 
    slice[slice["finalOrderM"] + slice["finalOrderW"] <= 2*i,][["finalOrder"]] = ifelse(arr == 0, i, arr)
  }
  ss[ss["wave"] == wave,c("basicOrder", "finalOrder")] = slice[,c("basicOrder", "finalOrder")] 
}
hist(ss[["orig"]])
hist(ss[["finalOrder"]])
for(k in 1:3){
  ones = ss[ss["basicOrder"] == c(k),]
  iids = c(unique(ones[["iidM"]]), unique(ones[["iidW"]]))
  for(j in seq(14,2,by= -1)){
    for(i in 2:nrow(ss)){
      if(ss[i, "basicOrder"] > k | ss[i, "basicOrder"] == 0){
        menBase = ss[i,"basicOrderM"]
        womenBase = ss[i,"basicOrderW"]
        iidM = ss[i,"iidM"]
        iidW = ss[i,"iidW"]
        if(menBase + womenBase >= 30 - 2*j & !(iidM %in% iids) & !(iidW %in% iids)){
          ss[i,"basicOrder"] = k
          iids = c(iids, iidW, iidW)
        }  
      }
    }      
  }
}  


table(ss[["basicOrder"]])
# newSS = ss
# newSS[c("basicOrderR", "finalOrderR")] = 0
# for(wave in unique(newSS[["wave"]])){
#   print(wave)
#   tws = newSS[newSS["wave"] == wave,]
#   tws = processWave(tws, "basicOrderM" , "basicOrderW", "basicOrderR")
#   tws = processWave(tws, "finalOrderM" , "finalOrderW", "finalOrderR")
#   newSS[newSS["wave"] ==wave,][["basicOrderR"]] = tws[["basicOrderR"]]
#   newSS[newSS["wave"] ==wave,][["finalOrderR"]] = tws[["finalOrderR"]]
# }
# table(newSS[newSS["finalOrderR"] == 0,][["match"]])
# processWave = function(ws, orderNameM, orderNameW, newColName){
#   ws[newColName] = 0
#   iidMs = unique(ws[["iidM"]])
#   iidWs = unique(ws[["iidW"]])
#   len = 2*min(length(iidMs), length(iidWs))
#   h = hash()
#   newWS = ws
#   for(i in 1:(len/2)){
#     for(k in 1:len){
#       h[[toString(k)]] = ws[ ws[orderNameM] + ws[orderNameW] >= k & ws[orderNameM] + ws[orderNameW] <= k + 1 ,][c("iidW", "iidM", "match")]
#     }
#     rIIDMs = iidMs
#     rIIDWs = iidWs
#     for(key in seq(len, 1, by= -1)){
#       slice = h[[toString(key)]]
#       if(nrow(slice) > 0){
#         for(k in 1:nrow(slice)){
#           iidM = slice[k,"iidM"]
#           iidW = slice[k,"iidW"]
#           if(iidM %in% rIIDMs & iidW %in% rIIDWs){
#             newWS[newWS["iidM"] == iidM & newWS["iidW"] == iidW, newColName] = i
#             rIIDMs = rIIDMs[rIIDMs != iidM]
#             rIIDWs = rIIDWs[rIIDWs != iidW]
#           }
#         }
#       }
#     }
#     ws = newWS[newWS[newColName] == 0,] 
#   }
#   return(newWS)
# }
# ws = ss[ss["wave"] == 12,]
# ns = processWave(ws, "finalOrderM" , "finalOrderW", "finalOrderR")
# table(ns["finalOrderR"])

ss = scoreSlice 


topMetrics = data.frame()
topMetrics[1:20,] = 0
ss["origOrderW"] = ss["orig"]
ss["origOrderM"] = ss["orig"]
probNames = c("orig","basic", "final")
for(probName in probNames){
  for(i in 2:40){
    tops = ss[ss[paste(probName,"OrderW",sep="")] +  ss[paste(probName,"OrderM",sep="")] <= i,]
    topMetrics[i,paste(probName,"NumDates",sep="")] = nrow(tops)
    matches = tops[tops["match"] ==1,]
    topMetrics[i,paste(probName,"NumMatches",sep="")] = nrow(matches)
    topMetrics[i,paste(probName,"AtLeastOne",sep="")] = length(unique(matches[["iidM"]])) + length(unique(matches[["iidW"]]))
  }
  topMetrics[paste(probName,"PercentMatch",sep="")] = topMetrics[paste(probName,"NumMatches",sep="")]/topMetrics[paste(probName,"NumDates",sep="")]
  topMetrics[paste(probName,"FracFound",sep="")] = topMetrics[paste(probName,"NumMatches",sep="")]/nrow(ss[ss["match"] == 1,])
}
n = names(topMetrics)
changes = n[grep("PercentMatch|FracFound",n)]
topMetrics = round(topMetrics,2)
topMetrics[changes] = 100*topMetrics[changes]
plot(topMetrics[["origAtLeastOne"]], type="l", col="blue",xlim=c(2,40), ylim=c(0,300))
lines(topMetrics[["basicAtLeastOne"]], type="l", col="red",xlim=c(2,40), ylim=c(0,300))
lines(topMetrics[["finalAtLeastOne"]], type="l", col="green",xlim=c(2,40), ylim=c(0,300))


topMetrics[n[grep("PercentMatch",n)]]
topMetrics[n[grep("FracFound",n)]]
topMetrics[n[grep("FracFoundW",n)]]
topMetrics[n[grep("MInTopN",n)]]

topMetrics[n[grep("NumDates",n)]]
topMetrics[n[grep("AtLeastOneM",n)]]
topMetrics[n[grep("FracFoundM",n)]]
topMetrics[n[grep("MInTopM",n)]]

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
