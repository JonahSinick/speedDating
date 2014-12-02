library(gdata)
library(ggplot2)
library(miscTools)
library(xlsx)
library(hash)
library(aod)
library(randomForest)
library(LiblineaR)
library(Matrix)
library(plyr)
library(recommenderlab)
library(scatterplot3d)
library(Metrics)


merged = read.csv( '~/Desktop/speedDating/mergedProbsAddedFinal.csv')

waves = unique(merged[["wave"]])
merged = merged[merged[["wave"]] %in% c(7, 9, 11, 12, 19, 21, 4),c("wave", "iidM", "iidW", "match", "matchGuess")]
merged["newRound"] = 0
for(wave in c(7)){
  rankingHash = hash()
  waveC = merged[merged["wave"] == wave,]
  iidWs = unique(waveC[["iidW"]])
  iidMs = unique(waveC[["iidM"]])
  numRounds = length(iidMs)
  for(iidM in iidMs){
    slice = waveC[waveC["iidM"] == iidM,]
    slice = slice[order(-slice["matchGuess"]),]
    rankingHash[[toString(iidM)]] = slice
  }
  for(iidW in iidWs){
    slice = waveC[waveC["iidW"] == iidW,]
    slice = slice[order(-slice["matchGuess"]),]
    rankingHash[[toString(iidW)]] = slice
  }
  for(round in 1:numRounds){
    freeMen = iidMs
    freeWomen = iidWs
    for(i in 1:numRounds){
      minProb = 1
      candidate = NA
      for(key in keys(rankingHash)){
        ranked = rankingHash[[key]]
        ranked = ranked[ranked[["iidM"]] %in% freeMen & ranked[["iidW"]] %in% freeWomen,]
        if(nrow(ranked) > 0){
          if(ranked[1,"matchGuess"] < minProb){
            print(ranked[1,"matchGuess"])
            minProb = slice[1,"matchGuess"]
            candidate = slice[1,]         
          }
        }
      }
      iidM = candidate[1,"iidM"]
      iidW = candidate[1,"iidW"]
      merged[merged["iidM"] == iidM & merged["iidW"] == iidW,"newRound"] = i
      freeWomen = freeWomen[freeWomen != iidW]
      freeMen = freeMen[freeMen != iidM]
      rankingHash[[toString(iidM)]] = rankingHash[[toString(iidM)]][rankingHash[[toString(iidM)]]["iidW"] != iidW,]
      rankingHash[[toString(iidW)]] = rankingHash[[toString(iidW)]][rankingHash[[toString(iidW)]]["iidM"] != iidM,]
    }
  }
}
minimumProb = 100
nextCandidate = NA
for(key in keys(rankingHash)){
  topProb = 
  if(rankingHash[[key]])
}

orderingM = data.frame()
orderingW = data.frame()
matchesM = data.frame()
matchesW = data.frame()
#   for(iidM in iidMs){
#     orderingM[toString(iidM),] = 0
#     matchesM[toString(iidM),] = 0
#   }
#   for(iidW in iidWs){
#     orderingW[toString(iidW),] = 0
#     matchesW[toString(iidW),] = 0
#   }
#   m = max(length(iidWs), length(iidMs))
#   for(i in 1:m){
#     orderingM[paste("round",toString(i),sep="_")] = 0
#     matchesM[paste("round",toString(i),sep="_")] = 0
#     orderingW[paste("round",toString(i),sep="_")] = 0
#     matchesW[paste("round",toString(i),sep="_")] = 0
#   }
#   waveC = waveC[order(waveC["matchGuess"]),]
#   for(idx in 1:m){
#     remainingWomen = iidWs
#     remainingMen = iidMs
#     for(i in 1:nrow(waveC)){
#       iidM = waveC[i,"iidM"]
#       iidW = waveC[i,"iidW"]
#       pair = paste(toString(iidM), toString(iidW),sep= "_")
#       if(iidM %in% remainingMen & iidW %in% remainingWomen & !(pair %in% values(idContainerHash))){
#         orderingM[toString(iidM),idx] = iidW
#         orderingW[toString(iidW),idx] = iidM
#         matchesM[toString(iidM),idx] = waveC[i,"match"]
#         matchesW[toString(iidW),idx] = waveC[i,"match"]
#         remainingMen = remainingMen[remainingMen != iidM]
#         remainingWomen = remainingWomen[remainingWomen != iidW]
#         idContainerHash[[pair]] = pair
#       }
#     }
#   }
#   waveOrderingHash[[paste("orderingM_Wave",toString(wave),sep="_")]] = orderingM
#   waveOrderingHash[[paste("orderingW_Wave",toString(wave),sep="_")]] = orderingW
#   waveOrderingHash[[paste("matchesM_Wave",toString(wave),sep="_")]] = matchesM
#   waveOrderingHash[[paste("matchesW_Wave",toString(wave),sep="_")]] = matchesW
}
matchesMWaves = keys(waveOrderingHash)[grep("matchesM_Wave",keys(waveOrderingHash))]
numMatches = sum(merged[["match"]])
h = hash()
h[["1"]] = 0
h[["2"]] = 0
h[["3"]] = 0
h[["4"]] = 0
s = hash()
s[["1"]] = 0
s[["2"]] = 0
s[["3"]] = 0
s[["4"]] = 0
k = hash()
t = hash()


k[["1"]] = 0
k[["2"]] = 0
k[["3"]] = 0
k[["4"]] = 0


t[["1"]] = 0
t[["2"]] = 0
t[["3"]] = 0
t[["4"]] = 0
df = df[df[["wave"]] %in% c(7, 9, 11, 12, 19, 21, 4), ]
for(wave in unique(df[["wave"]])){
  slice = df[df["wave"] == wave,]
  slice = slice[order(slice["round"]),]
  men = slice[slice["gender"] == 1,]
  baselineMatrix = matrix(slice[["match"]], nrow = length(unique(men[["iid"]])), ncol = length(unique(men[["iid"]])))
  matchMatrix = as.matrix(waveOrderingHash[[paste("matchesM_Wave",toString(wave),sep="_")]])
  h[["1"]] = h[["1"]] +  table(matchMatrix[,1:floor(1*ncol(matchMatrix)/4)])[[2]]
  h[["2"]] = h[["2"]] +  table(matchMatrix[,1:floor(2*ncol(matchMatrix)/4)])[[2]]
  h[["3"]] = h[["3"]] +  table(matchMatrix[,1:floor(3*ncol(matchMatrix)/4)])[[2]]
  h[["4"]] = h[["4"]] +  table(matchMatrix[,1:floor(4*ncol(matchMatrix)/4)])[[2]]
  
  s[["1"]] =  s[["1"]] + table(rowSums(matchMatrix[,1:floor(1*ncol(matchMatrix)/4)]) > 0)[[2]]
  s[["2"]] =  s[["2"]] + table(rowSums(matchMatrix[,1:floor(2*ncol(matchMatrix)/4)]) > 0)[[2]]
  s[["3"]] =  s[["3"]] + table(rowSums(matchMatrix[,1:floor(3*ncol(matchMatrix)/4)]) > 0)[[2]]
  s[["4"]] =  s[["4"]] + table(rowSums(matchMatrix[,1:floor(4*ncol(matchMatrix)/4)]) > 0)[[2]]
  
  k[["1"]] = k[["1"]] +  table(baselineMatrix[,1:floor(1*ncol(baselineMatrix)/4)])[[2]]
  k[["2"]] = k[["2"]] +  table(baselineMatrix[,1:floor(2*ncol(baselineMatrix)/4)])[[2]]
  k[["3"]] = k[["3"]] +  table(baselineMatrix[,1:floor(3*ncol(baselineMatrix)/4)])[[2]]
  k[["4"]] = k[["4"]] +  table(baselineMatrix[,1:floor(4*ncol(baselineMatrix)/4)])[[2]]
  
  t[["1"]] =  t[["1"]] + table(rowSums(baselineMatrix[,1:floor(1*ncol(baselineMatrix)/6)]) > 0)[[2]]
  t[["2"]] =  t[["2"]] + table(rowSums(baselineMatrix[,1:floor(2*ncol(baselineMatrix)/4)]) > 0)[[2]]
  t[["3"]] =  t[["3"]] + table(rowSums(baselineMatrix[,1:floor(3*ncol(baselineMatrix)/4)]) > 0)[[2]]
  t[["4"]] =  t[["4"]] + table(rowSums(baselineMatrix[,1:floor(4*ncol(baselineMatrix)/4)]) > 0)[[2]]
}
table(as.matrix(matchesM)[,1:18])

#NEW STUFF

for(wave in waves){
  waveC = waveHash[[toString(wave)]]
  waveC = wave[c("iidM", "iidW", "match", "decM", "decW", "matchGuess", "matchGuessRec")]
}





