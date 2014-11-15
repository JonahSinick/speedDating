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
df = read.csv('~/Desktop/speedDating/SpeedDatingData.csv')
unique(df[["wave"]])
wave = df[df["gender"] == 1 & df["wave"] == 14,]


wave = ddply(wave, .(id) ,transform ,raterCount = length(unique(partner)))
wave = ddply(wave, .(partner) ,transform ,rateeCount = length(unique(id)))
wave = ddply(wave, .(partner) ,transform ,rateeDecNum = sum(dec))
wave = ddply(wave, .(id) ,transform ,raterDecNum = sum(dec))

wave["raterDecAvgExc"] = (wave["raterDecNum"] - wave["dec"])/(wave["rateeCount"] - 1)
wave["rateeDecAvgExc"] = (wave["rateeDecNum"] - wave["dec"])/(wave["raterCount"] - 1)

wave["attrLikeFunSum"] = wave["attr"] + wave["like"] + wave["fun"]
wave["allSum"] = wave["attr"] + wave["like"] + wave["fun"] + wave["amb"] + wave["shar"] + wave["sinc"] + wave["intel"] + wave["prob"]

attributes = c("attr", "sinc", "intel", "amb", "like", "shar", "fun", "prob", "attrLikeFunSum", "allSum")
wave = wave[c("dec", attributes, "id", "partner")]

for(s in attributes){
  m = median(wave[[s]], na.rm = TRUE)
  wave[s] = ifelse(is.na(wave[[s]]), m, wave[[s]])
}

for(s in attributes){
  matrix = t(matrix(wave[[s]], nrow = 20, ncol = 18))
  
  dimnames(matrix) <- list(rownames(matrix, do.NULL = FALSE, prefix = "rater"),
                           colnames(matrix, do.NULL = FALSE, prefix = "ratee"))
  
  empty_matrix = matrix(nrow = 18, ncol = 20)
  for(i in 1:18){
    for(j in 1:20){
      print(c(s,i,j))
      new_matrix = matrix
      new_matrix[i,j] = NA
      r <- as(new_matrix, "realRatingMatrix")
      recommender = Recommender(r, method = "UBCF")
      recom <- predict(recommender, r, type="ratings")
      empty_matrix[i, j] = as(recom, "matrix")[i,j]
    }
  }
  fixed0 = matrix(t(matrix), nrow=360, ncol = 1)[,1]
  fixed1 = matrix(t(empty_matrix), nrow = 360, ncol = 1)[,1]
  wave[[paste(s,"Pred",sep="")]] = fixed1
}

for(str in c(attributes)){
  print(str)
  predicted = paste(str,"Pred", sep="")
  pred_mean = paste(str,"PredMean", sep="")
  actual_mean = paste(str,"Mean", sep="")
  pred_adj = paste(str,"PredAdj", sep="")
  actual_adj =  paste(str,"Adj", sep="")
  wave[pred_mean] = 0
  wave[actual_mean] = 0
  wave[pred_adj] = 0
  wave[actual_adj] = 0
  for(i in unique(wave[["id"]])){
    wave[wave["id"] == i,][[pred_mean]] = sum(wave[wave["id"] == i,][[predicted]])/length(unique(wave[["partner"]]))
    wave[wave["id"] == i,][[actual_mean]] = sum(wave[wave["id"] == i,][[str]])/length(unique(wave[["partner"]]))
  }
  wave[pred_adj] =  wave[predicted] - wave[pred_mean]
  wave[actual_adj] =  wave[str] - wave[actual_mean]
}
wave = round(wave, 2)
round(cor(wave, wave["dec"]),2)
n = names(wave)
adjusted = n[grep("Adj", n)]
predicted_adjusted = adjusted[c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19)]
actual_adjusted = adjusted[c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20)]

f = function(df, features){
  df = df[sample(nrow(df)),]

  tar = c("dec")
  wave_train = df[seq(1, nrow(df)/2),] 
  wave_test = df[seq(nrow(df)/2 + 1, nrow(df)),]
  target = factor(wave_train[,tar])
  wave_train = wave_train[c(tar, features)]
  wave_test = wave_test[c(tar, features)]  
  s=scale(wave_train[features],center=TRUE,scale=TRUE)
  co=heuristicC(s)
  m=LiblineaR(data=s,labels=target,type=0,cost=co,bias=TRUE,verbose=FALSE)
  s2= scale(wave_test[features],attr(s,"scaled:center"),attr(s,"scaled:scale"))
  p=predict(m,s2)
  t = table(p$predictions,wave_test[[tar]])
  print(t)
  print((t[1,1] + t[2, 2])/nrow(wave_test))
}
features =  c("raterDecAvgExc", "rateeDecAvgExc", predicted_adjusted)

f(wave, features)


df = ddply(df, .(pid) ,transform ,rateeDecSum = sum(dec))
df = ddply(df, .(pid) ,transform ,rateeAttrSum = sum(attr))
df = ddply(df, .(pid) ,transform ,rateeSincSum = sum(sinc))
df = ddply(df, .(pid) ,transform ,rateeIntelSum = sum(intel))
df = ddply(df, .(pid) ,transform ,rateeFunSum = sum(fun))
df = ddply(df, .(pid) ,transform ,rateeAmbSum = sum(amb))
df = ddply(df, .(pid) ,transform ,rateeSharSum = sum(shar))
df = ddply(df, .(pid) ,transform ,rateeLikeSum = sum(like))
df = ddply(df, .(pid) ,transform ,rateeProbSum = sum(prob)) 




df = ddply(df, .(iid) ,transform ,raterDecSum = sum(dec))
df = ddply(df, .(pid) ,transform ,decSum = sum(dec))
df = ddply(df, .(pid) ,transform ,attrSum = sum(attr))
df = ddply(df, .(pid) ,transform ,sincSum = sum(sinc))
df = ddply(df, .(pid) ,transform ,intelSum = sum(intel))
df = ddply(df, .(pid) ,transform ,funSum = sum(fun))
df = ddply(df, .(pid) ,transform ,ambSum = sum(amb))
df = ddply(df, .(pid) ,transform ,sharSum = sum(shar))
df = ddply(df, .(pid) ,transform ,likeSum = sum(like))
df = ddply(df, .(pid) ,transform ,probSum = sum(prob)) 