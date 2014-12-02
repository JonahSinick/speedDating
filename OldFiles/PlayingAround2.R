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

df <- read.csv('~/Desktop/speedDatingData.csv')
f=function(x){
  x<-as.numeric(as.character(x)) #first convert each column into numeric if it is from factor
  x[is.na(x)] =median(x, na.rm=TRUE) #convert the item with NA to median value from the column
  x #display the column
}
keeps = c("wave", "gender", "iid", "pid", "id", "partner",  "dec", "match")
df = df[keeps]
keeps_to_fix = c("dec", "match")

df_rm_na = df[keeps_to_fix]
df_rm_na = data.frame(apply(df_rm_na,2,f))
df[keeps_to_fix] = df_rm_na[keeps_to_fix]
df = na.omit(df)

df["dec"] = ifelse(df["dec"] == 1, 1, -1)

df = ddply(df, .(pid) ,transform ,raterCount = length(unique(iid)))
df = ddply(df, .(iid) ,transform ,rateeCount = length(unique(pid)))
df = ddply(df, .(pid) ,transform ,rateeDecNum = sum(dec))
df = ddply(df, .(iid) ,transform ,raterDecNum = sum(dec))

df["raterDecAvg"] = (df["raterDecNum"])/(df["rateeCount"])
df["rateeDecAvg"] = (df["rateeDecNum"])/(df["raterCount"])

df["raterDecAvgExc"] = (df["raterDecNum"] - df["dec"])/(df["rateeCount"] - 1)
df["rateeDecAvgExc"] = (df["rateeDecNum"] - df["dec"])/(df["raterCount"] - 1)

df["raterDecAdj"] = df["dec"] - df["raterDecAvgExc"] + 0.01

df["rateeWeight"] = log(df["raterCount"]/(df["raterCount"] + abs(df["rateeDecNum"])))
df["weightedDec"] = df["dec"]*df["raterDecAdj"]
men = df[df["gender"] == 1,]
women = df[df["gender"] == 0,]
own_id_info = c("iid", "pid", "match", "wave")
partner_id_info = c("pid", "iid", "match", "wave")

merged = merge(women, men, by.x = own_id_info, by.y = partner_id_info)


processWave = function(wave){
  ratee_count = wave["rateeCount.x"]
  x_ids = unique(wave[["id.x"]])
  x_partners = unique(wave[["partner.x"]])
  h = hash()
  g = hash()
  for(i in x_ids){
    for(p in x_partners){
      h[[paste(toString(i), toString(p))]] =  wave[wave["id.x"] == i & wave["partner.x"] != p,]
      g[[paste(toString(i), toString(p))]] =  wave[wave["id.x"] == i & wave["partner.x"] == p,]
    }
  }
  for(i in x_ids){
    dec_str = paste("decX", toString(i))
    cor_str = paste("corX", toString(i))
    vote_str = paste("RaterX", toString(i))
    wave[dec_str] = wave[wave["id.x"] == i,][["raterDecAdj.x"]]
    wave[cor_str] = 0
    wave[vote_str] = 0
    for(p in x_partners){
      slice_i = h[[paste(toString(i), toString(p))]]
      slice_i_2 = g[[paste(toString(i), toString(p))]]
      for(j in x_ids){
        if(i !=j ){
          slice_j = h[[paste(toString(j), toString(p))]]       
          c = cor(slice_i[["weightedDec"]], slice_j[["weightedDec"]])
          c = ifelse(is.na(c) | c == 0, 0.01, c)
          wave[wave["id.x"] == j & wave["partner.x"] == p,][[cor_str]] = c
          wave[wave["id.x"] == j & wave["partner.x"] == p,][[vote_str]] = c*slice_i_2[["raterDecAdj.x"]] 
        }
      }
    }
  }
  wave = round(wave, 3)
  return(wave)
}
for(i in 1:2){
  wave = merged[merged["wave"] == i,]
  wave = processWave(wave)
  write.csv(wave, paste("~/Desktop/waves/decisions",paste(toString(i),".csv",sep=""),sep=""))    
}

wave = read.csv(paste("~/Desktop/waves/decisions",paste(toString(i),".csv",sep=""),sep=""))
