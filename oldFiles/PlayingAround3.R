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

df = ddply(df, .(pid) ,transform ,raterCount = length(unique(iid)))
df = ddply(df, .(iid) ,transform ,rateeCount = length(unique(pid)))
df = ddply(df, .(pid) ,transform ,rateeDecNum = sum(dec))
df = ddply(df, .(iid) ,transform ,raterDecNum = sum(dec))

df["raterDecNum"] = ifelse(df["raterDecNum"] == 0 , 0.5, df[["raterDecNum"]])
df["rateeDecNum"] = ifelse(df["rateeDecNum"] == 0 , 0.5, df[["rateeDecNum"]])
df["raterDecAvg"] = (df["raterDecNum"])/(df["rateeCount"])
df["rateeDecAvg"] = (df["rateeDecNum"])/(df["raterCount"])
df["raterDecNumExc"] = df["raterDecNum"] - df["dec"]
df["rateeDecNumExc"] = df["rateeDecNum"] - df["dec"]
df["raterDecAvgExc"] = df["raterDecNumExc"]/(df["rateeCount"] - 1)
df["rateeDecAvgExc"] = df["rateeDecNumExc"]/(df["raterCount"] - 1)
df["raterDecAdj"] = df["dec"] - df["raterDecAvgExc"]

# df["diff"] = ifelse(df["rateeDecAvgExc"] > 1/2, df[["rateeDecAvgExc"]], 1 - df[["rateeDecAvgExc"]])
df["diff"] = 1
df["rateeWeight"] = df["diff"]
df["weightedDec"] = df["raterDecAdj"]*df["rateeWeight"]
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
    decs =  wave[wave["id.x"] == i,][["dec.x"]]
    if(length(unique(decs)) != 1){
      dec_str = paste("decX", toString(i))
      cor_str = paste("corX", toString(i))
      vote_str = paste("RaterX", toString(i))
      wave[dec_str] = wave[wave["id.x"] == i,][["dec.x"]]
      wave[cor_str] = 0
      wave[vote_str] = 0
      for(j in x_ids){
        if(i !=j ){
          for(p in x_partners){
            slice_i = h[[paste(toString(i), toString(p))]]
            slice_i_2 = g[[paste(toString(i), toString(p))]]
            slice_j = h[[paste(toString(j), toString(p))]]
            c = cor(slice_i[["weightedDec.x"]], slice_j[["weightedDec.x"]])
            c = ifelse(is.na(c) | c == 0, 0.01, c)
            wave[wave["id.x"] == j & wave["partner.x"] == p,][[cor_str]] = c
            wave[wave["id.x"] == j & wave["partner.x"] == p,][[vote_str]] = c*ifelse(slice_i_2[["dec.x"]] == 1, 1, -1)
          }
        } 
      }
    }
  }
  wave = wave[names(wave[,colSums(wave) != 0])]
  wave = data.frame(apply(wave,2,f))
  wave = round(wave, 2)
  return(wave)
}
for(i in 1:20){
  print(i)
  wave = merged[merged["wave"] == i,]
  wave = processWave(wave)
  write.csv(wave, paste("~/Desktop/waves/decisions",paste(toString(i),".csv",sep=""),sep=""))    
}

wave = read.csv(paste("~/Desktop/waves/decisions",paste(toString(4),".csv",sep=""),sep=""))
