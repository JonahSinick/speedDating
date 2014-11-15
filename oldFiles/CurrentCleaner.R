
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
  partnersExcluded = hash()
  for(i in x_ids){
    for(p in x_partners){
      partnersExcluded[[paste(toString(i), toString(p))]] =  wave[wave["id.x"] == i & wave["partner.x"] != p,]
    }
  }
  for(i in x_ids){
    decs =  wave[wave["id.x"] == i,][["dec.x"]]
    if(length(unique(decs)) != 1){
      wave = createOne(wave, i, partnersExcluded)
    }
  }
  n = names(wave)
  votes = n[grep(c("^RaterX ."), n)]    
  wave["voteSum"] = 0
  for(v in votes){
    wave["voteSum"] = wave[v] + wave["voteSum"]
  }
  wave["prediction"] = ifelse(wave[["voteSum"]] < 0, 0, 1)
  return(wave)
}
createOne = function(wave, i, h){
  x_ids = unique(wave[["id.x"]])
  x_partners = unique(wave[["partner.x"]])
  dec_str = paste("decX", toString(i))
  cor_str = paste("corX", toString(i))
  vote_str = paste("RaterX", toString(i))
  prob_str = paste("probX", toString(i))
  wave[dec_str] = wave[wave["id.x"] == i,][["dec.x"]]
  wave[cor_str] = 0
  wave[vote_str] = 0
  wave[prob_str] = 0
  for(j in x_ids){
    if(i !=j ){
      for(p in x_partners){
        slice_i = h[[paste(toString(i), toString(p))]]
        slice_j = h[[paste(toString(j), toString(p))]]
        c = cor(slice_i[["raterDecAdj.x"]], slice_j[["raterDecAdj.x"]])
        c = ifelse(is.na(c) | c == 0, 0.01, c)
        ind = wave[wave["id.x"] == i & wave["partner.x"] == p,][["dec.x"]]
        wave[wave["id.x"] == j & wave["partner.x"] == p,][[cor_str]] = c
        wave[wave["id.x"] == j & wave["partner.x"] == p,][[vote_str]] = c*ind
        both_yes = sum(slice_i[["dec.x"]]*slice_j[["dec.x"]])
        both_no = sum((1 - slice_i[["dec.x"]]) *( 1 - slice_j[["dec.x"]]))
        agree_frac = ((both_no + both_yes)/(length(x_ids) - 1))
        wave[wave["id.x"] == j & wave["partner.x"] == p,][[prob_str]] = ifelse(ind == 1, agree_frac, 1 - agree_frac)
      } 
    }
    if(i == j){
      wave[wave["id.x"] == j,][[prob_str]] = wave[wave["id.x"] == j,][["raterDecAvgExc"]]
    }
    print(prob_str)      
    
  }
  print(names(wave))
  return(wave)
}

for(i in 1:20){
  print(i)
  wave = merged[merged["wave"] == i,]
  wave = processWave(wave)
  wave = round(wave, 3)
  write.csv(wave, paste("~/Desktop/waves/decisions",paste(toString(i),".csv",sep=""),sep=""))    
}