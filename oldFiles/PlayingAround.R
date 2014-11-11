# library(gdata)
# library(ggplot2)
# library(miscTools)
# library(xlsx)
# library(hash)
# library(aod)
# library(randomForest)
# library(LiblineaR)
# library(Matrix)
# library(plyr)
# library(recommenderlab)
# library(scatterplot3d)
# 
# df <- read.csv('~/Desktop/speedDatingData.csv')
# f=function(x){
#   x<-as.numeric(as.character(x)) #first convert each column into numeric if it is from factor
#   x[is.na(x)] =median(x, na.rm=TRUE) #convert the item with NA to median value from the column
#   x #display the column
# }
# keeps = c("wave", "gender", "iid", "pid", "id", "partner",  "dec", "match")
# df = df[keeps]
# keeps_to_fix = c("dec", "match")
# 
# df_rm_na = df[keeps_to_fix]
# df_rm_na = data.frame(apply(df_rm_na,2,f))
# df[keeps_to_fix] = df_rm_na[keeps_to_fix]
# df = na.omit(df)
# df = ddply(df, .(pid) ,transform ,raterCount = length(unique(iid)))
# df = ddply(df, .(iid) ,transform ,rateeCount = length(unique(pid)))
# df = ddply(df, .(pid) ,transform ,rateeYesCount = sum(dec))
# df = ddply(df, .(iid) ,transform ,raterYesCount = sum(dec))
# df["rateeYesCount"] = ifelse(df["rateeYesCount"] == 0, 0.5, df[["rateeYesCount"]])
# df["rateeYesCount"] = ifelse(df["rateeYesCount"] == df["raterCount"], df[["raterCount"]] - 0.5, df[["rateeYesCount"]])
# df["rateeYesWeight"] = log(df["raterCount"]/df["rateeYesCount"])
# df["rateeNoWeight"] = log(df["raterCount"]/(df["raterCount"] - df["rateeYesCount"]))
# df["dec"] = ifelse(df[["dec"]] == 1, 1, -1)
# df["decItemAdj"] = ifelse(df[["dec"]] == 1, df[["dec"]]*df[["rateeYesWeight"]],  df[["dec"]]*df[["rateeNoWeight"]])
# df = ddply(df, .(iid) ,transform ,raterDecSum = sum(decItemAdj))
# df["raterDecSumExc"] = df["raterDecSum"] - df["decItemAdj"]
# df["raterDecAvg"] = df["raterDecSumExc"]/(df["rateeCount"] - 1)
# df["decUserAdj"] = df["decItemAdj"] - df["raterDecAvg"]
# 
# men = df[df["gender"] == 1,]
# women = df[df["gender"] == 0,]
# own_id_info = c("iid", "pid", "match", "wave")
# partner_id_info = c("pid", "iid", "match", "wave")
# 
# merged = merge(women, men, by.x = own_id_info, by.y = partner_id_info)
# 

processWave = function(wave){
  wave = merged[merged["wave"] == 1,]
  x_ids = unique(wave[["id.x"]])
  x_partners = unique(wave[["partner.x"]])
  h = hash()
  for(i in x_ids){
    for(p in x_partners){
      h[[paste(toString(i), toString(p))]] =  wave[wave["id.x"] == i & wave["partner.x"] != p,]
    }
  }
  for(i in x_ids){
    vote_str = paste("RaterX Vote", toString(i))
    wave[vote_str] = 0
    for(p in x_partners){
      slice_i = h[[paste(toString(i), toString(p))]]
      for(j in x_ids){
        if(i !=j ){
          print(c(i,j))
          slice_j = h[[paste(toString(j), toString(p))]]
          c = cor(slice_i[["decUserAdj.x"]], slice_j[["decUserAdj.x"]])
          wave[wave["id.x"] == j & wave["partner.x"] == p,][[vote_str]] = c*slice_i[["dec.x"]]  
        }
      }
    }
  }
  return(wave)
}
w = processWave(3)
# for(i in 1:20){
#   print(i)
#   wave = merged[merged["wave"] == i,]
#   wave = processWave(wave)
#   write.csv(wave, paste("~/Desktop/waves/decisions",paste(toString(i),".csv",sep=""),sep=""))    
# }