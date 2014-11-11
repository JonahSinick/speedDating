


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
keeps = c("wave", "gender", "iid", "pid", "id", "partner",  "dec", "attr", "sinc", "intel",  "fun", "amb", "shar", "like","prob", "match")
df = df[keeps]
keeps_to_fix = c("dec", "attr", "sinc", "intel",  "fun", "amb", "shar", "like","prob", "match")

df_rm_na = df[keeps_to_fix]
df_rm_na = data.frame(apply(df_rm_na,2,f))
df[keeps_to_fix] = df_rm_na[keeps_to_fix]
df = na.omit(df)

men = df[df["gender"] == 1,]
women = df[df["gender"] == 0,]
own_id_info = c("iid", "pid")
partner_id_info = c("pid", "iid")
# for(i in 1:21){
#   wave = merged[merged["wave.x"] == i,]
#   write.csv(wave, paste("~/Desktop/waves/wave",paste(toString(i),".csv",sep=""),sep=""))
# }

meansAdded = function(wave){
  wave = ddply(wave, .(id) ,transform ,decMean = mean(dec))
  wave = ddply(wave, .(id) ,transform ,attrMean = mean(attr))
  wave = ddply(wave, .(id) ,transform ,sincMean = mean(sinc))
  wave = ddply(wave, .(id) ,transform ,intelMean = mean(intel))
  wave = ddply(wave, .(id) ,transform ,funMean = mean(fun))
  wave = ddply(wave, .(id) ,transform ,ambMean = mean(amb))
  wave = ddply(wave, .(id) ,transform ,sharMean = mean(shar))
  wave = ddply(wave, .(id) ,transform ,likeMean = mean(like))
  wave = ddply(wave, .(id) ,transform ,probMean = mean(prob))
  
  wave = ddply(wave, .(partner) ,transform ,decPartnerMean = mean(dec))
  wave = ddply(wave, .(partner) ,transform ,attrPartnerMean = mean(attr))
  wave = ddply(wave, .(partner) ,transform ,sincPartnerMean = mean(sinc))
  wave = ddply(wave, .(partner) ,transform ,intelPartnerMean = mean(intel))
  wave = ddply(wave, .(partner) ,transform ,funPartnerMean = mean(fun))
  wave = ddply(wave, .(partner) ,transform ,ambPartnerMean = mean(amb))
  wave = ddply(wave, .(partner) ,transform ,sharPartnerMean = mean(shar))
  wave = ddply(wave, .(partner) ,transform ,likePartnerMean = mean(like))
  wave = ddply(wave, .(partner) ,transform ,probPartnerMean = mean(prob))
  for(rat in c("dec", "like", "attr", "fun", "prob", "sinc", "amb")){    
    str1 = paste(rat,"Mean",sep="")
    str2 = paste(rat,"PartnerMean",sep="")
    wave[paste(str1,"Exc",sep="")] = (len*wave[str1] - wave[rat])/(len - 1)
    wave[paste(str2,"Exc",sep="")] = (len*wave[str2] - wave[rat])/(len - 1)
  }
  return(wave)
}


addRaters = function(wave){
  ids = unique(wave[["id"]])
  len = length(ids)
  for(rat in c("dec", "like", "attr", "fun", "prob", "sinc", "amb")){    
    str1 = paste(rat,"Mean",sep="")
    str2 = paste(rat,"PartnerMean",sep="")
    wave[paste(str1,"Exc",sep="")] = (len*wave[str1] - wave[rat])/(len - 1)
    wave[paste(str2,"Exc",sep="")] = (len*wave[str2] - wave[rat])/(len - 1)
#     for(i in ids){
#       slice_i = wave[wave["id"] == i,][[rat]]
#       wave[wave["id"] == i,][str2] = mean(slice_i)
#       all_zero = TRUE
#       for (k in 1:length(slice_i)){
#         if(slice_i[k] != 0){
#           all_zero = FALSE
#         }
#       }
#       if(!all_zero){
#         str = paste(rat,paste("RaterCor",toString(i),sep="_"),sep="_")
#         str2 = paste(rat,paste("RaterSquareCor",toString(i),sep="_"),sep="_")
#         wave[str] = 0
#         wave[str2] = 0
#         for(j in ids){
#           if(i != j){
#             slice_j = wave[wave["id"] == j,][[rat]]
#             c = cor(slice_i, slice_j)
#             c = ifelse(is.na(c), 0, c)
#             num = ifelse(c >= 0, c*c, -c*c)
#             wave[wave["id"] == j,][[str]] = c*slice_i
#             wave[wave["id"] == j,][[str2]] = (c*c)*slice_i
#           }
#         }
#       }
#     }
  }
#   wave = wave[,colSums(wave^2) !=0]
#   num_cols = ncol(wave)
#   for(x in names(wave)[18:num_cols]){
#     wave[x] = wave[x] - mean(wave[[x]])
#   }
  return(wave)
}

addDeciders = function(wave){
  ids = unique(wave[["id"]])
  len = length(ids)
  for(i in ids){
    slice_i = wave[wave["id"] == i,][["dec"]]
    str = paste("dec",paste("Best",toString(i),sep="_"),sep="_")
    wave[str] = 0
    for(j in ids){
      if(i != j){
        slice_j = wave[wave["id"] == j,][["dec"]]
        comparisons = ifelse(slice_i == slice_j, 1, 0)
        fracs = -x(comparisons - sum(comparisons))/(length(comparisons) - 1)
        print(c(i,j))
        wave[wave["id"] == j,][[str]] = ifelse(slice_i == 1, fracs, 1 - fracs)
      }
    }
  }
  return(wave)
}

oddsRatio = function(p){
  return(p/(1 - p))
}

inverseOddsRatio = function(or){
  return(or/(1 + or))
}

addProbs = function(wave){
  ids = unique(wave[["id"]])
  n = names(wave)
  wave["decProbs"] = 1
  probs = n[grep("dec_Best.", n)]
  for(i in ids){
    slice = wave[wave["id"] == i,]
    for(p in probs){
        wave[wave["id"] == i,][["decProbs"]] = slice[["decProbs"]]*oddsRatio(slice[[p]])
    }    
  }
  wave["decProbs"] = inverseOddsRatio(wave["decProbs"])^(1/(length(probs) - 1))
  return(wave)
}

addProbSum = function(wave){
  ids = unique(wave[["id"]])
  n = names(wave)
  wave["probSum"] = 0
  probs = n[grep("dec_Best.", n)]
  for(p in probs){
    wave["probSum"] = wave["probSum"] + wave[p]
  }    
  return(wave)
}


addCols = function(wave){
  ids = unique(wave[["id"]])
  len = length(ids)
  wave = meansAdded(wave)
  wave = addRaters(wave)
  wave = addDeciders(wave)
  wave = addProbs(wave)
  return(wave)
}


f_new=function(x){
  x<-as.numeric(as.character(x)) #first convert each column into numeric if it is from factor
  x[is.na(x)] =mean(x, na.rm=TRUE) #convert the item with NA to median value from the column
  x #display the column
}

for(i in 1:20){
  read.csv(paste("~/Desktop/waves/wave",paste(toString(i),".csv",sep=""),sep=""))  
  
#   wave = wave[wave["gender"] == 0,]
#   wave = meansAdded(wave)
#   wave = addProbSum(wave)
  wave = data.frame(apply(wave,2,f_new))
  wave = addRaters(wave)
  write.csv(wave, paste("~/Desktop/waves/wave",paste(toString(i),".csv",sep=""),sep=""))  
}


for(i in 1:20){
  read.csv(paste("~/Desktop/waves/waveM",paste(toString(i),".csv",sep=""),sep=""))  
  wave = addRaters(wave)
  #   wave = wave[wave["gender"] == 0,]
#   wave = meansAdded(wave)
#   wave = men[men["wave"] == i,]
#   wave = addCols(wave)
  wave = data.frame(apply(wave,2,f_new))
  write.csv(wave, paste("~/Desktop/waves/waveM",paste(toString(i),".csv",sep=""),sep=""))  
}

# # for(i in unique(wave["id.x"])){
# #   wave = merged[merged["wave.x"] == i,]
# #   wave = addCols(wave)
# #   write.csv(wave, paste("~/Desktop/waves/wave",paste(toString(i),".csv",sep=""),sep=""))  
# # }
# 
# 
# 
# # wave = read.csv(paste("~/Desktop/waves/wave",paste(toString(i),".csv",sep=""),sep=""))
