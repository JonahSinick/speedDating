


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
keeps = c("wave", "gender", "iid", "pid", "id", "partner",  "dec", "dec_o", "match")
df = df[keeps]
keeps_to_fix = c("dec", "dec_o", "match")

df_rm_na = df[keeps_to_fix]
df_rm_na = data.frame(apply(df_rm_na,2,f))
df[keeps_to_fix] = df_rm_na[keeps_to_fix]
df = na.omit(df)

men = df[df["gender"] == 1,]
women = df[df["gender"] == 0,]
own_id_info = c("iid", "pid")
partner_id_info = c("pid", "iid")

addMeans = function(wave){
  wave = ddply(wave, .(id) ,transform ,decSum = sum(dec))
  wave = ddply(wave, .(id) ,transform ,dec_oSum = sum(dec_o))
  wave = ddply(wave, .(id) ,transform ,matchSum = match(dec_o))
  wave = ddply(wave, .(partner) ,transform ,decPartnerSum = sum(dec))
  wave = ddply(wave, .(partner) ,transform ,dec_oPartnerSum = sum(dec_o))
  wave = ddply(wave, .(partner) ,transform ,matchPartnerSum = sum(match))
  len = unique(wave[["id"]])
  for(rat in c("dec", "dec_o", "match")){    
    str1 = paste(rat,"Sum",sep="")
    str2 = paste(rat,"SumExc",sep="")
    str3 = paste(rat,"PartnerSum",sep="")
    str4 = paste(rat,"PartnerSumExc",sep="")
    wave[str2] = wave[str1] - wave[rat]
    wave[str4] = wave[str3] - wave[rat]
    str5 = paste(rat,"MeanExc",sep="")
    str6 = paste(rat,"PartnerMeanExc",sep="")
    wave[str5] = wave[str2]/(len - 1)
    wave[str6] = wave[str4]/(len - 1)
  }
  return(wave)
}
str_mean = paste(rat,"MeanExc",sep="")
str_partner_mean = paste(rat,"PartnerMeanExc",sep="")

addRaters = function(wave){
  ids = unique(wave[["id"]])
  partners = unique(wave[["partner"]])
  len = length(ids)
  h = hash()
  targets = c("dec", "dec_o", "match")
  features = c("dec", "dec_o", "match")
  for(t in targets){
    for(i in ids){
      wave[gsub("$",paste(paste(t,"Rater",sep="_"),toString(i),sep="_"),rats)] = 0
      wave[gsub("$",paste(paste(t,"RaterCor",sep="_"),toString(i),sep="_"),rats)] = 0
      wave[gsub("$",paste(paste(t,"RaterCorGuessProb",sep="_"),toString(i),sep="_"),rats)] = 0
      wave[gsub("$",paste(paste(t,"RaterCorSqGuessProb",sep="_"),toString(i),sep="_"),rats)] = 0
      wave[gsub("$",paste(paste(t,"CommonYes",sep="_"),toString(i),sep="_"),rats)] = 0
      wave[gsub("$",paste(paste(t,"CommonNo",sep="_"),toString(i),sep="_"),rats)] = 0
      wave[gsub("$",paste(paste(t,"Common",sep="_"),toString(i),sep="_"),rats)] = 0
      wave[gsub("$",paste(paste(t,"CommonYesGuessProb",sep="_"),toString(i),sep="_"),rats)] = 0
      wave[gsub("$",paste(paste(t,"CommonNoGuessProb",sep="_"),toString(i),sep="_"),rats)] = 0
      wave[gsub("$",paste(paste(t,"CommonGuessProb",sep="_"),toString(i),sep="_"),rats)] = 0
      wave[gsub("$",paste(paste(t,"CommonYesGuessBin",sep="_"),toString(i),sep="_"),rats)] = 0
      wave[gsub("$",paste(paste(t,"CommonNoGuessBin",sep="_"),toString(i),sep="_"),rats)] = 0
      wave[gsub("$",paste(paste(t,"CommonGuessBin",sep="_"),toString(i),sep="_"),rats)] = 0
    }
    
  }
  for(i in ids){
    h[[paste(t,toString(i))]] = wave[wave["id"] == i,][targets]
    wave[gsub("$",paste("Rater",toString(i),sep=""),targets)] = rep(c(h[[i]]), len)
  }
  for(i in ids){
    for(p in partners){
      h[[paste(toString(i), toString(p))]] = h[[i]][h[[i]]["partner"] != "p",][rats]
    }
  }
  for(i in ids){
    for(j in ids){
      for(k in partners){
        for(rat in rats){
          wave[gsub("$",paste("RaterCor",toString(i),sep=""),c(rat))] = cor(c(h[[paste(toString(i), toString(p))]]), c(h[[paste(toString(j), toString(p)))]])          
        }
      }
    }
  }
  for(i in ids){
    cor = wave[gsub("$",paste("RaterCor",toString(i),sep=""),rats)]
    rater = wave[gsub("$",paste("Rater",toString(i),sep=""),rats)]
    wave[gsub("$",paste("RaterCorGuess",toString(i),sep=""),rats)] = cor*rater
    wave[gsub("$",paste("RaterCorSqGuess",toString(i),sep=""),rats)] = (cor^2)*rater
  }
  for(i in ids){
    r = h[[toString(i)]]
    for(j in ids){
      s = h[[toString(j)]]
     wave[wave[gsub("$",paste("Common",toString(i),sep=""),rats)] = length()
    }
  }  
}
    for(i in ids){
      str0 = paste(rat,toString(i),sep="")
      h[[str0]] = wave[wave["id"] == i,][[rat]]
      str = paste(rat,paste("Rater",toString(i),sep=""),sep="")
      str2 = paste(rat,paste("RaterCor",toString(i),sep=""),sep="")
      str3 = paste(rat,paste("RaterCorSq", toString(i),sep=""),sep="")
      str4 = paste(rat,paste("CommonYesRater", toString(i),sep=""),sep="")
      str5 = paste(rat,paste("CommonNoRater", toString(i),sep=""),sep="")
      str6 = paste(rat,paste("CommonRater", toString(i),sep=""),sep="")
      str7 = paste(rat,paste("RaterCorGuess",toString(i),sep=""),sep="")
      str8 = paste(rat,paste("RaterCorSqGuess", toString(i),sep=""),sep="")
      str9 = paste(rat,paste("CommonYesRaterGuess", toString(i),sep=""),sep="")
      str10 = paste(rat,paste("CommonNoRaterGuess", toString(i),sep=""),sep="")
      str11 = paste(rat,paste("CommonRaterGuess", toString(i),sep=""),sep="")
      for(string in c(str, str2, str3, str4, str5, str6, str7, str8, str9, str10, str11)){
        wave[string] = 0
        h[[string]] = 0
      }
    }

      for(j in ids){

        wave[wave["id" == j,][[str]] = wave[wave["id" == j,][[str]]
        wave[j] = h
      }
        str = paste(rat,paste("Rater",toString(i),sep=""),sep="")
        
      }
    }
    

    h = hash()
    g = hash()
    for(i in ids){
      h[[toString(i)]] = wave[wave["id"] == i,][str1]
      g[[toString(i)]] = wave[wave["id"] == i,][str2]
    }
    for(i in ids){
      slice_i = h[[toString(i)]]
      slice_i2 = g[[toString(i)]]
      for(p in partners){
        h[[paste(toString(i),toString(p),sep="")]] = slice_i[slice_i["partner"] !=p,]
        g[[paste(toString(i),toString(p),sep="")]] = slice_i2[slice_i2["partner"] !=p,]
      }
    }
     
    h[[paste(toString(i),toString(i),sep="")]] = wave[wave["id"] == i,][str1]
    g[[paste(toString(i),toString(i),sep="")]] = wave[wave["id"] == i,][str1]
      
    g[[toString(i)]] = wave[wave["id"] == i,][str2]
    }
    for(i in ids){
      for(j in ids){
        slice_j = wave[wave["id"] == j,][str1]
        slice_j2 = wave[wave["id"] == j,][str2]
        for(k in partners){
          slice_ik = slice_i[[str1]]
          slice_i2 = wave[wave["id"] == i,][[str2]]
          slice_j = wave[wave["id"] == j,][[str1]]
          slice_j2 = wave[wave["id"] == ij,][[str2]]
        }
      }
      for(j in partners){
        slice_ij = slice_i[slice_i["partner"] != j,][[str1]]
        slice_i2j = slice_i[slice_i["partner"] != j,][[str1]]
         for(k in ids){
          slice_kj = wave[wave["id"] == k & wave["partner"] != j,][[str1]]
          slice_kj = wave[wave["id"] == k & wave["partner"] != j,][[str2]]
          
        }
      }
      for(j in ids){
        wave[wave["id"] == j,][str] = wave[wave["id"] == i,][rat]
      }
    }    
    wave = wave[,colSums(wave^2) !=0]
    for(i in ids){
      str = paste(rat,paste("Rater",toString(i),sep="_"),sep="_")
      wave[str] = 0
      slice_i = wave[wave["id"] == i,][rat]
      for(j in ids){
        wave[wave["id"] == j,][str] = wave[wave["id"] == i,][rat]
      }
    }
  }
}
    

  wave = wave[,colSums(wave^2) !=0]
  num_cols = ncol(wave)
  for(x in names(wave)[18:num_cols]){
    wave[x] = wave[x] - mean(wave[[x]])
  }
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
