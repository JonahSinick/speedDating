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
men = df[df["gender"] == 1,]
women = df[df["gender"] == 0,]
own_id_info = c("iid", "pid", "match", "wave")
partner_id_info = c("pid", "iid", "match", "wave")
merged = merge(women, men, by.x = own_id_info, by.y = partner_id_info)
addDecs = function(wave){
  for(gender in c("X", "Y")){
    str = paste("rater", gender, sep="")
    other_str = paste("ratee", gender, sep="")
    otherDecType = ifelse(gender=="X", "dec.y", "dec.x")
    if(gender =="X"){
      decType = "dec.x"
      idType = "id.x"
      partnerType = "partner.x"
      ids = unique(wave[[idType]])
      partners = unique(wave[[partnerType]])
    }
    if(gender =="Y"){
      decType = "dec.y"
      idType = "id.y"
      partnerType = "partner.y"
      ids = unique(wave[[idType]])
      partners = unique(wave[[partnerType]])
    }

    for(i in ids){
      strung = paste(str,toString(i))
      strung2 = paste(other_str,toString(i))
      wave[strung] = 0
      wave[strung2] = 0
      slice_i = wave[wave[idType] == i,]
      for(j in ids){
        if(i == j){
          wave[wave[idType] == j,][[strung]] = 0
        }
        else{
          wave[wave[idType] == j,][[strung]] = slice_i[[decType]]
          wave[wave[idType] == j,][[strung2]] = slice_i[[otherDecType]]
        }
      }
    }			
  }
  return(wave)
}

addCorsAndProbs = function(wave){
  for(gender in c("X", "Y")){
    if(gender =="X"){
      decType = "dec.x"
      otherDecType = "dec.y"
      idType = "id.x"
      partnerType = "partner.x"
      ids = unique(wave[[idType]])
      partners = unique(wave[[partnerType]])
      otherGender = "Y"
    }
    if(gender =="Y"){
      decType = "dec.y"
      idType = "id.y"
      partnerType = "partner.y"
      otherDecType = "dec.x"
      ids = unique(wave[[idType]])
      partners = unique(wave[[partnerType]])
      otherGender = "X"
    }
    for(i in ids){
      rater = paste("rater", paste(gender,toString(i),sep=" "), sep="")
      ratee = paste("ratee", paste(otherGender,toString(i),sep=" "), sep="")
      rater_cor = paste("raterCor", paste(gender,toString(i),sep=""), sep="")
      rater_sq_cor = paste("raterSqCor", paste(gender,toString(i),sep=""), sep="")
      ratee_cor = paste("rateeCor", paste(otherGender,toString(i),sep=""), sep="")
      ratee_sq_cor = paste("rateeSqCor", paste(otherGender,toString(i),sep=""), sep="")
      rater_prob_estimate = paste("raterProbEst", paste(gender,toString(i),sep=""), sep="")			
      ratee_prob_estimate = paste("rateeProbEst", paste(otherGender,toString(i),sep=""), sep="")
      wave[rater_cor] = 0
      wave[rater_sq_cor] = 0			
      wave[ratee_cor] = 0
      wave[ratee_sq_cor] = 0			
      for(j in ids){
        if(i !=j ){
          slice_j = wave[wave[idType] == j,]
          for(p in partners){
            p_slice_j = slice_j[slice_j[partnerType] !=p,]
            c = cor(p_slice_j[decType], p_slice_j[rater])
            c = ifelse(is.na(c), 0, c)
            num = ifelse(c >= 0, c*c, -c*c)
            wave[wave[idType] == j,][[rater_cor]] = c*ifelse(slice_j[[rater]] == 1, 1, -1)
            wave[wave[idType] == j,][[rater_sq_cor]] = num*ifelse(slice_j[[rater]] == 1, 1, -1)  				
          }
          agrees = ifelse(slice_j[decType] == slice_j[rater], 1, 0)
          agrees = sum(agrees) - agrees
          wave[wave[idType] == j,][[rater_prob_estimate]] = agrees/(length(ids) - 1) 
        }
      }
      for(p in partners){
        slice_p = wave[wave[partnerType] == p,]
        for(j in ids){
          if(i != j){
            j_slice_p = slice_p[slice_p[idType] !=j,]
            c = cor(j_slice_p[otherDecType], j_slice_p[ratee])
            c = ifelse(is.na(c), 0, c)
            num = ifelse(c >= 0, c*c, -c*c)
            wave[wave[idType] == j,][[ratee_cor]] = c*ifelse(slice_p[[ratee]] == 1, 1, -1)
            wave[wave[idType] == j,][[ratee_sq_cor]] = num*ifelse(slice_p[[ratee]] == 1, 1, -1)  				  
          }
        }
        agrees = ifelse(slice_j[decType] == slice_j[ratee], 1, 0)
        agrees = sum(agrees) - agrees
        wave[wave[idType] == j,][[ratee_prob_estimate]] = agrees/(length(ids) - 1)        
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

# addFinalProbEstimates = function(wave){
#   wave["finalProb"] = 1
#   wave[pa"finalProb"] = 1
#   rater_probs = n[grep("raterProbEst.", n)]
#   ratee_probs = n[grep("rateeProbEst.", n)]
#   for(p in c(rater_probs,ratee_probs)){
#     wave[p] = ifelse(wave[p] == 0, 0.01, wave[p])
#     wave[p] = ifelse(wave[p] == 1, 0.99, wave[p])
#   }
#   for(p in rater_probs){
#     wave[paste("finalProb",rater,sep="")] = wave[paste("finalProb",rater,sep="")]*oddsRatio(p)
#   }
#   for(p in ratee_probs){
#     wave[paste("finalProb",ratee,sep="")] = wave[paste("finalProb",ratee,sep="")]*oddsRatio(p)
#   }
#   return(wave)
# }

for(i in 1:20){
  print(i)
  wave = merged[merged["wave"] == i,]
  wave = addDecs(wave)
  wave = addCorsAndProbs(wave)
  
  write.csv(wave, paste("~/Desktop/waves/decisions",paste(toString(i),".csv",sep=""),sep=""))    
}