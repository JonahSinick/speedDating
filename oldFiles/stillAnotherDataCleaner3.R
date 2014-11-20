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
to_be_processed = c("XX", "XY", "YX", "YY")
# Cases: 
# Gender = (Same, Different), person = (Rater, Ratee)
# If gender is Same and person is Rater, look at how much cor there is between own decisions and person's decisions
#and then look at how person decided on each ratee
# If gender is Same and person is Ratee, look at how much cor there is between raters of opposite gender saying yes
# and saying yes to them, and then see whether ratee of opposite gender said yes to Ratee
addDecCols = function(wave, sym1, sym2){
  genderDecider = ifelse(sym1=="X", "dec.x", "dec.y")
  deciderIDs = ifelse(sym1=="X", unique(wave[["id.x"]]), unique(wave[["id.y"]]))
  partnerIDs = ifelse(sym1=="X", unique(wave[["partner.x"]]), unique(wave[["partner.y"]]))
  partnerID = ifelse(sym1=="X", "partner.x", "partner.y")
  deciderID = ifelse(sym1=="X", "id.x", "id.y")
  genderRater = ifelse(sym2=="X","dec.x", "dec.y")
  raterIDs = ifelse(sym2=="X", unique(wave[["id.x"]]), unique(wave[["id.y"]]))
  raterID = ifelse(sym2=="X", "id.x", "id.y")
  yes_str = ifelse(sym2 == "X", "yesAnswerX", "yesAnswerY")
  no_str = ifelse(sym2 == "X", "noAnswerX", "noAnswerY")
  if(sym1 == "X" & sym2 == "X"){
    prob_str = "probXX"
    cor_str = "corXX"
    sq_cor_str = "sqCorXX"
  }
  if(sym1 == "X" & sym2 == "Y"){
    prob_str = "probXY"
    cor_str = "corXY"
    sq_cor_str = "sqCorXY"
  }
  if(sym1 == "Y" & sym2 == "X"){
    prob_str = "probYX"
    cor_str = "corYX"
    sq_cor_str = "sqCorYX"
  }
  if(sym1 == "Y" & sym2 == "Y"){
    prob_str = "probYY"
    cor_str = "corYY"
    sq_cor_str = "sqCorYY"
  }
  for(j in raterIDs){
    slice_j = wave[wave[raterID] == j,]
    wave[paste(yes_str,toString(j),sep="_")] = slice_j[[genderRater]]
    wave[paste(no_str,toString(j),sep="_")] = 1 - wave[paste(yes_str,toString(j),sep="_")]
    wave[paste(prob_str,toString(j),sep="_")] = 0
    wave[paste(cor_str,toString(j),sep="_")] = 0
    wave[paste(sq_cor_str,toString(j),sep="_")] = 0
  }
  for(j in raterIDs){
    slice_j = wave[wave[raterID] == j,]
    for(k in deciderIDs){
      if(j !=k ){
        slice_k = wave[wave[deciderID] == k,]
        for(p in partnerIDs){
          c =  cor(slice_k[slice_k[partnerID] != p,][[paste(yes_str,toString(j),sep="_")]], slice_k[slice_k[partnerID] != p,][[paste(yes_str,toString(k),sep="_")]])
          c = ifelse(is.na(c), 0.01, c)
          num = ifelse(c >= 0, c*c, -c*c)
          wave[wave[deciderID] == k,][paste(cor_str,toString(j),sep="_")] = c*ifelse(slice_j[[paste(yes_str,toString(j),sep="_")]] == 1, 1, -1)
          wave[wave[deciderID] == k,][paste(sq_cor_str,toString(j),sep="_")] = (num)*ifelse(slice_j[[paste(yes_str,toString(j),sep="_")]] == 1, 1, -1)
        }        
      }
    }
  }
  for(j in raterIDs){
    slice_j = wave[wave[raterID] == j,]
    for(k in deciderIDs){
      if(k == j){
        wave[wave[deciderID] == k,][[paste("prob",toString(j),sep="_")]] = 0.5
        wave[wave[deciderID] == k,][[paste("betterProb",toString(j),sep="_")]] = 0.5
      }
      else{
        slice_k = wave[wave[deciderID] == k,]
        both_yes = slice_k[[paste("yesAnswerX",toString(k),sep="_")]]*slice_j[[paste("yesAnswerX",toString(j),sep="_")]]
        both_no = slice_k[[paste("noAnswerX",toString(k),sep="_")]]*slice_j[[paste("noAnswerX",toString(j),sep="_")]]
        other_yes = slice_k[[paste("noAnswerX",toString(k),sep="_")]]*slice_j[[paste("yesAnswerX",toString(j),sep="_")]]
        other_no = slice_k[[paste("yesAnswerX",toString(k),sep="_")]]*slice_j[[paste("noAnswerX",toString(j),sep="_")]]
        both_yes = sum(both_yes) - both_yes
        both_no = sum(both_no) - both_no
        other_no = sum(other_no) - other_no
        other_yes = sum(other_yes) - other_yes        
        yes_sequence = ((both_yes + other_no)/(length(x_ids) - 1))
        no_sequence = ((both_no + other_yes)/(length(x_ids) - 1))
        agree_sequence = ((both_no + both_yes)/(length(x_ids) - 1))
        disagree_sequence = ((other_no + other_yes)/(length(x_ids) - 1))
        
        wave[wave["id.x"] == k,][[paste("prob",toString(j),sep="_")]] = c(ifelse(slice_j[paste("yesAnswerX",toString(j),sep="_")] == 1, yes_sequence, 1 - no_sequence))
        wave[wave["id.x"] == k,][[paste("betterProb",toString(j),sep="_")]] = c(ifelse(slice_j[paste("yesAnswerX",toString(j),sep="_")] == 1, agree_sequence, 1 - agree_sequence))
        
  
}
oddsRatio = function(p){
  return(p/(1 - p))
}


inverseOddsRatio = function(or){
  return(or/(1 + or))
}












for(i in 1:20){
  print(i)
  wave = merged[merged["wave"] == i,]

  
  len = length(unique(wave[["id.x"]]))
  len2 = length(unique(wave[["id.y"]]))
  x_ids = unique(wave[["id.x"]])
  y_ids = unique(wave[["id.y"]])
  for(j in x_ids){
    slice_j = wave[wave["id.x"] == j,]
    wave[paste("yesAnswerX",toString(j),sep="_")] = slice_j[["dec.x"]]
    wave[paste("noAnswerX",toString(j),sep="_")] = 1 - slice_j[["dec.x"]]
    wave[paste("prob",toString(j),sep="_")] = 0
    wave[paste("betterProb",toString(j),sep="_")] = 0
    wave[paste("raterCor",toString(j),sep="_")] = 0
    wave[paste("raterCorSq",toString(j),sep="_")] = 0
  }
  for(j in x_ids){
    slice_j = wave[wave["id.x"] == j,]
    for(k in x_ids){
      if(j !=k ){
        slice_k = wave[wave["id.x"] == k,]
        for(p in y_ids){
          c =  cor(slice_k[slice_k["id.y"] != p,][[paste("yesAnswerX",toString(j),sep="_")]], slice_k[slice_k["id.y"] != p,][[paste("yesAnswerX",toString(k),sep="_")]])
          c = ifelse(is.na(c), 0.01, c)
          num = ifelse(c >= 0, c*c, -c*c)
          wave[wave["id.x"] == k,][paste("raterCor",toString(j),sep="_")] = c*ifelse(slice_j[[paste("yesAnswerX",toString(j),sep="_")]] == 1, 1, -1)
          wave[wave["id.x"] == k,][paste("raterCorSq",toString(j),sep="_")] = (num)*ifelse(slice_j[[paste("yesAnswerX",toString(j),sep="_")]] == 1, 1, -1)
        }        
      }
    }
  }
  for(j in x_ids){
    slice_j = wave[wave["id.x"] == j,]
    for(k in x_ids){
      if(k == j){
        wave[wave["id.x"] == k,][[paste("prob",toString(j),sep="_")]] = 0.5
        wave[wave["id.x"] == k,][[paste("betterProb",toString(j),sep="_")]] = 0.5
      }
      else{
        slice_k = wave[wave["id.x"] == k,]
        both_yes = slice_k[[paste("yesAnswerX",toString(k),sep="_")]]*slice_j[[paste("yesAnswerX",toString(j),sep="_")]]
        both_no = slice_k[[paste("noAnswerX",toString(k),sep="_")]]*slice_j[[paste("noAnswerX",toString(j),sep="_")]]
        other_yes = slice_k[[paste("noAnswerX",toString(k),sep="_")]]*slice_j[[paste("yesAnswerX",toString(j),sep="_")]]
        other_no = slice_k[[paste("yesAnswerX",toString(k),sep="_")]]*slice_j[[paste("noAnswerX",toString(j),sep="_")]]
        both_yes = sum(both_yes) - both_yes
        both_no = sum(both_no) - both_no
        other_no = sum(other_no) - other_no
        other_yes = sum(other_yes) - other_yes        
        yes_sequence = ((both_yes + other_no)/(length(x_ids) - 1))
        no_sequence = ((both_no + other_yes)/(length(x_ids) - 1))
        agree_sequence = ((both_no + both_yes)/(length(x_ids) - 1))
        disagree_sequence = ((other_no + other_yes)/(length(x_ids) - 1))

        wave[wave["id.x"] == k,][[paste("prob",toString(j),sep="_")]] = c(ifelse(slice_j[paste("yesAnswerX",toString(j),sep="_")] == 1, yes_sequence, 1 - no_sequence))
        wave[wave["id.x"] == k,][[paste("betterProb",toString(j),sep="_")]] = c(ifelse(slice_j[paste("yesAnswerX",toString(j),sep="_")] == 1, agree_sequence, 1 - agree_sequence))
        
      }
    }
  }
  
  wave = addProbs(wave)
  write.csv(wave, paste("~/Desktop/waves/newestWave",paste(toString(i),".csv",sep=""),sep=""))    
}

# ----------------------------------------------------

for(i in 1:20){
  wave = read.csv(paste("~/Desktop/waves/newestWave",paste(toString(i),".csv",sep=""),sep=""))    
  
  print(i)  
  len2 = length(unique(wave[["id.y"]]))
  y_ids = unique(wave[["id.y"]])
  for(j in y_ids){
    slice_j = wave[wave["id.y"] == j,]
    wave[paste("yesAnswerY",toString(j),sep="_")] = slice_j[["dec.y"]]
    wave[paste("noAnswerY",toString(j),sep="_")] = 1 - slice_j[["dec.y"]]
    wave[paste("probY",toString(j),sep="_")] = 0
    wave[paste("betterProbY",toString(j),sep="_")] = 0
    wave[paste("raterCorY",toString(j),sep="_")] = 0
    wave[paste("raterCorSqY",toString(j),sep="_")] = 0
  }
  for(j in y_ids){
    slice_j = wave[wave["id.y"] == j,]
    for(k in y_ids){
      if(j !=k ){
        slice_k = wave[wave["id.y"] == k,]
        for(p in y_ids){
          c =  cor(slice_k[slice_k["id.y"] != p,][[paste("yesAnswerY",toString(j),sep="_")]], slice_k[slice_k["id.y"] != p,][[paste("yesAnswerY",toString(k),sep="_")]])
          c = ifelse(is.na(c), 0.01, c)
          num = ifelse(c >= 0, c*c, -c*c)
          wave[wave["id.y"] == k,][paste("raterCorY",toString(j),sep="_")] = c*ifelse(slice_j[[paste("yesAnswerY",toString(j),sep="_")]] == 1, 1, -1)
          wave[wave["id.y"] == k,][paste("raterCorSqY",toString(j),sep="_")] = (num)*ifelse(slice_j[[paste("yesAnswerY",toString(j),sep="_")]] == 1, 1, -1)
        }        
      }
    }
  }
  for(j in y_ids){
    slice_j = wave[wave["id.y"] == j,]
    for(k in y_ids){
      if(k == j){
        wave[wave["id.y"] == k,][[paste("probY",toString(j),sep="_")]] = 0.5
        wave[wave["id.y"] == k,][[paste("betterProbY",toString(j),sep="_")]] = 0.5
      }
      else{
        slice_k = wave[wave["id.y"] == k,]
        both_yes = slice_k[[paste("yesAnswerY",toString(k),sep="_")]]*slice_j[[paste("yesAnswerY",toString(j),sep="_")]]
        both_no = slice_k[[paste("noAnswerY",toString(k),sep="_")]]*slice_j[[paste("noAnswerY",toString(j),sep="_")]]
        other_yes = slice_k[[paste("noAnswerY",toString(k),sep="_")]]*slice_j[[paste("yesAnswerY",toString(j),sep="_")]]
        other_no = slice_k[[paste("yesAnswerY",toString(k),sep="_")]]*slice_j[[paste("noAnswerY",toString(j),sep="_")]]
        both_yes = sum(both_yes) - both_yes
        both_no = sum(both_no) - both_no
        other_no = sum(other_no) - other_no
        other_yes = sum(other_yes) - other_yes        
        yes_sequence = ((both_yes + other_no)/(length(y_ids) - 1))
        no_sequence = ((both_no + other_yes)/(length(y_ids) - 1))
        agree_sequence = ((both_no + both_yes)/(length(y_ids) - 1))
        disagree_sequence = ((other_no + other_yes)/(length(y_ids) - 1))
        
        wave[wave["id.y"] == k,][[paste("probY",toString(j),sep="_")]] = c(ifelse(slice_j[paste("yesAnswerY",toString(j),sep="_")] == 1, yes_sequence, 1 - no_sequence))
        wave[wave["id.y"] == k,][[paste("betterProbY",toString(j),sep="_")]] = c(ifelse(slice_j[paste("yesAnswerY",toString(j),sep="_")] == 1, agree_sequence, 1 - agree_sequence))
        
      }
    }
  }
  write.csv(wave, paste("~/Desktop/waves/newestWave2",paste(toString(i),".csv",sep=""),sep=""))    
}  




addProbs = function(wave){
  n = names(wave)
  wave["decProb"] = 1
  wave["betterDecProb"] = 1
  probs = n[grep("prob.", n)]
  betterProbs = n[grep("betterProb.", n)]
  for(p in probs){
    wave[p] = ifelse(wave[[p]] == 1, 0.99,  wave[[p]])
    wave[p] = ifelse(wave[[p]] == 0, 0.01,  wave[[p]])
    wave["decProb"] = wave["decProb"]*oddsRatio(wave[p])    
  }
  for(p in betterProbs){
    wave[p] = ifelse(wave[[p]] == 1, 0.99,  wave[[p]])
    wave[p] = ifelse(wave[[p]] == 0, 0.01,  wave[[p]])    
    wave["betterDecProb"] = wave["betterDecProb"]*oddsRatio(wave[p])
  }  
  wave["decProb"] = inverseOddsRatio(wave["decProb"]^(1/(length(probs))))
  wave["betterDecProb"] = inverseOddsRatio(wave["betterDecProb"]^(1/(length(betterProbs))))
  wave["guess"] = ifelse(wave["decProb"] > 0.5, 1, 0)
  wave["betterGuess"] = ifelse(wave["betterDecProb"] > 0.5, 1, 0)
  return(wave)
}