
library("plyr")
library("lme4")
library("stats")
library("Metrics")
library("hash")



#To run the code, first load the csv file here http://www.stat.columbia.edu/~gelman/arm/examples/speed.dating/ as df



df = read.csv("~/Dropbox/speedDating-master 2/speedDatingData.csv")


ratingTypes = c("attr", "like","fun", "intel", "amb", "sinc")

df = df[c("iid", "pid", "gender", "round", "order", "dec",ratingTypes)]

df = df[df["round"] > 13 & df["iid"] != 28,]
#Restricts to events with at least 14 dates per person


df[ratingTypes] = lapply(df[ratingTypes], NAFixer)
pcs =c("good", "tradeoff")   





mainPrComps = function(slice){
  pcs = c("good", "tradeoff")
  slice[pcs] = 0
  agged = aggregate(slice, slice["pid"], FUN=mean) 
  agged = addPrComps(agged, ratingTypes, pcs)
  for(i in seq_len(2)){  
    c = cor(agged[[pcs[i]]],agged[["dec"]]) 
    if( c < 0) { agged[pcs[i]] = -agged[pcs[i]] } 
  }
  for(pid in unique(slice[["pid"]])){
    slice[slice["pid"] == pid,pcs] = agged[agged["pid"] == pid,pcs]
  }
  slice = slice[complete.cases(slice),]
  return(slice)
}
df = ddply(df, "gender", mainPrComps)



trainTestSplit = function(df, thresh = 0.65){
  df[["train"]] = ifelse(df[["order"]] < thresh*df[["round"]], 1, 0)
  train = df[df$train == 1,]
  groupHash = hash()
  m = glmer(dec ~ 1 + good + tradeoff + (1|iid), family=binomial(link="logit"), data=train)  
  df = makePreds(df, m, "fixed", groupHash)
  m = glmer(dec ~ 1 + good + tradeoff + (1 + tradeoff|iid), family=binomial(link="logit"), data=train)  
  groupHash[["iid"]] = c("tradeoff")
  df = makePreds(df, m, "individual", groupHash)
  return(df)
}

df = ddply(df, "gender", trainTestSplit)

test = df[df["train"] == 0,]



printMetrics(test[["dec"]], test[["fixed"]])
printMetrics(test[["dec"]], test[["individual"]])
niceCors(df, names(df),"tradeoffCoef")
