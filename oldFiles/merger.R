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
# 
df = read.csv('~/Desktop/speedDating/speedDatingDataCleaned.csv')
# 
# df["totalRatingOwn"] = 0
# df["totalRatingPartner"] = 0
# n = names(df)
# for(avg in n[grep("Mean.Exc", n)][5:20]){
#   df[avg] = df[avg] - mean(df[[avg]])
# }
# for(partner_means in n[grep("Partner.Mean.Exc", n)][3:9]){
#   df["totalRatingPartner"] = df["totalRatingPartner"] + df[partner_means]
# }
# df["totalRatingPartner"] = df["totalRatingPartner"]/length(grep("Partner.Mean.Exc", n))
# for(own_means in n[grep("Own.Mean.Exc", n)][3:9]){
#   df["totalRatingOwn"] = df["totalRatingOwn"] + df[own_means]
# }
# df["totalRatingOwn"] = df["totalRatingOwn"]/length(grep("Own.Mean.Exc", n))
# 
# for(partner_mean in n[grep("Partner.Mean.Exc", n)][3:9]){
#   string = paste(partner_mean,"Adjusted")
#   df[string] = df[partner_mean]  - df["totalRatingPartner"] 
# }
# for(own_mean in n[grep("Own.Mean.Exc", n)][3:9]){
#   string = paste(own_mean,"Adjusted")
#   df[string] = df[own_mean]  - df["totalRatingOwn"] 
# }
df = round(df, 2)
men = df[df["gender"] == 1,]
women = df[df["gender"] == 0,]
# own_means = n[grep("Own.Mean.Exc", n)]
# n = names(men)
# partner_means_adjusted = n[grep("Partner.Mean.Exc.Adjusted", n)]
# own_means_adjusted = n[grep("Own.Mean.Exc.Adjusted", n)]
# cor(men[partner_means_adjusted], men["dec"])
# cor(women[partner_means_adjusted], women["dec"])

women_id_joins = c("iid",  "pid", "wave")
men_id_joins = c("pid", "iid", "wave")


merged = merge(women, men, by.x = women_id_joins, by.y = men_id_joins)


write.csv(men , '~/Desktop/speedDating/dataMen.csv')
write.csv(women , '~/Desktop/speedDating/dataWomen.csv')
write.csv(merged , '~/Desktop/speedDating/dataMerged.csv')

for
# 
# # merged = read.csv('~/Desktop/speedDating/dataMerged.csv')
# s = merged["dec.Partner.Mean.Exc.x"]
# t = merged["dec.Partner.Mean.Exc.y"]
# merged["differenceInDesirability"] = log(s/(1-s)) - log(t/(1-t))
# difference = merged[["differenceInDesirability"]]
# merged["differenceInDesirability"] = ifelse(is.na(difference) | difference == Inf | difference == -Inf, 0, merged[["differenceInDesirability"]])
# merged["differenceInDesirability"] = ifelse(merged[["differenceInDesirability"]] == Inf, 0, merged[["differenceInDesirability"]])
# 
# write.csv(merged , '~/Desktop/speedDating/dataMerged.csv')
# merged = read.csv('~/Desktop/speedDating/dataMerged.csv')
# 
# 
# 
# f = function(df, features,tar){
#   cutoff = nrow(df)/2
#   train = df[1:cutoff,]
#   test = df[(cutoff + 1):nrow(df),]
#   target = factor(train[,tar])
#   s=scale(train[features],center=TRUE,scale=TRUE)
#   co=heuristicC(s)
#   m=LiblineaR(data=s,labels=target,type=0,cost=co,bias=TRUE,verbose=FALSE)
#   s2= scale(test[features],attr(s,"scaled:center"),attr(s,"scaled:scale"))
#   p=predict(m,s2)
#   t = table(p$predictions, test[[tar]])
#   print(t)
#   print((t[1,2] + t[2,1])/nrow(test))
# }
# n = names(merged)
# womens_ratings_of_men = n[grep("Own.Mean.Exc.x", n)]
# mens_ratings_of_women = n[grep("Own.Mean.Exc.y", n)]
# womens_ratings_by_men = n[grep("Partner.Mean.Exc.y", n)]
# mens_ratings_by_women = n[grep("Partner.Mean.Exc.x", n)]
# wrom = womens_ratings_of_men
# mrow = mens_ratings_of_women
# wrbm = womens_ratings_by_men
# mrbw = mens_ratings_by_women
# women_acts = names(merged)[26:42]
# men_acts = names(merged)[232:248]
# round(cor(merged[women_acts], merged[women_acts]), 1) - round(cor(merged[men_acts], merged[men_acts]), 1)
# ratings_avgs = c("dec.Partner.Mean.Exc.y", "like.Partner.Mean.Exc.y", "attr.Partner.Mean.Exc.y", "fun.Partner.Mean.Exc.y", "intel.Partner.Mean.Exc.y", "sinc.Partner.Mean.Exc.y", "sinc.Partner.Mean.Exc.y")
# for(r in ratings_avgs){
#   sub = substring(r, 1, 3)
#   merged[paste(sub,"short.y")] = merged[r]
# }
# 
# ratings_avgs = c("dec.Partner.Mean.Exc.x", "like.Partner.Mean.Exc.x", "attr.Partner.Mean.Exc.x", "fun.Partner.Mean.Exc.x", "intel.Partner.Mean.Exc.x", "sinc.Partner.Mean.Exc.x", "sinc.Partner.Mean.Exc.x")
# for(r in ratings_avgs){
#   sub = substring(r, 1, 3)
#   merged[paste(sub,"short.x")] = merged[r]
# }
# for(act in men_acts){
#   merged[paste(act,"Adjusted.y")] = merged[act] - merged["activityAvg.y"]
# }
# for(act in women_acts){
#   merged[paste(act,"Adjusted.x")] = merged[act] - merged["activityAvg.x"]
# }
# 
# 
# 
# activityAvg.y
# round(cor(merged[women_acts], merged[418:423]), 1)
# round(cor(merged[women_acts], merged[424:429]), 1)
# round(cor(merged[c("activityAvg.x", "activityAvg.y")], merged[418:429]), 1)
# features = ratings_avgs
# f(merged,features,"dec.x")
