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
# df = read.csv( '~/Desktop/speedDating/speedDatingData.csv')

merged = read.csv( '~/Desktop/speedDating/dataMerged.csv')
n = names(merged)

women_acts = n[25:41]
women_prefs = n[44:49]
women_beliefs_about_opp_sex = n[50:55]
women_self_perceptions = n[56:59]
women_ratings_of_men = n[61:68]
women_goals = n[70:75]
women_date = n[76:82]
women_race = n[83:87]
women_go_out = n[88:94]
women_fields = n[95:112]
women_careers = n[113:129]
women_acts_adj = n[133:148]
men_acts = n[241:257]
men_prefs = n[254:259]
men_beliefs_about_opp_sex = n[260:265]
men_self_perceptions = n[266:269]
men_ratings_of_women = n[271:278]
men_goals = n[280:285]
men_date = n[286:292]
men_race = n[293:297]
men_go_out = n[298:304]
men_fields = n[305:322]
men_careers = n[323:339]
men_acts_adj = n[349:364]



# men_avg_ratee_ratings = n[grep("RateeAvgExc.y", n)]
# men_avg_rater_ratings = n[grep("RaterAvgExc.y", n)]
# women_avg_ratee_ratings = n[grep("RateeAvgExc.x", n)]
# men_avg_rater_ratings = n[grep("RaterAvgExc.x", n)]
# women_adj_rater_ratings = n[grep("RaterAdj.x", n)]
# women_adj_ratee_ratings = n[grep("RateeAdj.x", n)]
# men_adj_rater_ratings = n[grep("RaterAdj.y", n)]
# men_adj_ratee_ratings = n[grep("RateeAdj.y", n)]

stripped = merged
n = names(stripped)
careers = n[grep("career",n)]
goals = n[grep("goal", n)][14:16]
dates = n[grep("date", n)]
go_outs = n[grep("go_out", n)]
races = n[grep("race", n)]
races = races[c(1,3:9, 11:16)]
fields = n[grep("field", n)]
fields = fields[c(c(2:15),c(17:28))]
careers = careers[c(c(2:13),c(16:25))]
attributes = c(careers, races)

men_attributes = attributes[grep(".y", attributes)]
women_attributes = attributes[grep(".x", attributes)]
for(name in men_attributes){
  for(name2 in women_attributes){
    stripped[paste(name,name2,sep="_")] = stripped[name]*stripped[name2]
  }
}
n = names(stripped)
n = c(n[grep("career",n)], n[grep("race",n)])

w_avg = 0.375
m_avg = 0.484

# for(name in n){
#   slice = stripped[stripped[name] == 1,]
#   frac = sum(slice[["dec.x"]])/nrow(slice)    
#   if(nrow(slice) > 100 & abs(frac - w_avg) > 0.05){
#     print(name)
#     print(nrow(slice))
#     print(frac)    
#   }
# }

men_matrix = matrix(nrow = 4, ncol = 4)
women_matrix = matrix(nrow = 4, ncol = 4)
match_matrix = matrix(nrow = 4, ncol = 4)
frac_matrix = matrix(nrow = 4, ncol = 4)
anchor = 4.5
name = "sinc"
for(i in 1:4){
  for(j in 1:4){
    print(c(i,j))
    slice = merged[merged[paste(name, "RateeAvgExc.x", sep="")] < (anchor + i) &  merged[paste(name, "RateeAvgExc.x", sep="")] > (anchor + i - 1),]
    slice2 = slice[slice[paste(name, "RateeAvgExc.y", sep="")] < (anchor + j) &  slice[paste(name, "RateeAvgExc.y", sep="")] > (anchor +  j - 1),]
    frac_woman = mean(slice2[["dec.x"]])
    frac_man = mean(slice2[["dec.y"]])
    frac_match = mean(slice2[["match.x"]])
    frac_matrix[i, j] = round(nrow(slice2)/nrow(merged), 2)
    men_matrix[i, j] = round(100*frac_man, 0)
    women_matrix[i, j] = round(100*frac_woman, 0)
    match_matrix[i, j] = round(100*frac_match, 0)
  }
}


slice = merged[merged[paste(name, "RateeAvgExc.x", sep="")] < anchor + 2*i &  merged[paste(name, "RateeAvgExc.x", sep="")] > anchor + 2*(i -1),]
slice2 = slice[slice[paste(name, "RateeAvgExc.y", sep="")] < anchor + 2*j &  slice[paste(name, "RateeAvgExc.y", sep="")] > anchor + 2*(j - 1),]
frac_woman = mean(slice2[["dec.x"]])
frac_man = mean(slice2[["dec.y"]])
frac_match = mean(slice2[["match.x"]])
frac_matrix[i, j] = round(nrow(slice2)/nrow(merged), 2)
men_matrix[i, j] = round(100*frac_man, 0)
women_matrix[i, j] = round(100*frac_woman, 0)
match_matrix[i, j] = round(100*frac_match, 0)



merged["CompositeRateeAvgExc.y"] = merged["attrRateeAvgExc.y"] + merged["likeRateeAvgExc.y"] + merged["funRateeAvgExc.y"]
merged["CompositeRateeAvgExc.x"] = merged["attrRateeAvgExc.x"] + merged["likeRateeAvgExc.x"] + merged["funRateeAvgExc.x"]
n = names(merged)
cor(merged[n[grep("RateeAvgExc.x", n)]], merged[n[grep("RateeAvgExc.x", n)]])
m = median(merged[["CompositeRateeAvgExc.y"]])
m2 = median(merged[["CompositeRateeAvgExc.x"]])

for(name in c("Composite", "dec", "attr", "fun", "like", "prob", "sinc", "intel")){
  name1 = paste(name,"RateeAvgExc.y", sep="")
  name2 = paste(name,"RateeAvgExc.x", sep="")
  m = median(merged[[name1]])
  m2 = median(merged[[name2]])
  name3 = paste(name,"ManHighWomanHigh",sep="")
  name4 = paste(name,"ManHighWomanLow",sep="")
  name5 = paste(name,"ManLowWomanHigh",sep="")
  name6 = paste(name,"ManLowWomanLow",sep="")
  merged[name3] = ifelse(merged[[name1]] > m & merged[[name2]] > m2, 1, 0 )
  merged[name4] = ifelse(merged[[name1]] < m & merged[[name2]] > m2, 1, 0 )
  merged[name5] = ifelse(merged[[name1]] > m & merged[[name2]] < m2, 1, 0 )
  merged[name6] = ifelse(merged[[name1]] < m & merged[[name2]] < m2, 1, 0 )
}


for(i in 1:4){
  print(i)
  print(mean(h[[toString(i)]][["dec.x"]]))
  print(mean(h[[toString(i)]][["dec.y"]]))
  print(mean(h[[toString(i)]][["match.x"]]))
}

f = function(df, features,tar){
  df = df[sample(1:nrow(df)),]
  cutoff = nrow(df)/2
  train = df[1:cutoff,]
  test = df[(cutoff + 1):nrow(df),]
  target = factor(train[,tar])
  s=scale(train[features],center=TRUE,scale=TRUE)
  co=heuristicC(s)
  m=LiblineaR(data=s,labels=target,type=0,cost=co,bias=TRUE,verbose=FALSE)
  s2= scale(test[features],attr(s,"scaled:center"),attr(s,"scaled:scale"))
  p=predict(m,s2)
  t = table(p$predictions, test[[tar]])
  print(t)
  print((t[1,2] + t[2,1])/nrow(test))
}


merged["WhiteManAsianWoman"] = ifelse(merged[["race.2.y"]] == 1 & merged[["race.4.x"]] == 1, 1, 0)
merged["AsianManAsianWoman"] = ifelse(merged[["race.4.y"]] == 1 & merged[["race.4.x"]] == 1, 1, 0)
merged["AsianManWhiteWoman"] = ifelse(merged[["race.4.y"]] == 1 & merged[["race.2.x"]] == 1, 1, 0)
merged["WhiteManWhiteWoman"] = ifelse(merged[["race.2.y"]] == 1 & merged[["race.2.x"]] == 1, 1, 0)
merged["OtherManWhiteWoman"] = ifelse(merged[["race.2.y"]] != 1 & merged[["race.2.x"]] == 1, 1, 0)
merged["crossedRateeYeses"] = merged["decRateeAvgExc.x"]*merged["decRateeAvgExc.y"]
merged["crossedRaterYeses"] = merged["decRaterAvgExc.x"]*merged["decRaterAvgExc.y"]
races = names(merged)[473:477]
merged["decRateeAvgExc.y"] = merged["decRateeAvgExc.y"] + 0.01
merged["decRateeAvgExc.x"] = merged["decRateeAvgExc.x"] + 0.01
merged["decDifference"] =  merged["decRateeAvgExc.x"] - merged["decRateeAvgExc.y"]
merged["decLogQuotient"] =  log(merged["decRateeAvgExc.x"]/merged["decRateeAvgExc.y"])
merged["attrDifference"] =  merged["attrRateeAvgExc.x"] - merged["attrRateeAvgExc.y"]
merged["attrLogQuotient"] =  log(merged["attrRateeAvgExc.x"]/merged["attrRateeAvgExc.y"])
merged["intelDifference"] =  merged["intelRateeAvgExc.x"] - merged["intelRateeAvgExc.y"]
merged["intelLogQuotient"] =  log(merged["intelRateeAvgExc.x"]/merged["intelRateeAvgExc.y"])
merged["sincDifference"] =  merged["sincRateeAvgExc.x"] - merged["sincRateeAvgExc.y"]
merged["sincLogQuotient"] =  log(merged["sincRateeAvgExc.x"]/merged["sincRateeAvgExc.y"])
merged["funDifference"] =  merged["funRateeAvgExc.x"] - merged["funRateeAvgExc.y"]
merged["funLogQuotient"] =  log(merged["funRateeAvgExc.x"]/merged["funRateeAvgExc.y"])

for(i in 1:17){
  print(i)
  name1 = women_acts_adj[i]
  name2 = men_acts_adj[i]
  merged[paste(name1, name2, sep="_")] = merged[name1]*merged[name2]
}
woman_career = n[grep("career_c", n)][2:18]
man_career = n[grep("career_c", n)][20:36]
for(i in 1:17){
  for(j in 1:17){
    if(sum(merged[[woman_career[i]]]*merged[[man_career[j]]]) >=50) {
      merged[paste(woman_career[i],man_career[j],sep="")] = merged[woman_career[i]]*merged[man_career[j]]  
    }
  }
}

merged2 = merged[,colSums(merged)^2 != 0]
features = names(merged)[506:527]
f(merged, features, "dec.x")
# n = names(merged)
# prefs = n[grep("1_1", n)]
# x_prefs = prefs[grep(".x", prefs)]
# y_prefs = prefs[grep(".y", prefs)]
# for(pref in prefs){
#   m = median(merged[[pref]])
#   merged[pref] = merged[pref] - m
#   merged[paste(pref,"High",sep="")] = ifelse(merged[[pref]] > 0, 1,0)
# }
# 
# x_prefs_new = names(merged)[506:511]
# y_prefs_new = names(merged)[512:517]
# 
# for(k in length(y_prefs_new)){
#   x_pref_new = x_prefs_new[6]
#   y_pref_new = y_prefs_new[6]
#   merged[paste(x_pref_new,"BothHigh",sep="")] = merged[x_pref_new]*merged[y_pref_new]
# }
n = names(merged)
careers = n[grep("career_c", n)]
careers_x = careers[grep(".x",careers)][1:16]
careers_y = careers[grep(".y",careers)][1:16]


career_inters = names(merged)[506:527]
features = c(careers_x, careers_y, career_inters)
merged = merged[sample(nrow(merged)),]
train = merged[seq(1, 5*nrow(merged)/10),] 
test = merged[seq(5*nrow(merged)/10 +  1, nrow(merged)),] 
rf_fit <- randomForest(y=as.factor(train[,"dec.x"]), x=train[features], importance=TRUE, ntree=500)
t =  table(predict(rf_fit, test) == test[["dec.x"]])
print(t[[1]]/nrow(test))

seq = c()
h = hash()
slice1 = merged
slice1["noise"] =  sample(c(-2, -1, 0, 1,2), nrow(slice1), replace = TRUE)

# for(i in unique(slice1[["iid"]])){
#   h[[toString(i)]] = slice1[slice1["iid"] ==i,]
# }
for(j in 1:517){
  for(i in unique(slice1[["iid"]])){
    slice = h[[toString(i)]]
    seq[i] = cor(slice[[j]], slice[["match.x"]])
  }  
  alpha = na.omit(seq)
  if(length(alpha) > 0){
    beta = round(10*alpha,0)
    beta = beta - median(beta)
    t = table(abs(beta) > 2)
    gamma = 1 - t[[1]]/(length(beta))
    if(gamma < 0.35){
      print(names(slice)[j])
      print(gamma)
    }
  }
}

