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
men_acts = n[235:251]
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
men_acts_adj = n[343:358]
men_avg_ratee_ratings = n[grep("RateeAvgExc.y", n)]
men_avg_rater_ratings = n[grep("RaterAvgExc.y", n)]
women_avg_ratee_ratings = n[grep("RateeAvgExc.x", n)]
men_avg_rater_ratings = n[grep("RaterAvgExc.x", n)]
women_adj_rater_ratings = n[grep("RaterAdj.x", n)]
women_adj_ratee_ratings = n[grep("RateeAdj.x", n)]
men_adj_rater_ratings = n[grep("RaterAdj.y", n)]
men_adj_ratee_ratings = n[grep("RateeAdj.y", n)]


decs = c("decRaterAvgExc.x", "decRaterAvgExc.y", "decRateeAvgExc.x", "decRateeAvgExc.y")
for(d in 1:length(decs)){
  new_sequence[d] = mean(merged[[decs[d]]])
}
agged = aggregate(merged, by=merged["iid"], FUN=mean)
agged2 = aggregate(merged, by=merged["pid"], FUN=mean)

attrs = c(women_goals, women_date, women_race, women_go_out, women_fields, women_careers)
attrs2 = c(men_goals, men_date, men_race, men_go_out, men_fields, men_careers)

new_attrs = c(1,2,3)
new_attrs2 = c(1,2,3)
i = 1
for(a in attrs){
  if(nrow(agged[agged[a] == 1,]) >= 10){
    new_attrs[i] = a
    i = i + 1
  }
}
i = 1
for(a in attrs2){
  if(nrow(agged2[agged2[a] == 1,]) >= 10){
    new_attrs2[i] = a
    i = i + 1
  }
}

for(a in attrs){
  for(a2 in attrs2){
    merged[paste(a,a2,sep="_")] = merged[a]*merged[a2]
  }
}

final_attrs = c()
i = 1
for(n in names(merged)[445:4036]){
  if(nrow(merged[merged[n] == 1,]) >= 30){
    final_attrs[i] 
  }
}
round(cor(agged[new_attrs], agged[decs]),1)
