source("~/Desktop/speedDatingFinal/libraries.R")
source("~/Desktop/speedDatingFinal/machineLearning2.R")
merged = read.csv('~/Desktop/speedDatingFinal/crossesAdded.csv')
n = names(merged)
mRatings = n[grep("RatingM$",n)]
mRatings = mRatings[c(1:6,8)]
mRatings[c(2,3,4,5,6,7)] = mRatings[c(7,2,5,6,4,3)]
traits = gsub("RatingM$", "", mRatings)

wRatings = gsub("$", "RatingW", traits)
wAvgs = gsub("$", "AvgW", traits)
wPrefs = gsub("$", "PrefW", traits)
mPrefs = gsub("$", "PrefM", traits)

mAvgs = gsub("$", "AvgM", traits)
avgs = gsub("$", "Avg", traits)
n = names(merged)
prefs = n[grep("Pref",n)]
mPrefs = prefs[grep("M$",prefs)]
wPrefs = prefs[grep("W$",prefs)]
niceCors(merged,wPrefs,"attrLikeDecAvgW")
bNew = (niceCors(merged, wAvgs, wAvgs) + niceCors(merged, mAvgs, mAvgs))/200
wAvgRatingCors = niceCors(merged, wAvgs[1:7], wRatings[1:7])/100
myImagePlot(wAvgRatingCors, 
            xLabels=traits[1:7], yLabels=avgs[1:7],
            title=c("Averages vs. Ratings (W by M)"), 
            zlim=c(0.05,0.55)) 


mAvgRatingCors = niceCors(merged, mAvgs[1:7], mRatings[1:7])/100

merged["newMQual"] = merged["decAvgM"] + merged["funAvgM"] + merged["attrAvgM"] + merged["likeAvgM"]
mAvgRatingCors = niceCors(merged, c(mAvgs[1:7], "newMQual"), mRatings[1])/100
mAvgRatingCors
myImagePlot(mAvgRatingCors, 
            xLabels=traits[1:7], yLabels=avgs[1:7],
            title=c("Averages vs. Ratings (M by W)"), 
            zlim=c(0.05,0.55)) 


mfrow=c(1, 2)
bNew = niceCors(merged, mAvgs, mAvgs)/100

bNew = (niceCors(merged,wAvgs[1:8], wRatings[1:8]) + niceCors(merged, mAvgs[1:8], mRatings[1:8]))/200
myImagePlot(bNew, 
            xLabels=traits[1:8], yLabels=avgs[1:8],
            title=c("Average Ratings vs. Ratings"), 
            zlim=c(0,0.5)) 



niceCors(merged, wAvgs, c("decM"))
niceCors(merged, mAvgs, c("decW"))
orderDF = data.frame()
for(wave in unique(merged[["wave"]])){
  orderDF[toString(wave),] = 0
  orderDF[toString(wave),"decAvg"] = cor(merged[merged["wave"] ==wave,"decM"], merged[merged["wave"] ==wave,"decAvgW"])
  orderDF[toString(wave),"attrAvg"] = cor(merged[merged["wave"] ==wave,"decM"], merged[merged["wave"] ==wave,"attrAvgW"])
  orderDF[toString(wave),"likeAvg"] = cor(merged[merged["wave"] ==wave,"decM"], merged[merged["wave"] ==wave,"likeAvgW"])
}

round(100*orderDF)
colSums(orderDF)


myImagePlot(bNew, 
            xLabels=c(4,), yLabels=traits,
            title=c("Average Rating Correlations"), 
            zlim=c(0.15,1)) 


rounded = round(100*orderDF)
myImagePlot(cNew, 
            xLabels=traits, yLabels=traits,
            title=c("rat"), 
            zlim=c(0.15,0.35)) 

cNew = (niceCors(merged, wAvgs, wAvgs) + niceCors(merged, MAvgs, MAvgs))/200

ratings = gsub("$", "Rating", traits)
avgs = gsub("$", "Avg", traits)
bNew = niceCors(df, avgRatings, ratings)/100
myImagePlot(bNew, 
            xLabels=traits, yLabels=avgs,
            title=c("Average Rating Correlations"), 
            zlim=c(-0,1)) 

scoreSlice[["shavedFinal"]] = ifelse(scoreSlice[["final"]] == 0.01,0,scoreSlice[["final"]])
scoreSlice[["shavedBasic"]] = ifelse(scoreSlice[["basic"]] == 0.01,0,scoreSlice[["basic"]])

scoreSlice = read.csv('~/Desktop/speedDatingFinal/scoreSlice.csv')
topMetrics = read.csv('~/Desktop/speedDatingFinal/topMetrics.csv')
wSummed = aggregate(scoreSlice, scoreSlice["iidW"], FUN=sum)
mSummed = aggregate(scoreSlice, scoreSlice["iidM"], FUN=sum)
hist(round(mSummed[["probDecW"]]))
table(round(mSummed[["probDecW"]]))
hist(mSummed[["probMatchFinal"]])
head(topMetrics)
n = names(topMetrics)
topMetrics[n[grep("FracFoundW",n)]]
mean(scoreSlice[["orderM"]])
finalOrdered = maxes[order(maxes["final"]),]

table(finalOrdered[1:40,][c("final", "match")][["match"]])
matchSlice = scoreSlice[scoreSlice["match"] == 1,]

noMatchSlice = scoreSlice[scoreSlice["match"] == 0,]

mean(table(round(100*matchSlice[["final"]])))
mean(matchSlice[["basic"]])
mean(noMatchSlice[["basic"]])

mean(matchSlice[["final"]])
mean(noMatchSlice[["final"]])


topMetrics[n[grep("WInTopN",n)]]


niceCors(mSummed, c("decM", "decW", "match"), c('decM', "decW", "match"))
niceCors(mSummed, c("basicProbDecM", "basicProbDecW", "basicProbMatch"), c("basicProbDecM", "basicProbDecW", "basicProbMatch"))
niceCors(mSummed, c("probDecM", "probDecW", "probMatchFinal"), c("probDecM", "probDecW", "probMatchFinal"))

niceCors(wSummed, c("decM", "decW", "match"), c('decM', "decW", "match"))
plot(topMetrics[["randomFracFoundM"]], type="o", col="blue",xlim=c(1,6), ylim=c(0,60))


title(main="% of Matches In Top N Recommendations", col.main="red", font.main=4)
lines(topMetrics[["basicFracFoundM"]], type="o", col="red",xlim=c(1,10), ylim=c(0,100))
lines(topMetrics[["finalFracFoundM"]], type="o", col="green",xlim=c(1,10), ylim=c(0,100))

names(merged)
merged["combAvgM"] = rowSums(scale(merged[c("decAvgM", "attrAvgM", "likeAvgM")]))

for(w in unique(merged[["wave"]])){
  slice = merged[merged["wave"] == w,]
  print(niceCors(slice, c("decAvgM", "attrAvgM", "likeAvgM", "combAvgM" ), "decW"))
}





