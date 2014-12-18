
men = read.csv("~/Desktop/speedDating/menFeaturesAdded.csv")
write.csv("~/Desktop/speedDating/womenFeaturesAdded.csv")

men = men[, !(names(men) %in% c("gender", "X"))]
women = women[, !(names(women) %in% c("gender", "X"))]
men = men[-1:0]
women = women[-1:0]
colnames(men)= gsub("$", "M", names(men))
colnames(women) = gsub("$", "W", names(women))
x_merges = c("iidW", "idW", "waveW", "partnerW", "pidW", "matchW", "sameRaceW")
y_merges = c("pidM", "partnerM", "waveM", "idM", "iidM", "matchM", "sameRaceM")
merged = merge(women, men, by.x = x_merges, by.y = y_merges)


colnames(merged)[c(1,2,3,4,5,6,7)] = c("iidW", "idW", "wave", "idM", "iidM", "match", "sameRace")

mean(merged[["combRateeAvgM"]])
mean(merged[["combRateeAvgW"]])
n = names(merged)

features = n[grep("imprace|imprelig|happy|expnum|Pref|date$|goOut$|RaterAvgM$|RaterAvgW$|RateeAvgW$|RateeAvgM$",n)]
featuresW = features[grep("W$", features)]
featuresM = features[grep("M$", features)]
