merged = read.csv('~/Desktop/speedDating/mergedBinariesHandled.csv')


merged = merged[!(n %in% c("dateAfterM", "dateAfterW"))]
merged[["dateM"]] = ifelse(merged[["dateM"]] <3, 3, merged[["dateM"]])
merged[["dateW"]] = ifelse(merged[["dateW"]] <3, 3, merged[["dateW"]])
n = names(merged)
dates = n[grep("date",n)]
dateMAgged = aggregate(merged,by=merged[c("dateM")],FUN=mean)
dateWagged = aggregate(merged,by=merged[c("dateW")],FUN=mean)
dateCrossagged = aggregate(merged,by=merged[c("dateM","dateW")],FUN=mean)
round(100*dateMAgged[["decRatingW"]]) - 33
round(100*dateWagged[["decRatingW"]]) - 33
matrix(round(100*dateCrossagged[["decRatingW"]]) - 33, nrow=5, ncol = 5)

round(100*dateMAgged[["decRatingM"]]) - 48
round(100*dateWagged[["decRatingM"]]) - 48
matrix(round(100*dateCrossagged[["decRatingM"]]) - 48, nrow=5, ncol = 5)




dateMAgged = aggregate(merged,by=merged[c("goOutM")],FUN=mean)
dateWagged = aggregate(merged,by=merged[c("goOutW")],FUN=mean)
dateCrossagged = aggregate(merged,by=merged[c("goOutM","goOutW")],FUN=mean)
round(100*dateMAgged[["decRatingW"]]) - 33
round(100*dateWagged[["decRatingW"]]) - 33
matrix(round(100*dateCrossagged[["decRatingW"]]) - 33, nrow=5, ncol = 5)


merged[["dateM"]] = ifelse(merged[["dateM"]] <3, 3, merged[["dateM"]])
merged[["dateW"]] = ifelse(merged[["dateW"]] <3, 3, merged[["dateW"]])

round(100*dateMAgged[["decRatingM"]]) - 48
round(100*dateWagged[["decRatingM"]]) - 48
matrix(round(100*dateCrossagged[["decRatingM"]]) - 48, nrow=5, ncol = 5)