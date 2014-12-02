men = read.csv("~/Desktop/speedDating/menCollaborativelyFiltered.csv")
women = read.csv("~/Desktop/speedDating/womenCollaborativelyFiltered.csv")

merged[c("matchAvgM", "matchAvgW")] = 0
slice = merged[c("iidW", "pidW", "match")]
for(i in unique(merged[["iidW"]])){
  newSlice = slice[slice["iidW"] == i, "match"]
  merged[merged["iidW"] == i,][["matchAvgW"]]  = (sum(newSlice[["match"]]) - newSlice[["match"]])/(nrow(newSlice) - 1)
}
for(i in unique(merged[["pidW"]])){
  newSlice = slice[slice["pidW"] == i, "match"]
  merged[merged["pidW"] == i,][["matchAvgM"]]  = (sum(newSlice[["match"]]) - newSlice[["match"]])/(nrow(newSlice) - 1)
}

men = men[, !(names(men) %in% c("gender"))]
women = women[, !(names(women) %in% c("gender"))]
ratings = n[grep("Rating",n)]
menNames = gsub("$", "M", names(men))
womenNames = gsub("$", "W", names(women))
colnames(men) = menNames
colnames(women) = womenNames
menRatingIndices = grep("RatingM|RatingAvgM|GuessM|decAvgM", names(men))
menRatings  = names(men)[menRatingIndices]
menRatings = gsub("M$", "W",menRatings)
colnames(men)[menRatingIndices] = menRatings

womenRatingIndices = grep("RatingW|RatingAvgW|GuessW|decAvgW", names(women))
womenRatings  = names(women)[womenRatingIndices]
womenRatings = gsub("W$", "M",womenRatings)
colnames(women)[womenRatingIndices] = womenRatings
men = men[-2:0]
women = women[-2:0]
x_merges = c("iidW", "idW", "waveW", "partnerW", "pidW", "matchW", "sameraceW")
y_merges = c("pidM", "partnerM", "waveM", "idM", "iidM", "matchM", "sameraceM")
merged = merge(women, men, by.x = x_merges, by.y = y_merges)
merged = merged[-2:0]
names(merged)
colnames(merged)[c(1,2,3,4,5,6,7)] = c("iidW", "idW", "wave", "idM", "iidM", "match", "sameRace")
write.csv(merged, "~/Desktop/speedDating/merged.csv")

