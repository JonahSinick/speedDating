men = read.csv("~/Desktop/speedDating/menCollaborativelyFiltered.csv")
women = read.csv("~/Desktop/speedDating/womenCollaborativelyFiltered.csv")
men = men[, !(names(men) %in% c("gender"))]
women = women[, !(names(women) %in% c("gender"))]
ratings = n[grep("Rating",n)]
menNames = gsub("$", "M", names(men))
womenNames = gsub("$", "W", names(women))
colnames(men) = menNames
colnames(women) = womenNames
menRatingIndices = grep("RatingM|RatingAvgM|GuessM|decAvgM", names(men))
menRatings  = names(men)[menRatingIndices]
menRatings = gsub("M$", "byMofW",menRatings)
colnames(men)[menRatingIndices] = menRatings

womenRatingIndices = grep("RatingW|RatingAvgW|GuessW|decAvgW", names(women))
womenRatings  = names(women)[womenRatingIndices]
womenRatings = gsub("W$", "byWofM",womenRatings)
colnames(women)[womenRatingIndices] = womenRatings
men = men[-2:0]
women = women[-2:0]
x_merges = c("iidW", "idW", "waveW", "partnerW", "pidW", "matchW", "sameraceW")
y_merges = c("pidM", "partnerM", "waveM", "idM", "iidM", "matchM", "sameraceM")
merged = merge(women, men, by.x = x_merges, by.y = y_merges)
colnames(merged)[c(3,6,7)] = c("wave", "match", "sameRace")
write.csv(merged, "~/Desktop/speedDating/merged.csv")

