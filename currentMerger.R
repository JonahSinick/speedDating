men = read.csv("~/Desktop/speedDating/menCollaborativelyFiltered.csv")
women = read.csv("~/Desktop/speedDating/womenCollaborativelyFiltered.csv")
ratings = n[grep("Rating",n)]
menNames = gsub("$", "M", names(men))
womenNames = gsub("$", "W", names(men))
colnames(men) = menNames
colnames(women) = womenNames
menRatingIndices = grep("RatingM|RatingAvgM|GuessM|decAvgM", names(men))
menRatings  = names(men)[menRatingIndices]
menRatings = gsub("M$", "OfWbyM",menRatings)
colnames(men)[menRatingIndices] = menRatings

womenRatingIndices = grep("RatingW|RatingAvgW|GuessW|decAvgW", names(women))
womenRatings  = names(men)[womenRatingIndices]
womenRatings = gsub("W$", "OfMbyW",womenRatings)
colnames(women)[womenRatingIndices] = womenRatings

men = men[-2:0]
women = women[-2:0]
x_merges = c("iidW", "idW", "waveW", "partnerW", "pidW", "sameraceW", "matchW")
y_merges = c("pidM", "partnerM", "waveM", "idM", "iidM", "sameraceM", "matchM")
merged = merge(women, men, by.x = x_merges, by.y = y_merges)
colnames(merged)[grep(x_merges, names(merges))] = c("iidW, idW", "wave", 
                                                    "idM", "iidM", "sameRace", "match")
write.csv(merged, "~/Desktop/speedDating/merged.csv")

