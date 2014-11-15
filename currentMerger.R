men = read.csv("~/Desktop/speedDating/menCollaborativelyFiltered.csv")
women = read.csv("~/Desktop/speedDating/womenCollaborativelyFiltered.csv")
ratings = n[grep("Rating",n)]
men_names = gsub("$", "_M", names(men))
women_names = gsub("$", "_W", names(men))
colnames(men) = men_names
colnames(women) = women_names
men_ratings  = names(men)[grep("Rating_M|RatingAvg_M|Guess_M|decAvg", names(men))]
men_ratings = gsub("$", "_of_W",men_ratings)
colnames(men)[grep("Rating_M|RatingAvg_M|Guess_M|decAvg", names(men))] = men_ratings

women_ratings  = names(women)[grep("Rating_W|RatingAvg_W|Guess_W|decAvg", names(women))]
women_ratings = gsub("$", "_of_M", women_ratings)
colnames(women)[grep("Rating_W|RatingAvg_W|Guess_W|decAvg", names(women))] = women_ratings
men = men[-2:0]
women = women[-2:0]
x_merges = c("iid_W", "id_W", "wave_W", "partner_W", "pid_W", "samerace_W", "match_W")
y_merges = c("pid_M", "partner_M", "wave_M", "id_M", "iid_M", "samerace_M", "match_M")
merged = merge(women, men, by.x = x_merges, by.y = y_merges)
write.csv(merged, "~/Desktop/speedDating/merged.csv")

