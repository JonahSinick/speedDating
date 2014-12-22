n = names(merged)
sds = n[grep("SD",n)]
merged[sds] = scale(merged[sds])

f = function(df ,lower, upper, corName, corSD, avg, decType){
  slice = df[lower < df[corSD]  & df[corSD] < upper ,]
  print(nrow(slice))
  print(mean(slice[[corName]]))
  print(cor(slice[[decType]],slice[[avg]]))
  print(mean(slice[[decType]]))
}
hist(merged[["attrRatingCorSDM"]])
f(merged,-300000, 200, "attrRatingCorM","attrRatingCorSDM", "attrAvgW", "decM")
sd(merged[["attrRatingCorSDM"]])
nrow(merged[-3 < merged["attrRatingCorSDM"] & merged["attrRatingCorSDM"] < 3,])
table(merged[["attrRatingCorSDM"]])
table(merged[["attrRatingCorSDM"]] > 0)
