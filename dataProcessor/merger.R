
mergeDF = function(df){
  men = df[df["gender"] == 1,]
  women = df[df["gender"] == 0,]
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
  return(merged)
}
