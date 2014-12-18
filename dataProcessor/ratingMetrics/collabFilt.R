addGuesses = function(df){
  n = names(df)
  ratings = n[grep("Rating",n)]
  for(rating in ratings){
    guessName = paste(rating,"Guess",sep="")
    df[guessName] = 0
    waves = unique(df[["wave"]])
    for(wave in waves){
      print(c(guessName,wave))
      df[df["wave"] == wave,][[guessName]] = addGuessToWave(df[df["wave"] == wave,], rating, guessName)
    }
  }
  return(df)
}



addGuessToWave = function(slice, rating, guessName){
  slice["temp"] = 0
  raters = unique(slice[["iid"]])
  ratees = unique(slice[["pid"]])
  slice = slice[order(slice["pid"]),]
  for(i in 1:nrow(slice)){
    rater = slice[i,"iid"]
    ratee = slice[i,"pid"]
    slice[-i,"temp"] = slice[-i,rating]
    slice[i,"temp"] = NA
    ratingMatrix = matrix(slice[["temp"]], nrow = length(raters), ncol = length(ratees))
    r <- as(ratingMatrix, "realRatingMatrix")
    recommender = Recommender(r, method = "UBCF")
    recom <- predict(recommender, r, type="ratings") 
    slice[["temp"]] = c(as(recom, "matrix"))
    slice[i,guessName] = slice[i,"temp"]
  }
  slice = slice[order(slice["iid"]),] 
  return(slice[[guessName]])
}
