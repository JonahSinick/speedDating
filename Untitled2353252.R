addGuesses = function(df, rating){
  num_ratees = length(unique(df[["pid"]]))
  num_raters = length(unique(df[["iid"]]))
  rating_matrix = t(matrix(df[[rating]], nrow = num_ratees, ncol = num_raters))
  guesses = t(matrix(nrow = num_ratees, ncol = num_raters))
  for(i in 1:num_raters){
    print(c(rating, i))
    for(j in 1:num_ratees){
      temp_matrix = rating_matrix
      temp_matrix[i,j] = NA
      r <- as(temp_matrix, "realRatingMatrix")
      recommender = Recommender(r, method = "UBCF")
      recom <- predict(recommender, r, type="ratings")        
      guesses[i, j] = as(recom, "matrix")[i,j]
    }
  }
  guessName = gsub("$", "Guess", rating)
  df[[guessName]] =  matrix(t(guesses), nrow = num_raters*num_ratees, ncol = 1)[,1]
  df[guessName] = ifelse(df[[guessName]] > 10, 10, df[[guessName]] )    
  df[guessName] = ifelse(df[[guessName]] < 0, 0, df[[guessName]] )
  return(df)
}
