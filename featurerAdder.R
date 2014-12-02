library(gdata)
library(ggplot2)
library(miscTools)
library(xlsx)
library(hash)
library(aod)
library(randomForest)
library(LiblineaR)
library(Matrix)
library(plyr)
library(recommenderlab)
library(scatterplot3d)

all = read.csv('~/Desktop/speedDating/cleanedData.csv')

#Adds binary features for multiple choice questions
for(str in c("goal", "date", "race", "go_out", "field_cd", "career_c")){
  codes = unique(all[[str]])
  for(code in codes){
    all[paste(str,toString(code),sep="")] = ifelse(all[[str]] == code, 1, 0)
  }
}

all = all[, !(names(all) %in% c("race", "field_cd", "career_c", "goal"))]

colnames(all)[grep("goal",colnames(all))] =   c("goalMeetNew", "goalFunNight", "goalOther", 
                                                "goalGetDate", "goalSeriousRel", "goalSayDid")
colnames(all)[grep("^race",colnames(all))] = c("raceAsian", "raceWhite", "rateOther", "raceLatino", "raceBlack")
colnames(all)[grep("field",colnames(all))] = c("fieldLaw", "fieldMath", "fieldPoliSci", "fieldBusiness", "fieldEngin", "fieldAcademia",
                         "fieldSocialSci", "fieldSocialWork", "fieldUndecided", "fieldMedicine", "fieldHistReligPhil", "fieldEnglish",
                         "fieldScience", "fieldFilm", "fieldLanguages", "fieldArts", "fieldArch", "fieldOther")
colnames(all)[grep("career",colnames(all))] = c("careerCreative", "careerLaw", "careerInternational", "careerAcademic", "careerFinance",
                                                "careerUndecided","careerEngineer", "careerPsychology", "careerMedicine", "careerSports", 
                                                "careerSocialWork", "careerRealEstate","careerOther", "careerSpeech", 
                                                "careerArch", "careerPolitics", "careerJourn")


addAverages = function(df, ratings){
  n = names(df)
  sums = c(gsub("Rating", "Sum", ratings))
  averages = gsub("Rating", "AvgRating", ratings)
  df = ddply(df, .(id) ,transform ,raterDecSum = sum(dec))
  df = ddply(df, .(partner) ,transform ,decSum = sum(dec))
  df = ddply(df, .(partner) ,transform ,attrSum = sum(attrRating))
  df = ddply(df, .(partner) ,transform ,sincSum = sum(sincRating))
  df = ddply(df, .(partner) ,transform ,funSum = sum(funRating))
  df = ddply(df, .(partner) ,transform ,intelSum = sum(intelRating))
  df = ddply(df, .(partner) ,transform ,ambSum = sum(ambRating))
  df = ddply(df, .(partner) ,transform ,sharSum = sum(sharRating))
  df = ddply(df, .(partner) ,transform ,likeSum = sum(likeRating))
  df = ddply(df, .(partner) ,transform ,probSum = sum(probRating))
  
  
  num_ratees = length(unique(df[["partner"]]))
  num_raters = length(unique(df[["id"]]))
  df[averages] = (df[sums] - df[ratings])/(num_raters - 1)
  df["decAvg"] = (df["decSum"] - df["dec"])/(num_raters - 1)
  df["raterDecAvg"] = (df["raterDecSum"] - df["dec"])/(num_ratees - 1)
  df = df[,!(names(df) %in% c(sums, "decSum", "raterDecSum"))]
  return(df)
}


addGuesses = function(df, ratings){
  num_ratees = length(unique(df[["pid"]]))
  num_raters = length(unique(df[["iid"]]))
  for(s in ratings){
    print(s)
    rating_matrix = t(matrix(df[[s]], nrow = num_ratees, ncol = num_raters))
    guesses = t(matrix(nrow = num_ratees, ncol = num_raters))
    for(i in 1:num_raters){
      for(j in 1:num_ratees){
        temp_matrix = rating_matrix
        temp_matrix[i,j] = NA
        r <- as(temp_matrix, "realRatingMatrix")
        recommender = Recommender(r, method = "UBCF")
        recom <- predict(recommender, r, type="ratings")        
        guesses[i, j] = as(recom, "matrix")[i,j]
      }
    }
    guessName = gsub("$", "Guess", s)
    df[[guessName]] =  matrix(t(guesses), nrow = num_raters*num_ratees, ncol = 1)[,1]
    df[guessName] = ifelse(df[[guessName]] > 10, 10, df[[guessName]] )    
    df[guessName] = ifelse(df[[guessName]] < 0, 0, df[[guessName]] )
  }
  return(df)
}

addRatingFeatures = function(df){
  n = names(df)
  ratings = n[grep("Rating",n)]
  df = addAverages(df, ratings)
  df = addGuesses(df, ratings)
  return(df)
}

processWaves = function(df){
  stub = df[0,]
  for(w in unique(df[["wave"]])){
    print(w)
    slice = df[df["wave"] == w,]
    men = slice[slice["gender"] == 1,]
    women = slice[slice["gender"] == 0,]
    men = addRatingFeatures(men)
    women = addRatingFeatures(women)
    slice = rbind(men, women)
    stub = rbind(stub, slice)
  }
  return(stub)
}

new_all = processWaves(all)
men = new_all[new_all["gender"] == 1,]
women = new_all[new_all["gender"] == 0,]




write.csv(men, "~/Desktop/speedDating/menCollaborativelyFiltered2.csv")
write.csv(women, "~/Desktop/speedDating/womenCollaborativelyFiltered2.csv")


