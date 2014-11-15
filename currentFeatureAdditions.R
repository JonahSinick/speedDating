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

colnames(all)[56:61] =   c("goalMeetNew", "goalFunNight", "goalOther", "goalGetDate", "goalSeriousRel", "goalSayDid")
colnames(all)[69:73] = c("raceAsian", "raceWhite", "rateOther", "raceLatino", "raceBlack")
colnames(all)[81:98] = c("fieldLaw", "fieldMath", "fieldPoliSci", "fieldBusiness", "fieldEngin", "fieldAcademia",
                         "fieldSocialSci", "fieldSocialWork", "fieldUndecided", "fieldMedicine", "fieldHistReligPhil", "fieldEnglish",
                         "fieldScience", "fieldFilm", "fieldLanguages", "fieldArts", "fieldArch", "fieldOther")
colnames(all)[99:115] = c("careerCreative", "careerLaw", "careerInternational", "careerAcademic", "careerFinance", "careerUndecided",
                         "careerEngineer", "careerPsychology", "careerMedicine", "careerSports", "careerSocialWork", "careerRealEstate",
                         "careerOther", "careerSpeech", "careerArch", "careerPolitics", "careerJourn")
n = names(all)


#produces features for activities according to their *relative* ratings by the person
activities = n[grep("Act",n)]

all[["activityAvg"]] = rowSums(all[activities])/length(activities)
adjusted_acts = gsub("$", "Adj", n[grep("Act", n)])
all[adjusted_acts] = all[activities] - rowSums(all[activities])/length(activities)


#adds a feature for each type of rating of the partner, giving the average of the ratings 

addAverages = function(df, ratings){
  n = names(df)
  sums = gsub("Rating", "Sum", ratings)
  averages = gsub("$", "Avg", ratings)
  df = ddply(df, .(id) ,transform ,raterDecSum = sum(dec))
  df = ddply(df, .(partner) ,transform ,decSum = sum(dec))
  df = ddply(df, .(partner) ,transform ,attrSum = sum(attrRating))
  df = ddply(df, .(partner) ,transform ,sincSum = sum(sincRating))
  df = ddply(df, .(partner) ,transform ,funSum = sum(funRating))
  df = ddply(df, .(partner) ,transform ,intelSum = sum(intelRating))
  df = ddply(df, .(partner) ,transform ,ambSum = sum(ambRating))
  df = ddply(df, .(partner) ,transform ,sharSum = sum(sharRating))
  df = ddply(df, .(partner) ,transform ,likeSum = sum(likeRating))
  
  num_ratees = length(unique(df[["partner"]]))
  num_raters = length(unique(df[["id"]]))
  df[averages] = (df[sums] - df[ratings])/(num_raters - 1)
  df["decAvg"] = (df["decSum"] - df["dec"])/(num_raters - 1)
  df["raterDecAvg"] = (df["raterDecSum"] - df["dec"])/(num_ratees - 1)
  df = df[,!(names(df) %in% sums)]
  return(df)
}



addGuesses = function(df, ratings){
  num_ratees = length(unique(df[["pid"]]))
  num_raters = length(unique(df[["iid"]]))
  for(s in ratings){
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
    df[[paste(s,"Guess",sep="")]] =  matrix(t(guesses), nrow = num_raters*num_ratees, ncol = 1)[,1]
    df[[paste(s,"Guess",sep="")]] = ifelse(df[[paste(s,"Guess",sep="")]] > 10, 10, df[[paste(s,"Guess",sep="")]] )
    df[[paste(s,"Guess",sep="")]] = ifelse(df[[paste(s,"Guess",sep="")]] < 0, 0, df[[paste(s,"Guess",sep="")]] )
    
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
    slice = df[df["wave"] == w,]
    men = slice[slice["gender"] == 1,]
    women = slice[slice["gender"] == 0,]
    print(c(w,"men"))
    men = addRatingFeatures(men)
    print(c(w,"women"))
    women = addRatingFeatures(women)
    slice = rbind(men, women)
    stub = rbind(stub, slice)
  }
  return(stub)
}

new_all = processWaves(all)
men = new_all[new_all["gender"] == 1,]
women = new_all[new_all["gender"] == 0,]




write.csv(men, "~/Desktop/speedDating/menCollaborativelyFiltered.csv")
write.csv(women, "~/Desktop/speedDating/womenCollaborativelyFiltered.csv")


