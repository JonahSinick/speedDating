basicBinaries = function(df){
  len = ncol(df)
  for(str in c("goal", "date", "race", "goOut", "field_cd", "career_c")){
    codes = unique(df[[str]])
    for(code in codes){
      df[paste(str,toString(code),sep="")] = ifelse(df[[str]] == code, 1, 0)
    }
  }
  
  df = df[, !(names(df) %in% c("race", "field_cd", "career_c", "goal"))]
  
  colnames(df)[grep("goal",colnames(df))] =   c("goalMeetNew", "goalFunNight", "goalOther", 
                                                "goalGetDate", "goalSeriousRel", "goalSayDid")
  colnames(df)[grep("^race",colnames(df))] = c("raceAsian", "raceWhite", "rateOther", "raceLatino", "race")
  colnames(df)[grep("field",colnames(df))] = c("fieldLaw", "fieldMath", "fieldPoliSci", "fieldBusiness", "fieldEngin", "fieldAcademia",
                                               "fieldSocialSci", "fieldSocialWork", "fieldUndecided", "fieldMedicine", "fieldHistReligPhil", "fieldEnglish",
                                               "fieldScience", "fieldFilm", "fieldLanguages", "fieldArts", "fieldArch", "fieldOther")
  colnames(df)[grep("career",colnames(df))] = c("careerCreative", "careerLaw", "careerInternational", "careerAcademic", "careerFinance",
                                                "careerUndecided","careerEngineer", "careerPsychology", "careerMedicine", "careerSports", 
                                                "careerSocialWork", "careerRealEstate","careerOther", "careerSpeech", 
                                                "careerArch", "careerPolitics", "careerJourn")
  n = names(df)
  drops = c(n[grep("goOut|date",n)])
  for(name in n[-len:0]){
    slice = df[df[name] == 1,][c("gender", "iid")]
    len1 = length(unique(slice[slice["gender"] == 1,][["iid"]]))
    len2 = length(slice[slice["gender"] == 0,][["iid"]])
    if(len1 < 20 | len2 < 20){
      drops = c(drops, name)
    }  
  } 
  df = df[!(n %in% drops)]
  return(df)
}
