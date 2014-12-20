
mergeDF = function(men, women){
  men = makeRatingMetrics(men)
  women = makeRatingMetrics(women)
  n = names(men)
  men = men[, !(n %in% c("gender", "X"))]
  women = women[, !(n %in% c("gender", "X"))]
  n = names(men)
  colnames(men) = gsub("$", "M", n)
  colnames(women) = gsub("$", "W",n)
  x_merges = c("iidW", "idW", "waveW", "orderW" ,"partnerW", "pidW", "matchW", "sameraceW")
  y_merges = c("pidM", "partnerM", "waveM","orderM", "idM", "iidM", "matchM", "sameraceM")
  merged = merge(women, men, by.x = x_merges, by.y = y_merges)
  
  
  colnames(merged)[c(1,2,3,4,5,6,7,8)] = c("iidW", "idW", "wave", "order", "idM", "iidM", "match", "sameRace")
  
  merged[["combAvgM"]] = rowSums(scale(merged[c("decAvgM", "likeAvgM", "attrAvgM")]))/3
  merged[["combAvgW"]] = rowSums(scale(merged[c("decAvgW", "likeAvgW", "attrAvgW")]))/3
  merged[["attrDecAvgW"]] = rowSums(scale(merged[c("decAvgW", "attrAvgW")]))/2
  merged[["attrDecAvgM"]] = rowSums(scale(merged[c("decAvgM", "attrAvgM")]))/2
  merged[["financeMCombW"]] = rowSums(merged[c("careerFinanceMcareerFinanceWCross", "careerFinanceMcareerLawWCross")])
  merged[["ambMambW"]] = ifelse(merged[["ambAvgM"]] > 0 & merged[["ambAvgW"]] != 0, merged[["ambAvgM"]]*merged[["ambAvgW"]], 0)
  merged[["attrMattrW"]] = ifelse(merged[["attrAvgM"]] < 0.5 & merged[["attrAvgW"]] > 0,0, merged[["attrDecAvgM"]]*merged[["attrDecAvgW"]])
  merged[["attrMsincW"]] = ifelse(merged[["attrAvgM"]] > 0, merged[["attrAvgM"]]*merged[["sincAvgW"]], 0)
  merged[["attrLowM"]] = ifelse(merged[["attrAvgM"]] < 0, merged[["attrAvgM"]], 0)
  merged[["attrDecLowM"]] = ifelse(merged[["attrDecAvgM"]] < 0, merged[["attrDecAvgM"]], 0)
  merged[["attrDecLowW"]] = ifelse(merged[["attrDecAvgW"]] < 0, merged[["attrDecAvgW"]], 0)
  n = names(merged)
  return(merged)
}
