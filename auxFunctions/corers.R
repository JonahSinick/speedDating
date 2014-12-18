source("~/Desktop/speedDatingFinal/libraries.R")
source("~/Desktop/speedDatingFinal/machineLearning2.R")


# merged = read.csv('~/Desktop/speedDatingFinal/crossesAdded.csv')

menCorer = function(anotherDF, trait){
  sds = c(-3, 0, 3)
  
  df = data.frame()
  df[1:2,] = 0
  df[c("tier","corAttr", "corLike","corDec","corAttrLikeDec", "corSinc","corIntel", "corFun", "corAmb", "decMAvg")] = 0
  for(i in 1:2){
    df[i,"tier"] = i
    slice = anotherDF[anotherDF[trait] >= sds[i] & anotherDF[trait] < sds[i + 1],]
    df[i,"corAttr"] = round(100*cor(slice[["attrAvgW"]], slice[["decM"]]))
    df[i,"corSinc"] = round(100*cor(slice[["sincAvgW"]], slice[["decM"]]))
    df[i,"corIntel"] = round(100*cor(slice[["intelAvgW"]], slice[["decM"]]))
    df[i,"corFun"] = round(100*cor(slice[["funAvgW"]], slice[["decM"]]))
    df[i,"corAmb"] = round(100*cor(slice[["ambAvgW"]], slice[["decM"]]))
    df[i,"corDec"] = round(100*cor(slice[["decAvgW"]], slice[["decM"]]))
    df[i,"corLike"] = round(100*cor(slice[["likeAvgW"]], slice[["decM"]]))
    df[i,"corComb"] = round(100*cor(slice[["combAvgW"]], slice[["decM"]]))
    df[i,"decMAvg"] = round(100*mean(slice[["decM"]]) - 100*mean(merged[["decM"]]))
  }
  return(df)
}





womenCorer = function(anotherDF, trait){
  sds = c(-3, -0.5, 0.5, 3)
  
  df = data.frame()
  df[1:3,] = 0
  df[c("tier","corAttr", "corLike","corDec","corAttrLikeDec", "corSinc","corIntel", "corFun", "corAmb", "decWAvg")] = 0
  for(i in 1:3){
    df[i,"tier"] = i
    slice = anotherDF[anotherDF[trait] > sds[i] & anotherDF[trait] < sds[i + 1],]
    df[i,"corAttr"] = round(100*cor(slice[["attrAvgM"]], slice[["decW"]]))
    df[i,"corSinc"] = round(100*cor(slice[["sincAvgM"]], slice[["decW"]]))
    df[i,"corIntel"] = round(100*cor(slice[["intelAvgM"]], slice[["decW"]]))
    df[i,"corFun"] = round(100*cor(slice[["funAvgM"]], slice[["decW"]]))
    df[i,"corAmb"] = round(100*cor(slice[["ambAvgM"]], slice[["decW"]]))
    df[i,"corDec"] = round(100*cor(slice[["decAvgM"]], slice[["decW"]]))
    df[i,"corLike"] = round(100*cor(slice[["likeAvgM"]], slice[["decW"]]))
    df[i,"corComb"] = round(100*cor(slice[["combAvgM"]], slice[["decW"]]))
    
    
    df[i,"decWAvg"] = round(100*mean(slice[["decW"]]) - 100*mean(merged[["decW"]]))
  }
  return(df)
}



womenPrefCorer = function(anotherDF, trait){
  sds = c(-3, -0.5, 0.5, 3)
  
  df = data.frame()
  df[1:3,] = 0
  df[c("tier","corAttr","corAttrLikeDec", "corSinc","corIntel", "corFun", "corAmb","corShar", "decWAvg")] = 0
  for(i in 1:3){
    df[i,"tier"] = i
    slice = anotherDF[anotherDF[trait] > sds[i] & anotherDF[trait] < sds[i + 1],]
    df[i,"corAttr"] = round(100*cor(slice[["attrPrefM"]], slice[["decW"]]))
    df[i,"corSinc"] = round(100*cor(slice[["sincPrefM"]], slice[["decW"]]))
    df[i,"corIntel"] = round(100*cor(slice[["intelPrefM"]], slice[["decW"]]))
    df[i,"corFun"] = round(100*cor(slice[["funPrefM"]], slice[["decW"]]))
    df[i,"corAmb"] = round(100*cor(slice[["ambPrefM"]], slice[["decW"]]))
    df[i,"corShar"] = round(100*cor(slice[["sharPrefM"]], slice[["decW"]]))      
    df[i,"decWAvg"] = round(100*mean(slice[["decW"]]) - 100*mean(merged[["decW"]]))
  }
  return(df)
}


