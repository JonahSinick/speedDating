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

merged = read.csv('~/Desktop/speedDating/mergedCrossFeaturesAdded.csv')

formTrainAndTest = function(df){
  waves = unique(df[["wave_W"]])
  idxs = sample(waves, length(waves)/2)
  train = df[(df[["wave_W"]] %in% idxs),]
  test = df[!(df[["wave_W"]] %in% idxs),]
}
}