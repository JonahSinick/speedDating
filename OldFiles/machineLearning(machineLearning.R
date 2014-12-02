merged = read.csv('~/Desktop/speedDating/mergedCrossFeaturesAdded.csv')

formTrainAndTest = function(df){
  waves = unique(df[["wave_W"]])
  idxs = sample(waves, length(waves)/2)
  train = df[(df["wave_W"] %in% idxs)]
  test = df
}