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




wave = read.csv(paste("~/Desktop/waves/menWave",paste(toString(4),".csv",sep=""),sep=""))
matrix = t(matrix(wave[["dec"]][1:324], nrow=18, ncol= 18)) - 0.5
dimnames(matrix) <- list(rownames(matrix, do.NULL = FALSE, prefix = "rater"),
                         colnames(matrix, do.NULL = FALSE, prefix = "ratee"))
# for(i in 1:16){
#   my_mean = mean(matrix[i,])
#   for(j in 1:16){
#     print(c(i,j))
#     new_matrix = matrix
#     new_matrix[i,j] = NA
#     r <- as(new_matrix, "realRatingMatrix")
#     recommender = Recommender(r, method = "UBCF")
#     recom <- predict(recommender, r, type="ratings")
#     empty_matrix[i, j] = as(recom, "matrix")[i,j] + my_mean
#   }
# }
num = 100
sum_matrix = (matrix(seq(0,0, length.out = 324), nrow=18, ncol= 18))
print("PCA")

for(k in 1:num){
  new_matrix = matrix
  for(i in 1:1){
    x_coord = sample(seq(1,18), 1)
    y_coord = sample(seq(1,18), 1)
    new_matrix[x_coord, y_coord] = NA
  }  
  r <- as(new_matrix, "realRatingMatrix")
  recommender = Recommender(r, method = "SVD")
  recom <- predict(recommender, r, type="ratings")
  rec_matrix = as(recom, "matrix")
  rec_matrix = ifelse(is.na(rec_matrix), 0, rec_matrix)
  for(j in 1:18){
    rec_matrix[j,] = rec_matrix[j,] + mean(matrix[j,])
  }
  sum_matrix = sum_matrix + rec_matrix
}
avgs = sum_matrix/num
guesses = ifelse(avgs < 0, -0.5, 0.5)

print(t)

