

library(gdata)
library(ggplot2)
library(miscTools)
library(xlsx)
library(hash)
library(aod)
library(randomForest)
library(LiblineaR)
library(Matrix)
library(recommenderlab)
library(arules)



men <- read.csv('~/Desktop/sampe_matrix.csv')
drops = c("X", "Unnamed..0","iid", "wave", "id", "gender", "partner", "dec","dec_o")
men = men[,!(names(men) %in% drops)]
matrix = matrix(men[2,][1:100])
B = matrix(men[2,][1:100], nrow=10, ncol=10) 

# sorted = sort(sample(1:100, 10, replace=F))
# new_hash = hash()
# for(i in 1:10){
#   new_hash[i] <- c(sorted[i] %% 10, sorted[i] %/% 10)
# }
# first = matrix(values(new_hash), nrow = 2, ncol = 10)[1,]
# second = matrix(values(new_hash), nrow = 2, ncol = 10)[2,]
new_matrix = matrix(nrow = 10, ncol = 10)

for(i in 1:10){
  for(j in 1:10){
    if(sample(1:10, 1) != 1){
      new_matrix[[i,j]] = B[[i,j]]
    }
  }
}
new_matrix
b <- as(matrix, "binaryRatingMatrix")
matr = Matrix(happy)
matr