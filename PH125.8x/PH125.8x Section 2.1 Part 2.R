library(dslabs)
library(caret)
library(dplyr)
library(purrr)
data(iris)

suppressWarnings(set.seed(2, sample.kind="Rounding"))

iris <- iris[-which(iris$Species=="setosa"),]
y <- iris$Species

#7

test_index <- suppressWarnings(createDataPartition(y,times=1,p=0.5,list=F))
test <- iris[test_index,]
train <- iris[-test_index,]

#8

cols <- ncol(train) - 1 #(-1 because last column is factor)
x <- 1:cols
fcol <- 5

trainfn <- function(x) {
  range <- seq(min(train[,x]),max(train[,x]),by=0.1)
  accuracy <- sapply(range,function(i){
        y_hat <- ifelse(train[,x]>i,"virginica","versicolor")
        mean(y_hat==train[,fcol])
  })
  max(accuracy)
}
oaccuracy_train <- sapply(x,trainfn)

#9

max_col <- which.max(oaccuracy_train)
range_max_acc_col <- seq(min(train[max_col]),max(train[max_col]),by=0.1)

accuracy <- sapply(range_max_acc_col,function(i) {
  y_hat <- y_hat <- ifelse(train[,x]>i,"virginica","versicolor")
  mean(y_hat==train[,fcol])
})

cutoff_train <- range_max_acc_col[which.max(accuracy)]

oacc_test <- ifelse(test[,max_col]>cutoff,"virginica","versicolor")
mean(oacc_test==test[,fcol])

#10

cols <- ncol(train) - 1 #(-1 because last column is factor)
x <- 1:cols
fcol <- 5

testfn <- function(x) {
  range <- seq(min(test[,x]),max(test[,x]),by=0.1)
  accuracy <- sapply(range,function(i){
    y_hat <- ifelse(test[,x]>i,"virginica","versicolor")
    mean(y_hat==test[,fcol])
  })
  max(accuracy)
}
oaccuracy_test <- sapply(x,testfn)
oaccuracy_test

#11

max_col_test <- which.max(oaccuracy_test)
range_max_acc_col_test <- seq(min(train[max_col_test]),max(train[max_col_test]),by=0.1)

accuracy <- sapply(range_max_acc_col_test,function(i) {
  y_hat <- y_hat <- ifelse(test[,x]>i | test[,x]>i,"virginica","versicolor")
  mean(y_hat==train[,fcol])
})

cutoff_test <- range_max_acc_col_test[which.max(accuracy)]

oacc_test <- ifelse(test[,max_col]>cutoff_train | test[,max_col_test]>cutoff_test,"virginica","versicolor")
mean(oacc_test==test[,fcol])