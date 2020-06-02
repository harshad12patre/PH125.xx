library(caret)
library(dslabs)
library(dplyr)

data("mnist_27")

suppressWarnings(set.seed(1, sample.kind = "Rounding"))

#01

models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")

fits <- lapply(models, function(model){
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
})

names(fits) <- models

#02

mat <- matrix(sapply(fits, function(fits){ predict(fits, mnist_27$test)}), length(mnist_27$test$y), length(models))

#03

accuracy <- data.frame(model = models)
acc <- NA
for (i in 1:10) {
  acc <- c(acc, mean(mat[,i] == mnist_27$test$y))
  acc <- acc[!is.na(acc)]
}
accuracy <- mutate(accuracy, acc = acc)
mean(accuracy$acc)

#04

majority <- apply(mat, 1, function(v) {
  uniq <- unique(v)
  uniq[which.max(tabulate(match(v, uniq)))]
})
ens_acc <- mean(ifelse(majority == mnist_27$test$y, 1, 0))
ens_acc

#05

sum(accuracy$acc > ens_acc)
accuracy$model[which(accuracy$acc > ens_acc)]

#06

acc_hat <- sapply(fits, function(fit) min(fit$results$Accuracy))
mean(acc_hat)

#07

more80index <- which(acc_hat >= 0.8)
mean(acc_hat[more80index])