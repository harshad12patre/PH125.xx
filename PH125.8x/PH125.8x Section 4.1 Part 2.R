library(dslabs)
library(caret)

suppressWarnings(set.seed(1, sample.kind = "Rounding"))

#01

data("heights")

testindex <- createDataPartition(heights$sex, times = 1, p = 0.5, list = FALSE)

test_set <- heights[testindex,]
train_set <- heights[-testindex,]

ks <- seq(1, 101, 3)

f1 <- sapply(ks, function(k){
  fit <- knn3(sex ~ height, data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class") %>% factor(levels = c("Female", "Male"))
  F_meas(data = y_hat, reference = test_set$sex)
})

plot(ks,f1)

max(f1)
ks[which.max(f1)]

suppressWarnings(set.seed(1, sample.kind = "Rounding"))

#02

data("tissue_gene_expression")

datag <- data.frame(x = tissue_gene_expression$x, y = tissue_gene_expression$y)

testindexg <- createDataPartition(tissue_gene_expression$y, times = 1, p = 0.5, list = FALSE)

test_setg <- datag[testindexg,]
train_setg <- datag[-testindexg,]

ksg <- seq(1, 13, 2)

f1g <- sapply(ksg, function(k){
  fit <- knn3(y ~ ., data=train_setg, k=k)
  y_hat <- predict(fit, test_setg, type = "class") %>% factor(levels = levels(train_setg$y))
  mean(y_hat == test_setg$y)
})

f1g