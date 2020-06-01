library(dslabs)
library(caret)
library(rpart)
library(dplyr)

dat <- as.data.frame(tissue_gene_expression)
y <- dat$y

suppressWarnings(set.seed(1991, sample.kind = "Rounding"))

#01

train <- train(y ~ ., method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)), data = dat)
train$results$cp[which.max(train$results$Accuracy)]

#02

traina <- train(y ~ ., method = "rpart", control = rpart.control(minsplit = 0), tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)), data = dat)
max(traina$results["Accuracy"])

#03

fita <- rpart(y ~ ., control = rpart.control(minsplit = 0), data = dat)
plot(fita, margin = 0.1)
text(fita, cex = 0.75)

#04

fit <- train(y ~ ., method = "rf", tuneGrid = data.frame(mtry = seq(50, 200, 25)), nodesize = 1, data = dat)
fit$results$mtry[which.max(fit$results$Accuracy)]

#05

imp <- varImp(fit)

#06

tree_terms <- as.character(unique(traina$finalModel$frame$var[!(traina$finalModel$frame$var == "<leaf>")]))
tree_terms

imp$importance["x.CFHR4",]
rank(tree_terms == "x.CFHR4")[4]