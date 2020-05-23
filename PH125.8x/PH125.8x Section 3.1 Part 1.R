library(tidyverse)
library(dslabs)
library(caret)

#01

# suppressWarnings(set.seed(1, sample.kind="Rounding"))
# n <- 100
# Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
# dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
#   data.frame() %>% setNames(c("x", "y"))
# 
# suppressWarnings(set.seed(1, sample.kind="Rounding"))
# dataaa <- replicate(100, {
#   testindices <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
#   testdata <- dat[testindices,]
#   traindata <- dat[-testindices,]
#   model <- lm(y ~ x, data = traindata)
#   Y_hat <- predict.lm(model, testdata)
#   RMSE(testdata$y,Y_hat)
# })
# 
# mean(dataaa)
# sd(dataaa)

#02_03

# build <- function(n) {
#   
#   Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
#   dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
#     data.frame() %>% setNames(c("x", "y"))
#   
#   dataaa <- replicate(100, {
#     testindices <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
#     testdata <- dat[testindices,]
#     traindata <- dat[-testindices,]
#     model <- lm(y ~ x, data = traindata)
#     Y_hat <- predict.lm(model, testdata)
#     RMSE(testdata$y,Y_hat)
#   })
# 
#   list (mean(dataaa),sd(dataaa))
# }
# 
# suppressWarnings(set.seed(1, sample.kind="Rounding"))
# n <- c(100, 500, 1000, 5000, 10000)
# ans <- sapply(n,build)
# ans

#04_05

# suppressWarnings(set.seed(1, sample.kind="Rounding"))
# n <- 100
# Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
# dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
#   data.frame() %>% setNames(c("x", "y"))
# 
# suppressWarnings(set.seed(1, sample.kind="Rounding"))
# dataaa <- replicate(100, {
#   testindices <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
#   testdata <- dat[testindices,]
#   traindata <- dat[-testindices,]
#   model <- lm(y ~ x, data = traindata)
#   Y_hat <- predict.lm(model, testdata)
#   RMSE(testdata$y,Y_hat)
# })
# 
# mean(dataaa)
# sd(dataaa)

#06_07

# suppressWarnings(set.seed(1, sample.kind="Rounding"))
# Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
# dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
#   data.frame() %>% setNames(c("y", "x_1", "x_2"))
# 
# suppressWarnings(set.seed(1, sample.kind="Rounding"))
# dataaa <- replicate(1, {
#   testindices <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
#   testdata <- dat[testindices,]
#   traindata <- dat[-testindices,]
#   model_x1 <- lm(y ~ x_1, data = traindata)
#   model_x2 <- lm(y ~ x_2, data = traindata)
#   model_x1x2 <- lm(y ~ x_1 + x_2, data = traindata)
#   Y_hat_x1 <- predict.lm(model_x1, testdata)
#   Y_hat_x2 <- predict.lm(model_x2, testdata)
#   Y_hat_x1x2 <- predict.lm(model_x1x2, testdata)
#   list(RMSE(testdata$y,Y_hat_x1), RMSE(testdata$y,Y_hat_x2), RMSE(testdata$y,Y_hat_x1x2))
# })
# 
# dataaa

#08

suppressWarnings(set.seed(1, sample.kind="Rounding"))
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

suppressWarnings(set.seed(1, sample.kind="Rounding"))
dataaa <- replicate(1, {
  testindices <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  testdata <- dat[testindices,]
  traindata <- dat[-testindices,]
  model_x1 <- lm(y ~ x_1, data = traindata)
  model_x2 <- lm(y ~ x_2, data = traindata)
  model_x1x2 <- lm(y ~ x_1 + x_2, data = traindata)
  Y_hat_x1 <- predict.lm(model_x1, testdata)
  Y_hat_x2 <- predict.lm(model_x2, testdata)
  Y_hat_x1x2 <- predict.lm(model_x1x2, testdata)
  list(RMSE(testdata$y,Y_hat_x1), RMSE(testdata$y,Y_hat_x2), RMSE(testdata$y,Y_hat_x1x2))
})

dataaa