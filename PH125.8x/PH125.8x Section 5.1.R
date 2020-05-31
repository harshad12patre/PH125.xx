library(ggplot2)
library(dplyr)
library(rpart)
library(randomForest)

n <- 1000
sigma <- 0.25

suppressWarnings(set.seed(1, sample.kind = "Rounding"))

x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)

dat <- data.frame(x = x, y = y)

#010203

fit <- rpart(y ~ ., data = dat)

plot(fit, margin = 0.1)
text(fit, cex = 0.75)

dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col=2)

#0405

fitrf <- randomForest(y ~ x, data = dat)

dat %>% 
  mutate(y_hat = predict(fitrf)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")

plot(fitrf)

#06

fitrff <- randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)
  
dat %>% 
  mutate(y_hat = predict(fitrff)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")