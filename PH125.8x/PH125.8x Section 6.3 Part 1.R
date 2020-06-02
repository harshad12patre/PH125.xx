library(dplyr)
library(dslabs)
library(caret)

options(digits=7)

suppressWarnings(set.seed(1986, sample.kind="Rounding"))
n <- round(2^rnorm(1000, 8, 1))

suppressWarnings(set.seed(1, sample.kind="Rounding"))
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))

schools %>% top_n(10, quality) %>% arrange(desc(quality))

suppressWarnings(set.seed(1, sample.kind="Rounding"))
mu <- round(80 + 2*rt(1000, 5))
scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))

#01

arranged <- schools %>% 
  arrange(desc(schools$score))
arranged$id[1]
arranged$score[10]

#02

top10 <- arranged[1:10,]
median(schools$size)
median(top10$size)

#03

worst10 <- arranged %>%
  arrange(score)
worst10 <- worst10[1:10,]
median(worst10$size)

#04

ggplot() +
  geom_point(data = arranged, aes(size, score)) +
  geom_point(data = top10, aes(size, score), color = "red")

#05

overall <- mean(sapply(scores, mean))
alpha <- 25
top10reg <- schools %>%
  mutate(scorereg = overall + (score - overall) * size / (size + alpha)) %>%
  arrange(desc(scorereg))
top10reg %>%
  top_n(10, scorereg)

top10reg$id[1]
top10reg$scorereg[10]

#06

alphas <- seq(10, 250, 1)
RMSE <- function(alpha){
  temp <- schools %>%
    mutate(scorereg = overall + size*(score - overall)/(size + alpha))
  sqrt(mean((temp$quality - temp$scorereg)^2))
}

rmse <- sapply(alphas, RMSE)
plot(alphas, rmse)
alphas[which.min(rmse)]

#07

alphaa <- alphas[which.min(rmse)]
top10rega <- schools %>%
  mutate(scorereg = overall + (score - overall) * size / (size + alphaa)) %>%
  arrange(desc(scorereg))
top10rega %>%
  top_n(10, scorereg)

top10rega$id[1]
top10rega$scorereg[10]

#08

alphasb <- seq(10, 250, 1)
RMSEb <- function(alpha){
  temp <- schools %>%
    mutate(scorereg = overall/(size + alpha))
  sqrt(mean((temp$quality - temp$scorereg)^2))
}

rmseb <- sapply(alphasb, RMSEb)
plot(alphasb, rmseb)
alphasb[which.min(rmseb)]