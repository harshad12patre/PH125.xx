library(tidyverse)
library(HistData)
library(Lahman)

## question 1 

data("GaltonFamilies")
set.seed(1983,sample.kind = "Rounding")
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)
rss <- function(beta0, beta1, data){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}

beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)

## question 3

data("Teams")

class(data$yearID)

data <- Teams %>% filter(yearID %in% 1961:2001)

fit <- lm(R ~ BB + HR, data = data)
summary(fit)

#question 4

B <- 1000
N <- 100
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% .$coef 
})

lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 

plot(lse)

# question 6

#op1

galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth()

#op2


galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth(method = "lm")

#op3

model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as_tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = galton_heights, aes(x = father, y = son))

#op4

# model <- lm(son ~ father, data = galton_heights)
# predictions <- predict(model)
# data <- as_tibble(predictions) %>% bind_cols(father = galton_heights$father)
# 
# ggplot(data, aes(x = father, y = fit)) +
#   geom_line(color = "blue", size = 1) + 
#   geom_point(data = galton_heights, aes(x = father, y = son))