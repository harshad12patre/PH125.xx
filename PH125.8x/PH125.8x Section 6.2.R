library(tidyverse)
library(ggplot2)
library(lubridate)
library(dslabs)

data("movielens")

#01

rcount_year <- movielens %>%
  group_by(year) %>%
  summarize(rcount = n()) %>%
  ggplot(aes(year, sqrt(rcount), label = year)) +
  geom_line() +
  geom_text(cex = 2.5)

#02

ravg <- movielens %>%
  group_by(title, year) %>%
  filter(year > 1993) %>%
  summarize(avg = mean(rating), rate_n = n()/(2018 - 1994))

ravg[which(ravg$title == "Shawshank Redemption, The"),]$avg
ravg[which(ravg$title == "Forrest Gump"),]$rate_n

#03

movielens %>%
  group_by(title, year) %>%
  filter(year > 1993) %>%
  summarize(avg = mean(rating), rate_n = n()/(2018 - 1994)) %>% 
  ggplot(aes(rate_n, avg)) + 
  geom_point() + 
  geom_smooth()

#05

movielens <- mutate(movielens, date = as_datetime(timestamp))

#06

movielens %>% 
  mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarize(avg = mean(rating)) %>%
  ggplot(aes(date, avg)) +
  geom_point() +
  geom_smooth()

#08

movielens %>% 
  group_by(genres) %>% 
  filter(length(rating) >= 1000) %>%
  summarize(avg = mean(rating), sd = sd(rating)) %>%
  filter(avg < 3.5) %>%
  ggplot(aes(x=genres, ymin=avg-sd, ymax=avg+sd)) + 
  geom_errorbar()