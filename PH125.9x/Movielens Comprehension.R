library(tidyverse)
library(DescTools)

edx <- readRDS("D:/r-projects/edx.rds")
validation <- readRDS("D:/r-projects/validation.rds")

#01

nrow(edx)
ncol(edx)

#02

sum(edx$rating == 0)
sum(edx$rating == 3)

#03

length(unique(edx$movieId))

#04

length(unique(edx$userId))

#05

edx %>% filter(genres %like% "Drama") %>% summarize(n())
edx %>% filter(genres %like% "Comedy") %>% summarize(n())
edx %>% filter(genres %like% "Thriller") %>% summarize(n())
edx %>% filter(genres %like% "Romance") %>% summarize(n())

#06

edx %>%
  group_by(title) %>%
  summarize(nr = n()) %>%
  arrange(desc(nr)) %>%
  top_n(1)

#07

edx %>%
  group_by(rating) %>%
  summarize(nr = n()) %>%
  arrange(desc(nr)) %>%
  top_n(5)

#08

rat <- edx %>%
  group_by(rating) %>%
  summarize(nr = n())

sum(rat$nr[seq(2, 10, 2)]) > sum(rat$nr[seq(1, 9, 2)])

# odd indexes are half-star ratings and even indexes are full-star ratings

rm(rat)