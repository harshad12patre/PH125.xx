library(tidyverse)
library(DescTools)

edx <- readRDS("~/r-projects/PH125.xx/PH125.9x/edx.rds")
validation <- readRDS("~/r-projects/PH125.xx/PH125.9x/validation.rds")

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

nrow(filter(edx, genres %like% "%Drama%"))
nrow(filter(edx, genres %like% "%Comedy%"))
nrow(filter(edx, genres %like% "%Thriller%"))
nrow(filter(edx, genres %like% "%Romance%"))

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