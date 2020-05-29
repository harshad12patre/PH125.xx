library(dslabs)
library(caret)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

inclass <- dat %>% filter(type=="inclass")
online <- dat %>% filter(type=="online")

inclass %>% filter(sex=="Female") %>% nrow()/nrow(inclass)
online %>% filter(sex=="Female") %>% nrow()/nrow(online)

y_hat <- ifelse(x == 'inclass', "Female", "Male") %>% factor()
mean(y == y_hat)

table(y, y_hat) 

sensitivity(y_hat, factor(dat$sex))
specificity(y_hat, factor(dat$sex))
confusionMatrix(y_hat, factor(dat$sex))