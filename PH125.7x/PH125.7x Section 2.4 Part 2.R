library(Lahman)
library(broom)
library(ggplot2)

dat <- Teams %>%
  filter(yearID %in% 1961:2018) %>%
  group_by(yearID) %>%
  do(tidy(lm(R ~ BB + HR,data=.),conf.int=T)) %>%
  filter(term=="BB") %>%
  ggplot(aes(yearID,estimate)) +
  geom_point() +
  geom_smooth(method = "lm")

dat <- dat[[1]]

tidy(lm(estimate~yearID,data=dat),conf.int=T)