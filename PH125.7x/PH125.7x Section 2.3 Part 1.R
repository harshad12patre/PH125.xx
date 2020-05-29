library(tidyverse)
library(broom)
library(Lahman)

dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = HR/G,
         R = R/G) %>%
  select(lgID, HR, BB, R) 

#op1

dat %>% 
  group_by(lgID) %>% 
  do(tidy(lm(R ~ HR, data = .), conf.int = T)) %>% 
  filter(term == "HR") 

#op2

dat %>% 
  group_by(lgID) %>% 
  do(glance(lm(R ~ HR, data = .)))

#op3

dat %>% 
  do(tidy(lm(R ~ HR, data = .), conf.int = T)) %>% 
  filter(term == "HR")

#op4

dat %>% 
  group_by(lgID) %>% 
  do(mod = lm(R ~ HR, data = .))