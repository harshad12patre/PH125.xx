library(dslabs)
library(tidyverse)
library(Lahman)
library(ggplot2)
data("Teams")

data <- Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G,W_per_game = W/G, E_per_game = E/G,X2B_per_game = X2B/G, X3B_per_game = X3B/G)

cor(data$AB_per_game,data$R_per_game)
cor(data$W_per_game,data$E_per_game)
cor(data$X2B_per_game,data$X3B_per_game)