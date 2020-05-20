set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

fit <- lm(mother ~ daughter, data=female_heights)
x <- predict(fit,se.fit = TRUE)

library(Lahman)

bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

bat_9901 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  group_by(playerID) %>%
  summarize(mean_singles = mean(singles), mean_bb = mean(bb)) %>%
  select(playerID, mean_singles, mean_bb)

nrow(filter(bat_9901,bat_9901$mean_singles>0.2))
nrow(filter(bat_9901,bat_9901$mean_bb>0.2))

j <- inner_join(bat_02,bat_9901)

cor(j$singles,j$mean_singles)
cor(j$bb,j$mean_bb)

j %>% ggplot(aes(singles, mean_singles)) + 
  geom_point()
j %>% ggplot(aes(bb, mean_bb)) + 
  geom_point()

lm(singles ~ mean_singles,data=j)
lm(bb ~ mean_bb,data=j)