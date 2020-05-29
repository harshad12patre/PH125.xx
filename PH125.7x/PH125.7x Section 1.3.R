set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

mean_m <- mean(female_heights$mother)
mean_d <- mean(female_heights$daughter)

sd_m <- sd(female_heights$mother)
sd_d <- sd(female_heights$daughter)

cor <- cor(female_heights$mother,female_heights$daughter)

slope_d_mgiven <- cor * sd_d/sd_m
int_d_mgiven <- mean_d - (slope_d_mgiven*mean_m)

var <- cor*cor*100

height_m <- 60
mean_d_mheight <- mean_d + cor * ((height_m-mean_m)/sd_m) * sd_d 