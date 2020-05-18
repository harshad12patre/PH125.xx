library(HistData)
data("GaltonFamilies")

data <- GaltonFamilies %>%
  filter(gender=="male") %>%
  select(father,childHeight) %>%
  rename(son = childHeight)

lm(son ~ father, data = data)

galton_heights <- data %>%
  mutate(father_centered=father - mean(father))

lm(son ~ father_centered, data = galton_heights)

#sample for branches