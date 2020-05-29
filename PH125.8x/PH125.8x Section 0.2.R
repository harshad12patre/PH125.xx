library(dplyr)
library(dslabs)
data("heights")

class(heights)
class(heights$height)
class(heights$sex)
class("Male")
class(75.0000)

nrow(heights)

heights$height[777]

heights$sex[777]
heights[777,1]

max(heights$height)
heights[which.min(heights$height),]

mean(heights$height)
median(heights$height)

heights %>% filter(sex=="Male") %>% nrow()/nrow(heights)

heights %>% filter(height>78) %>% nrow()

heights %>% filter(sex=="Female" & height>78) %>% nrow()