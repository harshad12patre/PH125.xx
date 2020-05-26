library(tidyverse)
library(dslabs)
library(tidyr)

data <- read_mnist()

dat <- data$train

dat <- dat[["images"]]

dat1 <- as.matrix(dat[dat > 50 & dat < 205])


elements_dat <- ncol(dat) * nrow(dat)
elements_dat1 <- ncol(dat1) * nrow(dat1)

elements_dat1/elements_dat