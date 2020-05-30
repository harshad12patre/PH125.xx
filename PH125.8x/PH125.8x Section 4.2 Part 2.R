library(dslabs)
library(data.table)
library(caret)

#01

data(mnist_27)

suppressWarnings(set.seed(1995, sample.kind="Rounding"))

indexes <- createResample(mnist_27$train$y, 10)

sum(indexes$Resample01 == 3)
sum(indexes$Resample01 == 4)
sum(indexes$Resample01 == 7)

#02

data(mnist_27)

suppressWarnings(set.seed(1995, sample.kind="Rounding"))

indexes <- createResample(mnist_27$train$y, 10)

summm <- 0
for (i in seq(01, 10, 1)) {
  df <- indexes[[i]]
  summm <- summm + sum(df == 3)
}
summm

#03

suppressWarnings(set.seed(1, sample.kind="Rounding"))

B <- 10^4

est <- replicate(B, {
  y <- rnorm(100, 0, 1)
  quantile(y, 0.75)
})

mean(est)
sd(est)

#04

suppressWarnings(set.seed(1, sample.kind="Rounding"))
ya <- rnorm(100, 0, 1)
suppressWarnings(set.seed(1, sample.kind="Rounding"))
ba <- createResample(ya, 10)

qa <- NA
for (i in seq(01, 10, 1)) {
  da <- ba[[i]]
  yaa <- ya[da]
  qa <- c(qa, quantile(yaa, 0.75))
  qa <- qa[!is.na(qa)]
}

mean(qa)
sd(qa)

#05

suppressWarnings(set.seed(1, sample.kind="Rounding"))
yff <- rnorm(100, 0, 1)
suppressWarnings(set.seed(1, sample.kind="Rounding"))
bf <- createResample(yff, 10000)

qf <- NA
for (i in seq(01, 10000, 1)) {
  df <- bf[[i]]
  yf <- yff[df]
  qf <- c(qf, quantile(yf, 0.75))
  qf <- qf[!is.na(qf)]
}
mean(qf)
sd(qf)