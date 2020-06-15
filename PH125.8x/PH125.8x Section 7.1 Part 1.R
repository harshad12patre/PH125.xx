options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
data(brca)

#01

dim(brca$x)
mean(brca$y == "M")
which.max(colMeans(brca$x))
which.min(colSds(brca$x))

#02

x_centered <- sweep(brca$x, 2, colMeans(brca$x))
x_scaled <- sweep(x_centered, 2, colSds(brca$x), FUN = "/")
sd(x_scaled[,1])
median(x_scaled[,1])

#03

ind_B <- which(brca$y == "B")
ind_M <- which(brca$y == "M")
distances <- as.matrix(dist(x_scaled))
mean(distances[1, ind_B])
mean(distances[1, ind_M])

#04

heatmap(as.matrix(dist(t(x_scaled))), labRow = NA, labCol = NA)

#05

a <- hclust(dist(t(x_scaled)))
a <- cutree(a, 5)
split(names(a), a)