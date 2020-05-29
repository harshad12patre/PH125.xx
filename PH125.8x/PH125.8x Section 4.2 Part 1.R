library(dplyr)
library(caret)
library(genefilter)
library(dslabs)

suppressWarnings(set.seed(1996, sample.kind="Rounding")) #if you are using R 3.6 or later

n <- 1000
p <- 10000

x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")

y <- rbinom(n, 1, 0.5) %>% factor()

#01

x_subset <- x[ ,sample(p, 100)]

fit <- train(x_subset, y, method = "glm")
fit$results

#02

tt <- colttests(x, y)

pvals <- tt$p.value
pvals

#03

ind <- which(pvals<0.01)
length(ind)

#04

x_subseta <- x[, ind]

fita <- train(x_subseta, y, method = "glm")
fita$results

#05

fitb <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fitb)

#07

data("tissue_gene_expression")

xc <- tissue_gene_expression$x
yc <- tissue_gene_expression$y

fitc <- train(xc, yc, method = "knn", tuneGrid = data.frame(k = seq(1, 7, 2)))
ggplot(fitc)
