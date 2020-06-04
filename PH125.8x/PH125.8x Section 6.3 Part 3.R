library(dslabs)
library(tidyverse)
library(RColorBrewer)

data("tissue_gene_expression")

#01

d <- dist(tissue_gene_expression$x - rowMeans(tissue_gene_expression$x))

#02

plot(hclust(d), cex = 0.5)

#03

km <- kmeans(d, centers = 7)
clusters <- km$cluster
split(names(clusters), clusters)

k <- kmeans(d, centers = 7)
groups <- k$cluster
split(names(groups), groups)

k <- kmeans(d, centers = 7)
groups <- k$cluster
split(names(groups), groups)

k <- kmeans(d, centers = 7)
groups <- k$cluster
split(names(groups), groups)

k <- kmeans(d, centers = 7)
groups <- k$cluster
split(names(groups), groups)

#04

sds <- matrixStats::colSds(tissue_gene_expression$x)
ind <- order(sds, decreasing = TRUE)[1:50]
colors <- brewer.pal(7, "Dark2")[as.numeric(tissue_gene_expression$y)]
heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = colors)