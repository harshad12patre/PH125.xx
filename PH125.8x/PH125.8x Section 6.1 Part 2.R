library(dplyr)
library(ggplot2)
library(dslabs)

data("tissue_gene_expression")

data <- data.frame(tissue_gene_expression)[,-501]

#01

pca <- prcomp(data)
data.frame(pca$x[,1:2], tissue = tissue_gene_expression$y) %>%
  ggplot(aes(PC1, PC2, color = tissue))+
  geom_point()

#02

pcax <- data.frame(pca$x)
pcax %>%
  ggplot(aes(rowMeans(tissue_gene_expression$x), PC1, color = tissue_gene_expression$y))+
  geom_point()
cor(pcax$PC1, rowMeans(tissue_gene_expression$x))

#03

x <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x)))
data.frame(pc_1 = pca$x[,1], pc_2 = pca$x[,2],
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()

#04

pcax %>%
  ggplot(aes(pcax[,7],tissue_gene_expression$y)) +
  geom_boxplot()

#05

per_var <- t(summary(pca)$importance[2,])
sum(per_var[1:3])