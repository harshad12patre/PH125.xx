library(dslabs)
data(tissue_gene_expression)

dim(tissue_gene_expression$x)
table(tissue_gene_expression$y)

d <- dist(tissue_gene_expression$x)

sqrt(crossprod(tissue_gene_expression$x[1] - tissue_gene_expression$x[2]))
sqrt(crossprod(tissue_gene_expression$x[39] - tissue_gene_expression$x[40]))
sqrt(crossprod(tissue_gene_expression$x[73] - tissue_gene_expression$x[74]))

image(as.matrix(d))