#06

pca <- summary(prcomp(x_scaled))
sum(pca$importance[2, 1:7])

#07
