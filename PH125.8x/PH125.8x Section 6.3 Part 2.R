library(tidyverse)

suppressWarnings(set.seed(1987, sample.kind="Rounding"))

n <- 100
k <- 8
Sigma <- 64  * matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1), 3, 3) 
m <- MASS::mvrnorm(n, rep(0, 3), Sigma)
m <- m[order(rowMeans(m), decreasing = TRUE),]
y <- m %x% matrix(rep(1, k), nrow = 1) + matrix(rnorm(matrix(n*k*3)), n, k*3)
colnames(y) <- c(paste(rep("Math",k), 1:k, sep="_"),
                 paste(rep("Science",k), 1:k, sep="_"),
                 paste(rep("Arts",k), 1:k, sep="_"))

#01

my_image <- function(x, zlim = range(x), ...){
  colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
        xlab="", ylab="",  col = colors, zlim = zlim, ...)
  abline(h=rows + 0.5, v = cols + 0.5)
  axis(side = 1, cols, colnames(x), las = 2)
}

my_image(y)

#02

my_image(cor(y), zlim = c(-1,1))
range(cor(y))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

#03

s <- svd(y)
names(s)

y_svd <- s$u %*% diag(s$d) %*% t(s$v)
max(abs(y - y_svd))

ss_y <- apply(y^2, 2, sum)
ss_yv <- apply((y %*% s$v)^2, 2, sum)

sum(ss_y)
sum(ss_yv)

#04

plot(seq(1,ncol(y)), ss_y)
plot(seq(1,ncol(y)), ss_yv)

#05

plot(sqrt(ss_yv), s$d)

#06

sum(ss_yv[1:3])/sum(ss_yv)

#07

identical(s$u %*% diag(s$d), sweep(s$u, 2, s$d, FUN = "*"))

#08

avg_scores <- rowMeans(y)
UD <- sweep(s$u, 2, s$d, FUN = "*")

plot(UD[,1], avg_scores)

#09

my_image(s$v)
dim(s$v)

#10

plot(s$u[,1], ylim = c(-0.25, 0.25))
plot(s$v[,1], ylim = c(-0.25, 0.25))

my_image((s$u[,1] * s$d[1]) %*% t(s$v[,1]))
my_image(y)

#11

resida <- y - with(s,(u[, 1, drop=FALSE]*d[1]) %*% t(v[, 1, drop=FALSE]))
my_image(cor(resida), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

plot(s$u[,2], ylim = c(-0.25, 0.25))
plot(s$v[,2], ylim = c(-0.25, 0.25))

my_image((s$u[,2] * s$d[2]) %*% t(s$v[,2]))
my_image(resida)

#12

sum(s$d[1:2]^2)/sum(s$d^2) * 100

residb <- y - with(s,sweep(u[, 1:2], 2, d[1:2], FUN="*") %*% t(v[, 1:2]))
my_image(cor(residb), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

plot(s$u[,3], ylim = c(-0.25, 0.25))
plot(s$v[,3], ylim = c(-0.25, 0.25))

my_image((s$u[,3] * s$d[3]) %*% t(s$v[,3]))
my_image(residb)

#13

sum(s$d[1:3]^2)/sum(s$d^2) * 100

residc <- y - with(s,sweep(u[, 1:3], 2, d[1:3], FUN="*") %*% t(v[, 1:3]))
my_image(cor(residc), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

my_image(y, zlim = c(-1,1))
my_image((s$u[,1] * s$d[1]) %*% t(s$v[,1])+(s$u[,2] * s$d[2]) %*% t(s$v[,2])+(s$u[,3] * s$d[3]) %*% t(s$v[,3]), zlim = c(-1,1))
my_image(resida + residb + residc, zlim = c(-1,1))