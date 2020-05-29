library(dslabs)
library(caret)
library(tidyverse)

data("tissue_gene_expression")

suppressWarnings(set.seed(1993, sample.kind="Rounding"))
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

#0102

tlda <- train(x, y, data = y, method = "lda")
tlda

tlda$finalModel

t(tlda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()

#0304

tqda <- train(x, y, data = y, method = "qda")
tqda

tqda$finalModel

t(tqda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()

#05

tt <- train(x, y, data = y, method = "lda", preProcess = "center")
tt$finalModel

t(tt$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()

#06

ya <- tissue_gene_expression$y
xa <- tissue_gene_expression$x
xa <- xa[, sample(ncol(xa), 10)]

tldaa <- train(xa, ya, method = "lda")
tldaa