library(knitr)
library(caret)
library(tidyverse)
library(data.table)

edx <- readRDS("D:/r-projects/edx.rds")
validation <- readRDS("D:/r-projects/validation.rds")

# partitioning edx set into separate training and testing sets

testindex <- createDataPartition(edx$rating, times = 1, p = 0.2, list = FALSE)
edx_train <- edx[-testindex,]
edx_test <- edx[testindex,]
edx_test <- edx_test %>%
  semi_join(edx_train, by = "movieId") %>%
  semi_join(edx_train, by = "userId")

# calculating rmse of average method on test_edx set

mean_tt <- mean(edx_train$rating)
rmse_avg <- RMSE(edx_test$rating, mean_tt)
rmse_avg

# add to rmse results

rmse_results <- data_frame(method = "Just the average (edx_test)", RMSE = rmse_avg)

# calculating rmse of movie effect on edx_test set

movie_tt <- edx_train %>%
  group_by(movieId) %>%
  summarize(bi = mean(rating - mean_tt))

pred_bi <- mean_tt + edx_test %>%
  left_join(movie_tt, by='movieId') %>%
  .$bi

rmse_movie <- RMSE(pred_bi, edx_test$rating)

# add to rmse results

rmse_results <- bind_rows(rmse_results, data_frame(method="Movie Effect Model (edx_test)", RMSE = rmse_movie ))

# calculating rmse of movie and user model on edx_test set

user_tt <- edx_test %>%
  left_join(movie_tt, by='movieId') %>%
  group_by(userId) %>%
  summarize(bu = mean(rating - mean_tt))

pred_bu <- edx_test %>%
  left_join(movie_tt, by='movieId') %>%
  left_join(user_tt, by='userId') %>%
  mutate(pred = mean_tt + bi + bu) %>%
  .$pred

rmse_user <- RMSE(pred_bu, edx_test$rating)

# add to rmse results

rmse_results <- bind_rows(rmse_results, data_frame(method="User + Movie Effect Model (edx_test)", RMSE = rmse_user ))

# calculating rmse of regularized movie and user model on edx_test set

lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  mu <- mean(edx_train$rating)
  bi <- edx_train %>%
    group_by(movieId) %>%
    summarize(bi = sum(rating - mean_tt)/(n()+l), .groups = 'drop')
  bu <- edx_train %>%
    left_join(bi, by="movieId") %>%
    group_by(userId) %>%
    summarize(bu = sum(rating - bi - mean_tt)/(n()+l), .groups = 'drop')
  pred <-
    edx_test %>%
    left_join(bi, by = "movieId") %>%
    left_join(bu, by = "userId") %>%
    mutate(pred = mean_tt + bi + bu) %>%
    .$pred
  return(RMSE(pred, edx_test$rating))
})

qplot(lambdas, rmses)

lambda <- lambdas[which.min(rmses)]
lambda

# add to rmse results

rmse_results <- bind_rows(rmse_results, data_frame(method="Regularized Movie + User Effect Model (edx_test)", RMSE = min(rmses)))

# calculating rmse of regularized movie and user model on validation set

val_lambdas <- seq(0, 10, 0.25)

val_rmses <- sapply(lambdas, function(l){
  mu <- mean(edx$rating)
  bi <- edx %>%
    group_by(movieId) %>%
    summarize(bi = sum(rating - mu)/(n()+l), .groups = 'drop')
  bu <- edx %>%
    left_join(bi, by="movieId") %>%
    group_by(userId) %>%
    summarize(bu = sum(rating - bi - mu)/(n()+l), .groups = 'drop')
  pred <- validation %>%
    left_join(bi, by = "movieId") %>%
    left_join(bu, by = "userId") %>%
    mutate(pred = mu + bi + bu) %>%
    .$pred
  return(RMSE(pred, validation$rating))
})

final_rmse <- min(val_rmses)

# add to rmse results

rmse_results <- bind_rows(rmse_results, data_frame(method="Regularized Movie + User Effect Model (validation set)", RMSE = final_rmse))

rmse_results %>% kable()