library(titanic)    
library(caret)
library(tidyverse)
library(rpart)

options(digits = 3)

titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age),
         FamilySize = SibSp + Parch + 1) %>%
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

suppressWarnings(set.seed(42, sample.kind = "Rounding"))

testindex <- createDataPartition(titanic_clean$Survived, times = 1, p = 0.2, list = FALSE)
test_set <- titanic_clean[testindex,]
train_set <- titanic_clean[-testindex,]

#07

suppressWarnings(set.seed(1, sample.kind = "Rounding"))
train_lda <- train(Survived ~ Fare, method = "lda", data = train_set)
confusionMatrix(predict(train_lda, test_set), test_set$Survived)$overall[["Accuracy"]]

suppressWarnings(set.seed(1, sample.kind = "Rounding"))
train_qda <- train(Survived ~ Fare, method = "qda", data = train_set)
confusionMatrix(predict(train_qda, test_set), test_set$Survived)$overall[["Accuracy"]]

#08

suppressWarnings(set.seed(1, sample.kind = "Rounding"))
train_glm <- train(Survived ~ Age, method = "glm", data = train_set)
confusionMatrix(predict(train_glm, test_set), test_set$Survived)$overall[["Accuracy"]]

suppressWarnings(set.seed(1, sample.kind = "Rounding"))
train_glm_4 <- train(Survived ~ Sex + Pclass + Fare + Age, method = "glm", data = train_set)
confusionMatrix(predict(train_glm_4, test_set), test_set$Survived)$overall[["Accuracy"]]

suppressWarnings(set.seed(1, sample.kind = "Rounding"))
train_glm_all <- train(Survived ~ ., method = "glm", data = train_set)
confusionMatrix(predict(train_glm_all, test_set), test_set$Survived)$overall[["Accuracy"]]

#09ab

suppressWarnings(set.seed(6, sample.kind = "Rounding"))
k_knn = seq(3, 51, 2)
train_knn <- train(Survived ~ ., method = "knn", tuneGrid = data.frame(k = k_knn), data = train_set)

train_knn$bestTune

#09c

confusionMatrix(predict(train_knn, test_set), test_set$Survived)$overall[["Accuracy"]]

#10

suppressWarnings(set.seed(8, sample.kind = "Rounding"))
control_knn10 <- trainControl(method = "cv", number = 10, p = 0.9)
train_knn10 <- train(Survived ~ ., method = "knn", tuneGrid = data.frame(k = seq(3, 51, 2)), trControl = control_knn10, data = train_set)

train_knn10$bestTune
confusionMatrix(predict(train_knn10, test_set), test_set$Survived)$overall[["Accuracy"]]

#11a

suppressWarnings(set.seed(10, sample.kind = "Rounding"))
train_rpart <- train(Survived ~ ., method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)), data = train_set)

train_rpart$bestTune
confusionMatrix(predict(train_rpart, test_set), test_set$Survived)$overall[["Accuracy"]]

#11bc

plot(train_rpart[["finalModel"]], margin = 0.1)
text(train_rpart[["finalModel"]], cex = 0.75)

# left branch means true, 0 means not survived

#12

suppressWarnings(set.seed(14, sample.kind = "Rounding"))
train_rf <- train(Survived ~ ., method = "rf", tuneGrid = data.frame(mtry = seq(1, 7, 1)), data = train_set, ntree = 100)

train_rf$bestTune
confusionMatrix(predict(train_rf, test_set), test_set$Survived)$overall[["Accuracy"]]

imp <- varImp(train_rf)
imp$importance