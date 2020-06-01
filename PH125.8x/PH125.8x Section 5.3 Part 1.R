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

#01

nrow(train_set)
nrow(test_set)
mean(train_set$Survived == 1)

#02

suppressWarnings(set.seed(3, sample.kind = "Rounding"))
mean(sample(c(0, 1), nrow(test_set), replace = TRUE) == test_set$Survived)

#03a

mean((train_set %>% filter(Sex == "male"))$Survived == 1)
mean((train_set %>% filter(Sex == "female"))$Survived == 1)

#03b

sex_model <- ifelse(test_set$Sex == "female", 1, 0)
mean(sex_model == test_set$Survived)

#04a

mean((titanic_clean %>% filter(Pclass == 1))$Survived == 1)
mean((titanic_clean %>% filter(Pclass == 2))$Survived == 1)
mean((titanic_clean %>% filter(Pclass == 3))$Survived == 1)

#04b

class_model <- ifelse(test_set$Pclass == 1, 1, 0)
mean(class_model == test_set$Survived)

#04c

mean((titanic_clean %>% group_by(Sex, Pclass) %>% filter(Sex == "male" & Pclass == 1))$Survived == 1)
mean((titanic_clean %>% group_by(Sex, Pclass) %>% filter(Sex == "male" & Pclass == 2))$Survived == 1)
mean((titanic_clean %>% group_by(Sex, Pclass) %>% filter(Sex == "male" & Pclass == 3))$Survived == 1)
mean((titanic_clean %>% group_by(Sex, Pclass) %>% filter(Sex == "female" & Pclass == 1))$Survived == 1)
mean((titanic_clean %>% group_by(Sex, Pclass) %>% filter(Sex == "female" & Pclass == 2))$Survived == 1)
mean((titanic_clean %>% group_by(Sex, Pclass) %>% filter(Sex == "female" & Pclass == 3))$Survived == 1)

#04d

sex_class_model <- ifelse((test_set$Sex == "female" & test_set$Pclass == 1)|(test_set$Sex == "female" & test_set$Pclass == 2), 1, 0)
mean(sex_class_model == test_set$Survived)

#05a

confusionMatrix(data = factor(sex_model), reference = factor(test_set$Survived))
confusionMatrix(data = factor(class_model), reference = factor(test_set$Survived))
confusionMatrix(data = factor(sex_class_model), reference = factor(test_set$Survived))

#05b

max(data.frame(confusionMatrix(data = factor(sex_model), reference = factor(test_set$Survived))[["byClass"]][["Balanced Accuracy"]], confusionMatrix(data = factor(class_model), reference = factor(test_set$Survived))[["byClass"]][["Balanced Accuracy"]], confusionMatrix(data = factor(sex_class_model), reference = factor(test_set$Survived))[["byClass"]][["Balanced Accuracy"]]))

#06

F_meas(factor(sex_model), reference = factor(test_set$Survived))
F_meas(factor(class_model), reference = factor(test_set$Survived))
F_meas(factor(sex_class_model), reference = factor(test_set$Survived))