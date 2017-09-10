library(tidyverse)
library(caret)

# Source of data: http://groupware.les.inf.puc-rio.br/har.  Big thanks to them for making the data available.

# Load packages and set seed for reproducibility
library(tidyverse)
library(caret)
library(randomForest)
library(ggplot2)
set.seed(111)

# Loading data
training <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
testing <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")

# Data cleaning

## Getting rid of columns with NAs
n <- which(sapply(testing, function(x) all(is.na(x))))
train1 <- training[, -n]
test1 <- testing[, -n]

## Getting rid of non-predictor variables
train1 <- train1[, -c(1:7)]
test1 <- test1[, -c(1:7)]
### I am not sure columns 6 and 7 are predictors, but I am leaving them out just in case they aren't.

# Cross Validation
## Since we have a lot of data in the training set and only 20 for the test set, we can create validation sets
## within the training set to fine tune the model
folds <- createFolds(y = train1$classe, k = 3, list = FALSE)
train_fold1 <- train1[folds!=1, ]
test_fold1 <- train1[folds==1, ]
train_fold2 <- train1[folds!=2, ]
test_fold2 <- train1[folds==2, ]
train_fold3 <- train1[folds!=3, ]
test_fold3 <- train1[folds==3, ]

rf1 <- randomForest(x = train_fold1[,c(1:52)], y = train_fold1$classe, xtest = test_fold1[,c(1:52)], ytest = test_fold1$classe, ntree = 501)
rf2 <- randomForest(x = train_fold2[,c(1:52)], y = train_fold2$classe, xtest = test_fold2[,c(1:52)], ytest = test_fold2$classe, ntree = 501)
rf3 <- randomForest(x = train_fold3[,c(1:52)], y = train_fold3$classe, xtest = test_fold3[,c(1:52)], ytest = test_fold3$classe, ntree = 501)

train_fold1$predRight <- train_fold1$classe==rf1$predicted
test_fold1$predRight <- test_fold1$classe==rf1$test$predicted
oob_fold1 <- (length(train_fold1$predRight)-sum(train_fold1$predRight))/length(train_fold1$predRight)
oos_fold1 <- (length(test_fold1$predRight)-sum(test_fold1$predRight))/length(test_fold1$predRight)

train_fold2$predRight <- train_fold2$classe==rf2$predicted
test_fold2$predRight <- test_fold2$classe==rf2$test$predicted
oob_fold2 <- (length(train_fold2$predRight)-sum(train_fold2$predRight))/length(train_fold2$predRight)
oos_fold2 <- (length(test_fold2$predRight)-sum(test_fold2$predRight))/length(test_fold2$predRight)

train_fold3$predRight <- train_fold3$classe==rf3$predicted
test_fold3$predRight <- test_fold3$classe==rf3$test$predicted
oob_fold3 <- (length(train_fold3$predRight)-sum(train_fold3$predRight))/length(train_fold3$predRight)
oos_fold3 <- (length(test_fold3$predRight)-sum(test_fold3$predRight))/length(test_fold3$predRight)

print("Average OOB error rate:")
print(round(mean(c(oob_fold1, oob_fold2, oob_fold3)), 4))
print("Average Out of Sample error rate:")
print(round(mean(c(oos_fold1, oos_fold2, oos_fold3)), 4))

# Final Prediction

mod_rf <- randomForest(x = train1[,c(1:52)], y = train1$classe, xtest = test1[,c(1:52)], ytest = test1$classe, ntree = 501)
print("Final Predictions:")
mod_rf$test$predicted