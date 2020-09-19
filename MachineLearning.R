ECHO=TRUE

library(e1071)
library(caret)
library(fscaret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(rattle)
library(corrplot)

url_train <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
url_test <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(url_train, destfile = "pml-training.csv")
download.file(url_test, destfile = "pml-testing.csv")

#locate downloaded dataset
testing<-read.csv(file="pml-testing.csv",head=TRUE,sep=",")
training<-read.csv(file="pml-training.csv",head=TRUE,sep=",")

set.seed(333)
##  data split and selecting features
inTrain <- createDataPartition(y=training$classe, p=0.7, list=F)
training1 <- training[inTrain, ]
training2 <- training[-inTrain, ]

#removing near-zero variance predictors
nzv <- nearZeroVar(training)
training1 <- training1[, -nzv]
training2 <- training2[, -nzv]
#removing predictors with NA values
training1 <- training1[, colSums(is.na(training1)) == 0]
training2 <- training2[, colSums(is.na(training2)) == 0]
#removing columns unfit for prediction (ID, user_name, raw_timestamp_part_1 etc ...)
training1 <- training1[, -(1:5)]
training2 <- training2[, -(1:5)]

## Selecting model 
mod1 <- train(classe ~., method = "rf", data = training1, verbose = TRUE, trControl = trainControl(method="cv"), number = 3)
pred1 <- predict(mod1, training1)
confusionMatrix(pred1, training1$classe)

pred12 <- predict(mod1, training2)
confusionMatrix(pred12, training2$classe)

## test the model 
testing <- testing[, colSums(is.na(testing)) == 0]
testing <- testing[, -(1:5)]
nzvt <- nearZeroVar(testing)
testing <- testing[, -nzvt]

pred13 <- predict(mod1, testing)
pred13
