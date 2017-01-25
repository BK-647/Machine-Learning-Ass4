setwd("C:/Users/Azamat/Documents/R/Machine Learning 4")

#libraries
library(caret)
library(randomForest)
library(corrplot)

#load data
TestA = read.csv("pml-testing.csv", na.strings = c("NA", "#DIV/0!", ""))
TrainA = read.csv("pml-training.csv",na.strings = c("NA", "#DIV/0!", ""))

#remove non numeric columns 
TrainA <- TrainA[, -(1:6)]

#clean NAs or mostly NA columns
NAs    <- sapply(TrainA, function(x) mean(is.na(x))) > 0.95
TrainA <- TrainA[, NAs==FALSE]

#seperate train and test set
inTrain  <- createDataPartition(TrainA$classe, p=0.5, list=FALSE)
TrainSet <- TrainA[inTrain, ]
TestSet  <- TrainA[-inTrain, ]

#plot correlation ofvariables
corMatrix <- cor(TrainSet[, -54])
corrplot(corMatrix, order = "FPC", method = "square", type = "upper",
         add = FALSE, tl.cex = 0.5)

#build a prediction model
# model fit
set.seed(150)
controlRF <- trainControl(method="cv", number=3, verboseIter=FALSE)
modFit <- train(classe ~ ., data=TrainSet, method="rf", trControl=controlRF, prox = TRUE)
modFit$finalModel

#prediction - cross validation
predictRF <- predict(modFit, newdata=TestSet)
confMatRF <- confusionMatrix(predictRF, TestSet$classe)
confMatRF

#prediction
Testcases <- predict(modFit, newdata=TestA)
Testcases
