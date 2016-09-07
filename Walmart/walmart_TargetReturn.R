library("caret")
library("RWeka")
library("rminer")
library("psych")
library("rpart")
library("kernlab")
library("e1071")
library("neuralnet")
library("MASS")
setwd("/Users/Anqi/Documents/Study/2015 Fall/Data Mining/project")
datWB <- read.csv("Walmart_basket.csv")
datWB <- datWB[,c(-2,-4)]
datWB$Weekend[datWB$Weekday == "Monday"] <- 0
datWB$Weekend[datWB$Weekday == "Tuesday"] <- 0
datWB$Weekend[datWB$Weekday == "Wednesday"] <- 0
datWB$Weekend[datWB$Weekday == "Thursday"] <- 0
datWB$Weekend[datWB$Weekday == "Friday"] <- 0
datWB$Weekend[datWB$Weekday == "Saturday"] <- 1
datWB$Weekend[datWB$Weekday == "Sunday"] <- 1
datWB$Return[datWB$ScanCount < 0] <- 1
datWB$Return[datWB$ScanCount >= 0] <- 0
datWB$TripType <- factor(datWB$TripType)
datWB$FinelineNumber <- factor(datWB$FinelineNumber)
datWB$Return<- factor(datWB$Return)
#datWB$Weekend <- factor(datWB$Weekend)
datWB <- datWB[, c(-2,-3)]
datWB=na.omit(datWB)
summary(datWB)
str(datWB)

set.seed(100)
folds <- createFolds(datWB$Return, k = 4)
#str(folds)


cv_resultsk <- lapply(folds, function(x){
  train <- datWB[-x, ]
  test <- datWB[x, ]
  modelK <- ksvm(train$Return~., data = train, kernel="laplacedot", C=1) 
  predK <- predict(modelK, test)
  return(mmetric(test$Return,predK,c("ACC","PRECISION","TPR","F1")))
})
rowMeans(cbind(cv_resultsk$Fold1[1:7], cv_resultsk$Fold2[1:7],cv_resultsk$Fold3[1:7],cv_resultsk$Fold4[1:7]))
