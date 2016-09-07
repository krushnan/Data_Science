setwd("E:/UnivOfUtah/DataMining/Labs")

# Task 1
# Load the dataset into R. Character values are imported as factors by default.
datCar = read.csv("carAuction.csv")


# Task 2
# Structure of the loadede dataset
str(datCar)


# Task 3
# Histogram of the Odometer Readings
hist(datCar$VehOdo, main="Histogram of Odometer Readings", xlab="Odometer (mi.)")
# The distribution is not a normal distribution. The distribution is skewed to the left.
mean(datCar$VehOdo)
# The mean value of the Odometer Reading is 71541.08.


# Task 4
# Histogram of the Warranty Cost
hist(datCar$WarrantyCost, main="Histogram of Waranty Cost", xlab="Price ($)")
# The distribution is not a normal distribution. The distribution is skewed to the right.
mean(datCar$WarrantyCost)
# The mean value of the Warranty Cost is 1279.954.


# Task 5
# Scatterplot to illustrate relationship between vehicle waranty cost and odomoter readings. 
# Correlation between the two variables.
plot(x =datCar$WarrantyCost, y=datCar$VehOdo, main="Scatter Plot  of Warranty Cost vs Odometer readings.",
     xlab = "Warranty Cost ($)", ylab = "Odometer Reading (mi.)")
cor(datCar$WarrantyCost, datCar$VehOdo)
# The two variables seem to be moderately correlated with a positive correlation of 0.4040918.


# Task 6
# Correlation table on the numeric values from the dataset.
cor(datCar[c("MMRCurrentAuctionAveragePrice", "VehBCost", "VehicleAge", "VehOdo", "WarrantyCost")])

# MMRCurrentAuctionAveragePrice and VehBCost have the highest correlation (0.7697305). The correlation 
# is positive. When the Acquisition price for the vehicle in average condition as of current day increases,
# the Acquisition cost paid for the vehicle at time of purchase also increases. Since these variables are
# highly correlated, only one of them should be included for prediction purpose. MMRCurrentAuctionAveragePrice
# and WarrantyCost are the leaset correlated.


# Task 7
# Correlation plot between all the numeric values from the data set.
pairs(subset(x= datCar, select = c(MMRCurrentAuctionAveragePrice, VehBCost, VehicleAge, VehOdo, WarrantyCost)))
# We can see a similar trend from the plot between these numeric values.


# Task 8
# Histograms for each of the numeric variable.
hist(datCar$MMRCurrentAuctionAveragePrice, main = "Histogram of Acquisition Price")
hist(datCar$VehBCost, main = "Histogram of Acquisition Cost")
hist(datCar$VehicleAge, main = "Histogram of Vehicle Age")
hist(datCar$VehOdo, main = "Histogram of Odometer Reading")
hist(datCar$WarrantyCost, main = "Histogram of Warranty Price")

# None of the histograms have multi-modal distributions.
# Acquisition Price is nearly normal.
# Acquisition Cost is normally distributed.
# Vehicle Age is slightly skewed to the left/ or nearly normal.
# Odometer reading is skewed to the left.
# Warranty Price is skewed to the right.


# Task 9
# Add a new column IsBadBuyCode which is a numeric translation of IsBadBuy where No = 0 and Yes = 1. 
IsBadBuyCode = ifelse(datCar$IsBadBuy == "Yes", 1, 0)
datCar$IsBadBuyCode = IsBadBuyCode


# Task 10
# Aggregate IsBadBuyCode by Color for the mean value.
aggregate (datCar$IsBadBuyCode, by=list(datCar$Color), FUN=mean)
# Alternate way to get the same output
#tapply(datCar$IsBadBuyCode, datCar$Color, mean)

# Yellow color car is most likely to be a bad buy whereas Purple color car is least likely to be a 
# bad buy.

aggregate (datCar$IsBadBuyCode, by=list(datCar$Auction), FUN=mean)
# ADESA has the highest Bad Buy rate whereas OTHER has the lowest Bad Buy rate.


# Task 11
# Drop IsBadBuyCode from the dataset.
datCar$IsBadBuyCode = NULL


# Task 12
# Randomly partition the dataset into training and validation sets using 100 as the seed number.
library(caret)
set.seed(100)
# 67% of the data needs to be in the training partition. So use p = 0.67
isTrue = createDataPartition(datCar$IsBadBuy, p = 0.67, list = FALSE)

datCarTrain = datCar[isTrue,]
datCarTest = datCar[-isTrue,]


# Task 13
# Decision tree model based on the training model datCarTrain.
library(C50)
model = C5.0(datCarTrain[-3], datCarTrain$IsBadBuy)
summary(model)
plot(model)

# Randomly partition the dataset into training and validation sets using 200 as the seed number.
set.seed(200)
# 67% of the data needs to be in the training partition. So use p = 0.67
isTrue1 = createDataPartition(datCar$IsBadBuy, p = 0.67, list = FALSE)

datCarTrain1 = datCar[isTrue1,]
datCarTest1 = datCar[-isTrue1,]

model1 = C5.0(datCarTrain1[-3], datCarTrain1$IsBadBuy)
summary(model1)

# Randomly partition the dataset into training and validation sets using 300 as the seed number.
set.seed(300)
# 67% of the data needs to be in the training partition. So use p = 0.67
isTrue2 = createDataPartition(datCar$IsBadBuy, p = 0.67, list = FALSE)

datCarTrain2 = datCar[isTrue2,]
datCarTest2 = datCar[-isTrue2,] 

model2 = C5.0(datCarTrain2[-3], datCarTrain2$IsBadBuy)
summary(model2)

predictions <- predict(model, datCarTest, type = "class")
summary(predictions)
predictions1 <- predict(model1, datCarTest1, type = "class")
summary(predictions1)
predictions2 <- predict(model2, datCarTest2, type = "class")
summary(predictions2)

library(gmodels)
CrossTable(datCarTest$IsBadBuy,predictions, prop.chisq=FALSE, prop.c=FALSE, prop.r=FALSE, dnn = c("Actual BadBuy", "Predicted BadBuy"))
CrossTable(datCarTest1$IsBadBuy,predictions1, prop.chisq=FALSE, prop.c=FALSE, prop.r=FALSE, dnn = c("Actual BadBuy", "Predicted BadBuy"))
CrossTable(datCarTest2$IsBadBuy,predictions2, prop.chisq=FALSE, prop.c=FALSE, prop.r=FALSE, dnn = c("Actual BadBuy", "Predicted BadBuy"))

#library(e1071)
#confusionMatrix(predictions, datCarTest$IsBadBuy, positive = "Yes",  dnn = c("Prediction", "True"))

# Accuracy of the three training models.
# The values for these calculations are got by using summary(model#)
accTrain1 = (5806+198)/(5806+198+670+27)
accTrain1
# 0.8959857

accTrain2 = (5804+202)/(5804+202+666+29)
accTrain2
# 0.8962841

accTrain3 = (5806+194)/(5806+194+674+27)
accTrain3
# 0.8953887

# The accuracy of the test models are calculated from the output of the crosstable functions.
accTest1 = (2857+101)/(2857+101+15+326) 
accTest1  
# 0.8966353  
  
accTest2 = (2859+97)/(2859+97+330+13)
accTest2 
# 0.8960291

accTest3 = (2857+105)/(2857+105+322+15)
accTest3
# 0.8978478
  
# The accuracy therefore the error rates of each tree almost match for the training and testing datasets. 
# The trees therefore do not seem to overfit the training data. The average accuracy of the three models 
# using the testing datasets is approximately 89.6%.


# Task 14
# Use the 3 fold validation method now.
library(caret)
folds <- createFolds(datCar$IsBadBuy, k = 3) #create the folds
str(folds)

fold_results <- lapply(folds, function(x) {   #apply the function on each flod
  datCar_train <- datCar[x, ]
  datCar_test <- datCar[-x, ]
  datCar_model <- C5.0(datCar_train[-3], datCar_train$IsBadBuy)
  datCar_pred <- predict(datCar_model, datCar_test)
  datCar_actual <- datCar_test$IsBadBuy
  accuracy <- as.integer(datCar_pred) - as.integer(datCar_actual)
  accuracy_value = (length(accuracy[accuracy==0])/length(datCar_actual))
})

str(fold_results)
#List of 3
#$ Fold1: num 0.896
#$ Fold2: num 0.891
#$ Fold3: num 0.894
# The average accuracy using the 3 Fold validation method is almost the same as by using the C5.0 tree method.

