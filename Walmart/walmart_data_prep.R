# Set the work directory 
setwd("E:/UnivOfUtah/DataMining/Labs")

walmart_basket <- read.csv("Walmart_basket.csv")
walmart_visit <- read.csv("Walmart_visit.csv")
departments = as.data.frame(model.matrix( ~ DepartmentDescription - 1, data=walmart_basket ))

unique_dep = as.data.frame(cbind(VisitNumber=walmart_basket$VisitNumber, ScanCount=walmart_basket$ScanCount, departments))
totalDepartments = unique_dep$ScanCount*departments
totalDepartments =cbind(TripType=walmart_basket$TripType, VisitNumber=walmart_basket$VisitNumber, totalDepartments)


aggregated_data = as.data.frame(aggregate(totalDepartments[3:71],totalDepartments[2], sum))
rm(totalDepartments)
walmart_visit_order = walmart_visit[with(walmart_visit,order(walmart_visit$visit_id)),]

days = as.data.frame(model.matrix( ~ dow - 1, data=walmart_visit_order ))

aggregated_data = cbind(TripType=walmart_visit_order$trip_type, 
                        UniqueDepartments = walmart_visit_order$uniq_departments,
                        aggregated_data, days)

aggregated_data = aggregated_data[,c(1:3,73:79,4:72)]

# Regression
aggregated_data_new = aggregated_data[-3]
rm(days)
rm(departments)
rm(unique_dep)
rm(aggregated_data)
aggregated_data_new$TripType = as.factor(aggregated_data_new$TripType)

# Linear Regression

library(caret)
set.seed(200)
inTrain <- createDataPartition(y=aggregated_data_new$TripType, p=0.70, list=FALSE)
traindata <- aggregated_data_new[inTrain,]
testdata0 <- aggregated_data_new[-inTrain,]
nrow(traindata)
nrow(testdata0)

set.seed(200)
inTest <- createDataPartition(y=testdata0$TripType, p=0.50, list=FALSE)
testdata1 <- testdata0[inTest,]
testdata2 <- testdata0[-inTest,]
nrow(testdata1)
nrow(testdata2)

library("kernlab")
model_ksvm = ksvm(TripType ~ ., data = traindata, kernel="rbfdot", C=1)
predictions_ksvm_test0 <- predict(model_ksvm, testdata0)
library("rminer")
mmetric(testdata0$TripType,predictions_ksvm_test0,c("MAE","RMSE","MAPE","RMSPE","RRSE","RAE","COR"))
#   MAE        RMSE        MAPE       RMSPE        RRSE         RAE         COR 
#65.3978663 188.0607102 203.1564606  37.2431427  67.2121947  40.8860862   0.7486008 


#predict result on test1
predictions_ksvm_test1 <- predict(model_ksvm, testdata1)
mmetric(testdata1$TripType,predictions_ksvm_test1,c("MAE","RMSE","MAPE","RMSPE","RRSE","RAE","COR"))
#MAE         RMSE       MAPE      RMSPE       RRSE        RAE        COR 
#63.253536 183.603091 202.722368  36.997116  66.624444  40.766113   0.752892


#predict result on test2
predictions_ksvm_test2 <- predict(model_ksvm, testdata2)
mmetric(testdata2$TripType,predictions_ksvm_test2,c("MAE","RMSE","MAPE","RMSPE","RRSE","RAE","COR"))
#   MAE        RMSE        MAPE       RMSPE        RRSE         RAE         COR 
#67.5426453 192.4159884 203.5906439  37.4876056  67.7678838  41.0074435   0.7445597 

library(caret)
library(rminer)
set.seed(200)
#create the folds
folds <- createFolds(aggregated_data_new$TripType, k = 4)
str(folds)
#apply the function on each fold
cv_results <- lapply(folds, function(x) {   
  train <- aggregated_data_new[-x, ]
  test <- aggregated_data_new[x, ]
  model <- ksvm(TripType ~ ., data = train, kernel="rbfdot", C=1)
  pred <- predict(model, test)
  return(mmetric(test$TripType, pred,c("ACC","PRECISION","TPR","F1")))
})






