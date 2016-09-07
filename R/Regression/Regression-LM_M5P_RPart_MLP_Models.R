# Set the work directory 
setwd("E:/UnivOfUtah/DataMining/Labs")

sink("output.txt")

# 1 a
# Import the dataset
housing <- read.csv("Housing.csv")
summary(housing)

# Mean of MEDV
mean(housing$MEDV)
# 22.53281

# 1 b
# Create a dummy variable
housing$RIVERCODE[housing$RIVER == "No"] <- 0
housing$RIVERCODE[housing$RIVER == "Yes"] <- 1

str(housing$RIVERCODE)
summary(housing$RIVERCODE)
mean(housing$RIVERCODE)
# 0.0691

# 1 c
# Correlation table
cor(subset(housing, select = c("CRIM","ZN", "INDUS", "NOX", "RM", "AGE", "DIS",
                               "RAD", "TAX", "PRATIO", "LSTAT", "MEDV", "RIVERCODE")))
# There is some correlation between between some of the dependent variables. The maximum 
# correlation is between the variables RAD and TAX. The variables NOX and DIS are also 
# relatively high correlated. The variable DIS is moderately correlated with INDUS, NOX
# and AGE variables.

# Correlation matrix
library(psych)
pairs.panels(subset(housing, select = c("CRIM","ZN", "INDUS", "NOX", "RM", "AGE", "DIS",
                                        "RAD", "TAX", "PRATIO", "LSTAT", "MEDV", "RIVERCODE")))
# The vairables RM and LSTAT have a slight non linear relation with the MEDV varaible.
# The variables AGE and DIS those that are having the least correlation with MEDV 
# other than RIVERCODE which has the least correlation with almost all variables.


# 2 (a) (b) (c)

library(caret)
set.seed(500)

# Split the dataset.
split <- createDataPartition(housing$MEDV, p = 0.5, list = FALSE)

# Since we do not want the RIVER variable in the dataset, remove it
# before we split the datasets.
housingNoRiver = housing[-4]

housingTrain = housingNoRiver[split,]
housingTest = housingNoRiver[-split,]

set.seed(500)
splitTest <- createDataPartition(housingTest$MEDV, p = 0.5, list = FALSE)
housingTest1 <- housingTest[splitTest,]
housingTest2 <- housingTest[-splitTest,]

# Using lm method

# Build the model on Training dataset using lm.
modelTrain <- lm(MEDV ~ ., data=housingTrain)
summary(modelTrain)

# Predict for the housingTrain dataset.
predictTrain <- predict(modelTrain, housingTrain)

# Predict for the housingTest1 dataset.
predictTest1 <- predict(modelTrain, housingTest1)

# Predict for the housingTest2 dataset.
predictTest2 <- predict(modelTrain, housingTest2)


# Calculate the evaluation metrics for these predictions.
install.packages("rminer")
library(rminer)

mmetric(housingTrain$MEDV, predictTrain, c("MAE","RMSE","RAE","RRSE","COR", "R2"))
#    MAE       RMSE        RAE       RRSE        COR         R2 
# 3.2340091  4.4433176 48.9717529 49.3412210  0.8697956  0.7565444

# For testing set 1 
mmetric(housingTest1$MEDV,predictTest1,c("MAE","RMSE","RAE","RRSE","COR", "R2"))
#   MAE       RMSE        RAE       RRSE        COR         R2 
# 3.4157025  4.8637193 52.8065063 53.5033736  0.8478542 0.7188567

# For testing set 2
mmetric(housingTest2$MEDV,predictTest2,c("MAE","RMSE","RAE","RRSE","COR", "R2"))
#   MAE       RMSE        RAE       RRSE        COR         R2 
# 3.5183925  5.7478827 50.7522918 59.7717031  0.8017397  0.6427866


# 2 (a)(ii)
# 4 Fold Cross Validation
set.seed(500)
#create the folds
folds <- createFolds(housingNoRiver$MEDV, k = 4)
str(folds)
#apply the function on each fold
cv_results <- lapply(folds, function(x) {   
  housing_train <- housingNoRiver[-x, ]
  housing_test <- housingNoRiver[x, ]
  housing_model <- lm(MEDV ~ ., data = housing_train)
  housing_pred <- predict(housing_model, housing_test)
  return(mmetric(housing_test$MEDV,housing_pred,c("MAE","RMSE","RAE","RRSE","COR", "R2")))
})

cv_results$Fold1
#   MAE       RMSE        RAE       RRSE        COR         R2 
# 3.4973999  5.2108241 51.9725622 55.5585418  0.8318779  0.6920208

cv_results$Fold2
#   MAE       RMSE        RAE       RRSE        COR         R2 
# 3.2822709  4.3210384 51.1098295 49.4331752  0.8695752 0.7561610

cv_results$Fold3
#   MAE       RMSE        RAE       RRSE        COR         R2 
# 3.3084799  4.3886629 47.5916912 45.7964024  0.8916538 0.7950465

cv_results$Fold4
#   MAE       RMSE        RAE       RRSE        COR         R2 
# 3.7888629  5.6899605 58.3850386 63.0494934  0.7844302 0.6153307


###---------------------------###
# Using RPart
library(rpart)

# Build the model on Training dataset using rpart.
modelTrain2 <- rpart(MEDV ~ ., data=housingTrain)
summary(modelTrain2)

# Predict for the housingTest dataset.
predictTrainRPart <- predict(modelTrain2, housingTrain)

# Predict for the housingTest1 dataset.
predictTest1RPart <- predict(modelTrain2, housingTest1)

# Predict for the housingTest2 dataset.
predictTest2RPart <- predict(modelTrain2, housingTest2)

mmetric(housingTrain$MEDV,predictTrainRPart,c("MAE","RMSE","RAE","RRSE","COR", "R2"))
#    MAE       RMSE        RAE       RRSE        COR         R2 
# 2.6032653  3.7210347 39.4205655 41.3205647  0.9106377  0.8292611

mmetric(housingTest1$MEDV,predictTest1RPart,c("MAE","RMSE","RAE","RRSE","COR", "R2"))
#   MAE       RMSE        RAE       RRSE        COR         R2 
# 3.4435804  4.8648462 53.2374963 53.5157695  0.8469709  0.7173597

mmetric(housingTest2$MEDV,predictTest2RPart,c("MAE","RMSE","RAE","RRSE","COR", "R2"))
#   MAE       RMSE        RAE       RRSE        COR         R2 
# 3.3779468  5.7279951 48.7263833 59.5648939  0.804384   0.6470338


# Using the 4 fold data.
cv_results <- lapply(folds, function(x) {   
  housing_trainrpart <- housingNoRiver[-x, ]
  housing_testrpart <- housingNoRiver[x, ]
  housing_modelrpart <- rpart(MEDV ~ ., data = housing_trainrpart)
  housing_predrpart <- predict(housing_modelrpart, housing_testrpart)
  return(mmetric(housing_testrpart$MEDV,housing_predrpart,c("MAE","RMSE","RAE","RRSE","COR", "R2")))
})

cv_results$Fold1
#   MAE       RMSE        RAE       RRSE        COR         R2
# 3.1744751  4.8700671 47.1737886 51.9253427  0.8552096  0.7313834

cv_results$Fold2
#   MAE       RMSE        RAE       RRSE        COR         R2
# 3.4417807  5.0435681 53.5936341 57.6989976  0.8388251  0.7036275

cv_results$Fold3
#   MAE       RMSE        RAE       RRSE        COR         R2
# 3.0984895  4.2886350 44.5710302 44.7525954  0.8952607  0.8014918

cv_results$Fold4
#   MAE       RMSE        RAE       RRSE        COR         R2
# 3.4117248  5.5359372 52.5734744 61.3427878  0.8077339  0.6524340


###---------------------------###
# Using M5P
library(RWeka)
# Build the model on Training dataset using M5P
modelTrain3 <- M5P(MEDV ~ ., data=housingTrain)
summary(modelTrain3)

# Predict for the housingTest dataset.
predictTrainM5P <- predict(modelTrain3, housingTrain)

# Predict for the housingTest1 dataset.
predictTest1M5P <- predict(modelTrain3, housingTest1)

# Predict for the housingTest2 dataset.
predictTest2M5P <- predict(modelTrain3, housingTest2)

mmetric(housingTrain$MEDV,predictTrainM5P,c("MAE","RMSE","RAE","RRSE","COR", "R2"))
#  MAE       RMSE        RAE        RRSE        COR         R2
# 2.097781  3.003351  31.766143   33.350980  0.943766   0.8906943

mmetric(housingTest1$MEDV,predictTest1M5P,c("MAE","RMSE","RAE","RRSE","COR", "R2"))
#   MAE       RMSE        RAE        RRSE        COR         R2
# 2.9568861  5.7336236 45.7132386 63.0727610  0.7939633 0.6303778

mmetric(housingTest2$MEDV,predictTest2M5P,c("MAE","RMSE","RAE","RRSE","COR", "R2"))
#   MAE       RMSE        RAE        RRSE        COR         R2
# 2.8088091  5.2480460 40.5166559 54.5739477  0.8390262 0.7039649


# Using the 4 fold data.
cv_results <- lapply(folds, function(x) {   
  housing_trainm5p <- housingNoRiver[-x, ]
  housing_testm5p <- housingNoRiver[x, ]
  housing_modelm5p <- M5P(MEDV ~ ., data = housing_trainm5p)
  housing_predm5p <- predict(housing_modelm5p, housing_testm5p)
  return(mmetric(housing_testm5p$MEDV,housing_predm5p,c("MAE","RMSE","RAE","RRSE","COR", "R2")))
})

cv_results$Fold1
#   MAE       RMSE        RAE        RRSE        COR         R2
# 2.2909766  3.0523660 34.0446982 32.5447565  0.9477208  0.8981746

cv_results$Fold2
#   MAE       RMSE        RAE        RRSE        COR         R2
# 2.3508015  3.3461604 36.6054687 38.2804590  0.9265501  0.8584951

cv_results$Fold3
#   MAE       RMSE        RAE        RRSE        COR         R2
# 2.4184820  3.2597088 34.7892841 34.0155853  0.9420897  0.8875330

cv_results$Fold4
#   MAE       RMSE        RAE        RRSE        COR         R2
# 2.7804274  4.6743881 42.8454049 51.7961075  0.8634614  0.7455656


###---------------------------###
# Using KSVM
install.packages("kernlab")
library("kernlab")

# Build the model on Training dataset using KSVM
set.seed(500)
modelTrain4 <- ksvm(MEDV ~ ., data=housingTrain, kernel="rbfdot", C=1)
summary(modelTrain4)

# Predict for the housingTest dataset.
predictTrainksvm <- predict(modelTrain4, housingTrain)

# Predict for the housingTest1 dataset.
predictTest1ksvm <- predict(modelTrain4, housingTest1)

# Predict for the housingTest2 dataset.
predictTest2ksvm <- predict(modelTrain4, housingTest2)

mmetric(housingTrain$MEDV,predictTrainksvm,c("MAE","RMSE","RAE","RRSE","COR", "R2"))
#    MAE       RMSE        RAE        RRSE        COR         R2
# 1.7656553  3.1419872 26.7368554 34.8904799  0.9426965  0.8886766

mmetric(housingTest1$MEDV,predictTest1ksvm,c("MAE","RMSE","RAE","RRSE","COR", "R2"))
#   MAE       RMSE        RAE        RRSE        COR         R2
# 2.3538632  3.6946351 36.3905495 40.6428560  0.9218652  0.8498355

mmetric(housingTest2$MEDV,predictTest2ksvm,c("MAE","RMSE","RAE","RRSE","COR", "R2"))
#   MAE       RMSE        RAE        RRSE        COR         R2
# 2.8529560  4.8936089 41.1534689 50.8881883  0.8717088  0.7598762  


# Using the 4 fold data.
set.seed(500)
cv_results <- lapply(folds, function(x) {   
  housing_trainksvm <- housingNoRiver[-x, ]
  housing_testksvm <- housingNoRiver[x, ]
  housing_modelksvm <- ksvm(MEDV ~ ., data = housing_trainksvm, kernel="rbfdot", C=1)
  housing_predksvm <- predict(housing_modelksvm, housing_testksvm)
  return(mmetric(housing_testksvm$MEDV,housing_predksvm,c("MAE","RMSE","RAE","RRSE","COR", "R2")))
})

cv_results$Fold1
#   MAE       RMSE        RAE      RRSE        COR         R2
#   2.607475  4.828687 38.747971 51.484146  0.859620  0.7389465

cv_results$Fold2
#   MAE       RMSE        RAE        RRSE        COR       R2
# 2.2400751  3.1073183 34.8812931 35.5480779  0.9353945  0.8749629

cv_results$Fold3
#   MAE       RMSE        RAE        RRSE        COR       R2
# 2.1703406  3.1031541 31.2198294 32.3819112  0.9572247  0.9162792

cv_results$Fold4
#   MAE       RMSE        RAE        RRSE        COR       R2
# 2.523227  4.428324   38.882039  49.069516  0.875247   0.7660573

###---------------------------###
# Using MultilayerPerceptron
library(RWeka)
# Build the model on Training dataset using Multilayer Perception
MLP <- make_Weka_classifier("weka/classifiers/functions/MultilayerPerceptron")
model_MLP = MLP(MEDV ~ .,data = housingTrain)

evalTrain = evaluate_Weka_classifier(model_MLP, housingTrain, numFolds = 0, complexity = FALSE,seed = 1, class = TRUE)
evalTrain$details[1]
R2_1 = 0.942673^2
# 0.8886324

evalTest1 = evaluate_Weka_classifier(model_MLP, housingTest1, numFolds = 0, complexity = FALSE,seed = 1, class = TRUE)
evalTest1$details[1]
R2_2 = 0.9138509^2
# 0.8351235

evalTest2 = evaluate_Weka_classifier(model_MLP, housingTest2, numFolds = 0, complexity = FALSE,seed = 1, class = TRUE)
evalTest2$details[1]
R2_3 = 0.8493373^2
# 0.7213738

# Using 4 folds
evalNumFold = evaluate_Weka_classifier(model_MLP, housingNoRiver, numFolds = 4, complexity = FALSE,seed = 1, class = TRUE)
evalNumFold$details[1]
R2_4 = 0.8914281^2
# 0.7946441
# evaluate_Weka_classifier returns all the required metrics 


# PART 2
# Set the work directory 
setwd("E:/UnivOfUtah/DataMining/Labs")

# 1 a
# Import the dataset
car <- read.csv("carAuction.csv")
summary(car)

library(caret)
set.seed(500)
# Split the dataset.
split <- createDataPartition(car$IsBadBuy, p = 0.5, list = FALSE)

carTrain = car[split,]
carTest = car[-split,]

set.seed(500)
splitTest <- createDataPartition(carTest$IsBadBuy, p = 0.5, list = FALSE)
carTest1 <- carTest[splitTest,]
carTest2 <- carTest[-splitTest,]

# Using SVM - KSVM
library("kernlab")
set.seed(500)
model5 <- ksvm(IsBadBuy ~ ., data = carTrain, kernel="rbfdot", C=1)
summary(model5)

# Predict using entire test dataset
predict_ksvm_train <- predict(model5, carTrain)

# Predict using test1 dataset
predict_ksvm_test1 <- predict(model5, carTest1)

# Predict using test2 dataset
predict_ksvm_test2 <- predict(model5, carTest2)

# Metrics for the test dataset 1 for Yes class 
precision1 <- posPredValue(predict_ksvm_test1, carTest1$IsBadBuy,positive = "Yes")
precision1
# 0.7685185

recall1 <- sensitivity(predict_ksvm_test1, carTest1$IsBadBuy, positive = "Yes")
recall1
# 0.2561728

F_measure1 <- (2*precision1*recall1)/(precision1+recall1)
F_measure1
# 0.3842593

# Metrics for the test dataset 1 for No class
precision2 <- posPredValue(predict_ksvm_test1, carTest1$IsBadBuy, positive = "No")
precision2
# 0.8992475

recall2 <- sensitivity(predict_ksvm_test1, carTest1$IsBadBuy, positive = "No")
recall2
# 0.988511

F_measure2 <- (2*precision2*recall2)/(precision2+recall2)
F_measure2
# 0.9417688

# Metrics for the test dataset 1 for Yes class 
precision3 <- posPredValue(predict_ksvm_test2, carTest2$IsBadBuy,positive = "Yes")
precision3
# 0.78

recall3 <- sensitivity(predict_ksvm_test2, carTest2$IsBadBuy, positive = "Yes")
recall3
# 0.2414861

F_measure3 <- (2*precision3*recall3)/(precision3+recall3)
F_measure3
# 0.3687943

# Metrics for the test dataset 2 for No class
precision4 <- posPredValue(predict_ksvm_test2, carTest2$IsBadBuy, positive = "No")
precision4
# 0.8978741

recall4 <- sensitivity(predict_ksvm_test2, carTest2$IsBadBuy, positive = "No")
recall4
# 0.9898897

F_measure4 <- (2*precision4*recall4)/(precision4+recall4)
F_measure4
# 0.9416393


# Using ANN-MultiLayerPerceptron
library("RWeka")
MLP <- make_Weka_classifier("weka/classifiers/functions/MultilayerPerceptron")
set.seed(500)
carModel_MLP = MLP(IsBadBuy ~ .,data = carTrain)
evaluate_Weka_classifier(carModel_MLP, carTrain, numFolds = 0, complexity = FALSE,seed = 1, class = TRUE)
evaluate_Weka_classifier(carModel_MLP, carTest1, numFolds = 0, complexity = FALSE,seed = 1, class = TRUE)
evaluate_Weka_classifier(carModel_MLP, carTest2, numFolds = 0, complexity = FALSE,seed = 1, class = TRUE)

# The metrics are calculated within the evaluate_Weka_classifier function itself.

sink()
