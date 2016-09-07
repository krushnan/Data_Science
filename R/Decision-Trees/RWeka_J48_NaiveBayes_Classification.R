# Set the work directory.
setwd("E:/UnivOfUtah/DataMining/Labs")
sink("assignment2_output.txt")

# Task 2
# Import the BankSet dataset
datBank = read.csv("BankSet.csv")

# Find out how many customers have housing loans.
table(datBank$housing)
#2559 have housing loans. 

# Find out how may customers are retired.
table(datBank$job)
# 230 customers are retired.

# Find the mean loan balance for the customers.
mean(datBank$balance)
# 1422.658 is the mean loan balance for the customers


# Task 3
# Plot the histogram for the loan balance.
hist(datBank$balance)
# The distribution is far from normal. It is heavily right skewed.


# Task 4
# Plot the histogram for the customer age.
hist(datBank$age)
# The distribution is close to normal. It is slightly right skewed.


# Task 5
# Add a new column called "purchase_code" to the dataset.
datBank$purchase_code[datBank$purchase == "no"] <- 0
datBank$purchase_code[datBank$purchase == "yes"] <- 1

# str command to verify if the data is coded as 0 or 1.
str(datBank)

# Code to check if there is any NA values
isNa = is.na(datBank$purchase_code)
table(isNa)
# Output is 4521 FALSE values; which means there are no missing values
# and we can use the mean function without worrying about missing values.

# Find the mean of purchase_code
mean(datBank$purchase_code)
# [1] 0.11524 is the mean of the new variable purchase_code


# Task 6
# Create a correlation table for all the numeric values.
cor(subset(x=datBank, select=c(age, balance, campaign, pdays, previous, purchase_code)))

# Based on the output, the varaible 'previous' is most correlated with 'purchase_code'
# with a correlation of 0.1167 and the variable 'balance' is the least correlated with
# 'purchase_code' with a correlation of 0.0179. The variable 'campaign' is negatively 
# correlated with 'purchase_code' with a correlation of -0.061. It is important to find 
# out the correlation as we get the strength of a linear association between the two
# variables. This association plays an important role in prediction. We can estimate 
# the value of one variable given the value of another.


# Task 7
# Perform a aggregate on the variables: job, marital, education, housing, loan, poutcome

# Aggregate the Job variable
P1 = aggregate (datBank$purchase_code, by=list(datBank$job), FUN=mean)[,2]
P0=1- P1
weight=aggregate (datBank$purchase_code, by=list(datBank$job), FUN=sum)[,2]/P1 /nrow(datBank)
entropy=-P0*log(P0,base=2)-P1*log(P1,base=2)
m=cbind("Group"=as.character(aggregate (datBank$purchase_code, by=list(datBank$job), FUN=mean)[,1]),"P0"=P0,
        "P1"=P1,"weight"=weight,"entropy"=entropy)
m
print(c("total entropy-job:", sum(weight*entropy)))


# Aggregate the marital variable
P1=aggregate (datBank$purchase_code, by=list(datBank$marital), FUN=mean)[,2]
P0=1- P1
weight=aggregate (datBank$purchase_code, by=list(datBank$marital), FUN=sum)[,2]/P1 /nrow(datBank)
entropy=-P0*log(P0,base=2)-P1*log(P1,base=2)
m=cbind("Group"=as.character(aggregate (datBank$purchase_code, by=list(datBank$marital), FUN=mean)[,1]),"P0"=P0,
        "P1"=P1,"weight"=weight,"entropy"=entropy)
m
print(c("total entropy-marital:", sum(weight*entropy)))


# Aggregate the education variable
P1=aggregate (datBank$purchase_code, by=list(datBank$education), FUN=mean)[,2]
P0=1- P1
weight=aggregate (datBank$purchase_code, by=list(datBank$education), FUN=sum)[,2]/P1 /nrow(datBank)
entropy=-P0*log(P0,base=2)-P1*log(P1,base=2)
m=cbind("Group"=as.character(aggregate (datBank$purchase_code, by=list(datBank$education), FUN=mean)[,1]),"P0"=P0,
        "P1"=P1,"weight"=weight,"entropy"=entropy)
m
print(c("total entropy-education:", sum(weight*entropy)))


# Aggregate the housing variable
P1=aggregate (datBank$purchase_code, by=list(datBank$housing), FUN=mean)[,2]
P0=1- P1
weight=aggregate (datBank$purchase_code, by=list(datBank$housing), FUN=sum)[,2]/P1 /nrow(datBank)
entropy=-P0*log(P0,base=2)-P1*log(P1,base=2)
m=cbind("Group"=as.character(aggregate (datBank$purchase_code, by=list(datBank$housing), FUN=mean)[,1]),"P0"=P0,
        "P1"=P1,"weight"=weight,"entropy"=entropy)
m
print(c("total entropy-housing:", sum(weight*entropy)))


# Aggregate the loan variable
P1=aggregate (datBank$purchase_code, by=list(datBank$loan), FUN=mean)[,2]
P0=1- P1
weight=aggregate (datBank$purchase_code, by=list(datBank$loan), FUN=sum)[,2]/P1 /nrow(datBank)
entropy=-P0*log(P0,base=2)-P1*log(P1,base=2)
m=cbind("Group"=as.character(aggregate (datBank$purchase_code, by=list(datBank$loan), FUN=mean)[,1]),"P0"=P0,
        "P1"=P1,"weight"=weight,"entropy"=entropy)
m
print(c("total entropy-loan:", sum(weight*entropy)))


# Aggregate the poutcome variable
P1=aggregate (datBank$purchase_code, by=list(datBank$poutcome), FUN=mean)[,2]
P0=1- P1	
weight=aggregate (datBank$purchase_code, by=list(datBank$poutcome), FUN=sum)[,2]/P1 /nrow(datBank)
entropy=-P0*log(P0,base=2)-P1*log(P1,base=2)
m=cbind("Group"=as.character(aggregate (datBank$purchase_code, by=list(datBank$poutcome), FUN=mean)[,1]),"P0"=P0,
        "P1"=P1,"weight"=weight,"entropy"=entropy)
m
print(c("total entropy-poutcome:", sum(weight*entropy)))

# Based on the total entropy of all the variables, "poutcome" variable seems to have 
# a high explanatory value for purchase_code. We care about this information as it gives
# us an idea on which variable would be the most important variable on which the decision
# tree will split.


# Task 8
# Drop the variable 'purchase_code'
datBank$purchase_code <- NULL
summary(datBank)


# Task 9
# Partition the dataset into training and validation datasets.
# Sample using a seed of 500
library(caret)
set.seed(500)
isSample = createDataPartition(datBank$purchase, p = 0.6, list = FALSE)

# Split the dataset into Training and Testing datasets.
datBankTrain = datBank[isSample,]
datBankTest = datBank[-isSample,]

nrow(datBankTrain)
nrow(datBankTest)

prop.table(table(datBankTrain$purchase))
prop.table(table(datBankTest$purchase))


# Task 10
# Train a decision tree model using the training data.
# Using the J48 algorithm

# Task (i)
# J48 model

library(RWeka)
model1 <- J48(purchase ~ ., data = datBankTrain)
model1
summary(model1)
#Plot tree
plot(model1, type = "simple")

# Evaluate the training model.
evaluate_Weka_classifier(model1,datBankTrain,numFolds = 0, complexity = FALSE,seed = 1, class = TRUE)

# Evaluate the testing model.
evaluate_Weka_classifier(model1,datBankTest,numFolds = 0, complexity = FALSE,seed = 1, class = TRUE)

# evaluate_Weka_classifier gives all the necessary metrics including the accuracy and all performance
# metrics.

#=========================================================================
# The other way is to use C5.0 to predict for training and testing data.
# Evaluate the model with the training data.
predTrain = predict(model1, datBankTrain, type = "class")

# Evaluate the model with the testing data.
predTest = predict(model1, datBankTest, type = "class")

# Calculating metrics for the training data
# Accuracy
datBankTrain_actual <- datBankTrain$purchase
accuracy <- as.integer(predTrain)-as.integer(datBankTrain_actual)
accuracy_value=(length(accuracy[accuracy==0])/length(datBankTrain_actual))
# 0.8945816

# Precision, Recall, F-Measure.

# Output train evaluation measures on class "Yes"
precision_class1 <- posPredValue(predTrain, datBankTrain_actual,positive = "yes")
# 0.6753247
recall_class1 <- sensitivity(predTrain, datBankTrain_actual, positive = "yes")
# 0.1661342
F_measure_class1 <- (2*precision_class1*recall_class1)/(precision_class1+recall_class1)
# 0.2666667

# Output train evaluation measures on class "No"
precision_class2 <- posPredValue(predTrain, datBankTrain_actual,positive = "no")
# 0.9009863
recall_class2 <- sensitivity(predTrain, datBankTrain_actual, positive = "no")
# 0.9895833
F_measure_class2 <- (2*precision_class2*recall_class2)/(precision_class2+recall_class2)
# 0.9432089

m=matrix(c("accuracy",accuracy_value,"Precision-Yes",precision_class1,"Recall-Yes",recall_class1,"F_measure-Yes",F_measure_class1,"Precision-No",precision_class2,"Recall-No",recall_class2,"F_measure-No",F_measure_class2),nrow=2)
m

# Calculating metrics for the testing data
# Accuracy
datBankTest_actual <- datBankTest$purchase
accuracy <- as.integer(predTest)-as.integer(datBankTest_actual)
accuracy_valueN=(length(accuracy[accuracy==0])/length(datBankTest_actual))
# 0.8904867

# Precision, Recall, F-Measure.
# Output test evaluation measures on class "Yes"
precision_class1N <- posPredValue(predTest, datBankTest_actual,positive = "yes")
# 0.5961538
recall_class1N <- sensitivity(predTest, datBankTest_actual, positive = "yes")
# 0.1490385
F_measure_class1N <- (2*precision_class1N*recall_class1N)/(precision_class1N+recall_class1N)
# 0.2384615

# Output test evaluation measures on class "No"
precision_class2N <- posPredValue(predTest, datBankTest_actual,positive = "no")
# 0.8992027
recall_class2N <- sensitivity(predTest, datBankTest_actual, positive = "no")
# 0.986875
F_measure_class2N <- (2*precision_class2N*recall_class2N)/(precision_class2N+recall_class2N)
# 0.9410012

m=matrix(c("accuracy",accuracy_value,"Precision-Yes",precision_class1N,"Recall-Yes",recall_class1N,"F_measure-Yes",F_measure_class1N,"Precision-No",precision_class2N,"Recall-No",recall_class2N,"F_measure-No",F_measure_class2N),nrow=2)
m
# The model does not overfit the training model. The accuracy and all the other metrics of the 
# trainging data are more than those of the testing dataset. This is one quick measure to decide
# about the model overfitting. 
#=========================================================================

# Task 10 (ii)
# Train decision tree again using CF as 0.35

model2 <- J48(purchase ~ ., data = datBankTrain, control = Weka_control(C=0.4))
model2
summary(model2)
#Plot tree
plot(model2, type = "simple")


# Evaluate the training model.
evaluate_Weka_classifier(model2,datBankTrain,numFolds = 0, complexity = FALSE,seed = 1, class = TRUE)

# Evaluate the testing model.
evaluate_Weka_classifier(model2,datBankTest,numFolds = 0, complexity = FALSE,seed = 1, class = TRUE)

# evaluate_Weka_classifier gives all the necessary metrics including the accuracy and all performance
# metrics.


# Task 10 (iii)
# The tree from (i) is more pruned compared to that from (ii). The tree from (ii) is more overfit 
# compared to the tree from (i) for the training data. The resulting tree from the (ii) is more 
# complicated than it should be. The algorithm tries to classify all the training set perfectly.


# Task 10 (iv)
# Train the decision tree using Naive Bayesian algorithm from package e1071
library(e1071)
library(caret)

# Create the Naive Bayes model.
model3 <- naiveBayes(datBankTrain$purchase ~ ., data = datBankTrain[-16])
model3
summary(model3)

# Evaluate the model with the training data.
predict_trainNB <- predict(model3,datBankTrain)

# Evaluate the model with the testing data.
predict_testNB <- predict(model3,datBankTest)

# Output train evaluation measures on class "Yes"
confusionMatrix(datBankTrain$purchase, predict_trainNB,  positive = "yes",  dnn = c("True","Prediction"))
# Accuracy : 0.8946
precision <- posPredValue(predict_trainNB, datBankTrain$purchase,positive = "yes")
# 0.6753247
recall <- sensitivity(predict_trainNB, datBankTrain$purchase, positive = "yes")
#0.1661342
F_measure <- (2*precision*recall)/(precision+recall)
# 0.2666667
m=matrix(c("class","yes","Precision",precision,"Recall",recall,"F_measure",F_measure),nrow=2)
print (m)

# Output train evaluation measures on class "No"
confusionMatrix(datBankTrain$purchase, predict_trainNB,  positive = "no",  dnn = c("True","Prediction"))
# Accuracy : 0.8946
precision <- posPredValue(predict_trainNB, datBankTrain$purchase,positive = "no")
# 0.9009863
recall <- sensitivity(predict_trainNB, datBankTrain$purchase, positive = "no")
# 0.9895833
F_measure <- (2*precision*recall)/(precision+recall)
# 0.9432089
m=matrix(c("class","no","Precision",precision,"Recall",recall,"F_measure",F_measure),nrow=2)
print (m)

# Output test evaluation measures on class "Yes"
confusionMatrix(datBankTest$purchase, predict_testNB,  positive = "yes",  dnn = c("True","Prediction"))
# Accuracy : 0.8905
precision <- posPredValue(predict_testNB, datBankTest$purchase,positive = "yes")
# 0.5961538
recall <- sensitivity(predict_testNB, datBankTest$purchase, positive = "yes")
# 0.1490385
F_measure <- (2*precision*recall)/(precision+recall)
# 0.2384615
m=matrix(c("class","yes","Precision",precision,"Recall",recall,"F_measure",F_measure),nrow=2)
print (m)

# Output test evaluation measures on class "No"
confusionMatrix(datBankTest$purchase, predict_testNB,  positive = "no",  dnn = c("True","Prediction"))
# Accuracy : 0.8905
precision <- posPredValue(predict_testNB, datBankTest$purchase,positive = "no")
# 0.8992027
recall <- sensitivity(predict_testNB, datBankTest$purchase, positive = "no")
# 0.986875
F_measure <- (2*precision*recall)/(precision+recall)
# 0.9410012
m=matrix(c("class","No","Precision",precision,"Recall",recall,"F_measure",F_measure),nrow=2)
print (m)


# Task 10 (v)
# Train the decision tree using Naive Bayesian algorithm from package RWeka
library(RWeka)

# Create an interface to Weka's NaiveBayes classifier
NB <- make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")

# Create a model using the Naive Bayesian algorithm.
NaiveBayes.model <- NB(purchase ~ ., data = datBankTrain)
NaiveBayes.model

# Evaluate the training model.
eval_NB_Split_train <- evaluate_Weka_classifier(NaiveBayes.model,newdata=datBankTrain, numFolds = 0, class=TRUE)
eval_NB_Split_train

# Evaluate the test model.
eval_NB_Split_test <- evaluate_Weka_classifier(NaiveBayes.model,newdata=datBankTest, numFolds = 0, class=TRUE)
eval_NB_Split_test

# In case of using the Naiev Bayesian algorithm from the RWeka package, we directly get the accuracy and other 
# performance metrics from the evaluate_Weka_classifier command output itself.


# Task 10 (vi)
# The Naive Bayesian model from (v) more overfits the training data as compared to the model from
# (iv). The difference in the accuracy between the training and testing dataset from the model in 
# (v) is quite high when compared to the difference in the accuracy between the training and testing 
# dataset from the model (iv). Hence it seems that the model from (v) more overfits the training data.



# Task 10 (vii)
# Using the 5 fold cross validation for the Naive Bayesian algorithm from the e1071 package.
library(caret)
library(e1071)
set.seed(500)
folds <- createFolds(datBank$purchase, k = 5)
str(folds)

# Apply the function to all the folds.
cv_results <- lapply(folds, function(x) {   
  datBankTrain1 <- datBank[-x, ]
  datBankTest1 <- datBank[x, ]
  datBankModel1 <- naiveBayes(datBankTrain1$purchase ~ ., data = datBankTrain1[-16])
  datBankPred1 <- predict(datBankModel1, datBankTest1)
  datBankActual1 <- datBankTest1$purchase
  accuracy <- as.integer(datBankPred1)-as.integer(datBankActual1)
  accuracy_value=(length(accuracy[accuracy==0])/length(datBankActual1))
  precision_class1 <- posPredValue(datBankPred1, datBankActual1,positive = "yes")
  recall_class1 <- sensitivity(datBankPred1, datBankActual1, positive = "yes")
  F_measure_class1 <- (2*precision_class1*recall_class1)/(precision_class1+recall_class1)
  precision_class2 <- posPredValue(datBankPred1, datBankActual1,positive = "no")
  recall_class2 <- sensitivity(datBankPred1, datBankActual1, positive = "no")
  F_measure_class2 <- (2*precision_class2*recall_class2)/(precision_class2+recall_class2)
  m=matrix(c("accuracy",accuracy_value,"Precision-Yes",precision_class1,"Recall-Yes",recall_class1,"F_measure-Yes",F_measure_class1,"Precision-No",precision_class2,"Recall-No",recall_class2,"F_measure-No",F_measure_class2),nrow=2)
  return (m)
})

cv_results$Fold1
cv_results$Fold2
cv_results$Fold3
colMeans(rbind(as.double(cv_results$Fold1[2,]), as.double(cv_results$Fold2[2,]),as.double(cv_results$Fold3[2,])))


# Task 10 (viii)
# Using the 5 fold cross validation for the Naive Bayesian algorithm from the RWeka package.

# Create an interface to Weka's NaiveBayes classifier
NB <- make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")

# Create a model using the Naive Bayesian algorithm.
NaiveBayes.model <- NB(purchase ~ ., data = datBankTrain)
NaiveBayes.model

# Evaluate using 5 folds.
eval_NB_CrossValidation <- evaluate_Weka_classifier(NaiveBayes.model,newdata=datBankTest, numFolds = 5, class=TRUE)
eval_NB_CrossValidation


# Task 11
# Our assumption that the cost of misclassification is the same regardless of the type of error is
# incorrect. Marketing term deposit products to customers is expensive and it only makes sense to 
# invest in customers most likely to buy these products. If we assume the cost of misclassification
# as the same for all type of errors, we cannot evaluate the actual benefit or cost of the campaign
# and understand the real worth and effort to be put behind this campaign. 


# Task 12
# Cost/Benefit calculations using the model created in Task 10(i).
eval = evaluate_Weka_classifier(model1,datBankTest,numFolds = 0, complexity = FALSE,seed = 1, class = TRUE)
eval$confusionMatrix
benefit=eval$confusionMatrix[2,2]*500
print(benefit)
cost=eval$confusionMatrix[2,1] * 500 + eval$confusionMatrix[1,2] * 50
print(cost)
print(benefit-cost)

# The net Cost is $74050. The cost per customer is $40.96. Based on this information, I would not
# go ahead with the campaign.


# Task 13 Create a cost matrix 
matrix_dimensions <- list(c("Not Purchased ", "Purchased"), c("Not Purchased", "Purchased"))
costMatrix=matrix(c(0,30,1,0),nrow=2,dimnames = matrix_dimensions)


# Task 14
# Recreate the model using this cost matrix 
model4=CostSensitiveClassifier(purchase ~ ., data=datBankTrain,control=Weka_control('cost-matrix'=costMatrix, W="weka.classifiers.trees.J48"))
model4

summary(model4)
# Yes, the new model is different from that model obtained in Task 10(i).
 

# Task 15
# Use the model created in Task 14.
eval1 = evaluate_Weka_classifier(model4,datBankTest,numFolds = 0, complexity = FALSE,seed = 1, class = TRUE)
eval1$confusionMatrix

# Cost/Benefit per customer for the Testing data
benefit=eval1$confusionMatrix[2,2]*500
print(benefit)
cost=eval1$confusionMatrix[2,1] * 500 + eval1$confusionMatrix[1,2] * 50
print(cost)
print(benefit-cost)

# The model performed better than the previous model. The net cost is $2050 and the cost per customer 
# is $1.13. Based on this information, I would think of changing my weights just a little more to
# make sure I have a benefit per customer and then go ahead with that model. However this model is 
# much better than the previous one and can be considered to move forward if required.


# Task 16
# Using Naive Bayesian algorithm from RWeka.

NB_Cost_model=CostSensitiveClassifier(purchase ~ ., data=datBankTrain,control=Weka_control('cost-matrix'=costMatrix, W="weka.classifiers.bayes.BayesNet"))
summary(NB_Cost_model)

eval_NB_Cost_Split_test <- evaluate_Weka_classifier(NB_Cost_model,datBankTest,numFolds = 0)
eval_NB_Cost_Split_test

confMatrix <- eval_NB_Cost_Split_test$confusionMatrix
confMatrix

benefit=confMatrix[2,2]*500
print(benefit)
cost=confMatrix[2,1] * 500 + confMatrix[1,2] * 50
total = confMatrix[1,1] + confMatrix[1,2] + confMatrix[2,1] + confMatrix[2,2]
print(cost)
print(benefit-cost)
print(total)
print((benefit-cost)/total)

# This model is a very good model. It has a net benefit of $16850 and the cost per customer is 
# $9.32. This is a profitable model and should be deployed for production.

sink()
