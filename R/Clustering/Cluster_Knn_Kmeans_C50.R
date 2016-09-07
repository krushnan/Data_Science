# Set the work directory 
setwd("E:/UnivOfUtah/DataMining/Labs")
sink("mahalingam.krushnan.A4.output")

# 2 
# Import the dataset
datBart = read.csv("BartRider.csv")
summary(datBart)
str(datBart)

# 3
# Create dummy variable for each of the categorical variables.
datBart$DualIncDummy[datBart$DualInc == "Y"] <- 1
datBart$DualIncDummy[datBart$DualInc == "N"] <- 0

datBart$GenderDummy[datBart$Gender == "F"] <- 1
datBart$GenderDummy[datBart$Gender == "M"] <- 0

datBart$OwnRentD1Own[datBart$OwnRent == "Own"] <- 1
datBart$OwnRentD1Own[datBart$OwnRent != "Own"] <- 0

datBart$OwnRentD2Parent[datBart$OwnRent == "Parent"] <- 1
datBart$OwnRentD2Parent[datBart$OwnRent != "Parent"] <- 0

datBart$LanguageD1English[datBart$Language == "English"] <- 1
datBart$LanguageD1English[datBart$Language != "English"] <- 0

datBart$LanguageD2Spanish[datBart$Language == "Spanish"] <- 1
datBart$LanguageD2Spanish[datBart$Language != "Spanish"] <- 0

# Normalize all other data other than the dummy varaibles.
# use the scale() function to z-score standardize a data frame
names(datBart)
str(datBart)
datBart$Age = as.numeric(datBart$Age)
datBart$DistToWork = as.numeric(datBart$DistToWork)
datBart$Education = as.numeric(datBart$Education)
datBart$Income = as.numeric(datBart$Income)
datBart$NbrInHouseHold = as.numeric(datBart$NbrInHouseHold)
datBart$NbrInHouseholdUnder18 = as.numeric(datBart$NbrInHouseholdUnder18)
datBart$YrsInArea = as.numeric(datBart$YrsInArea)
str(datBart)
datBart_n <- as.data.frame(scale(datBart[c(1,2,4,6,8,9,12:18)]))
datBart_n = cbind(datBart_n, datBart[c(11)])

datBartDummy = datBart_n

datBart_n$Rider <- factor(datBart_n$Rider, levels = c("Yes", "No"),
                         labels = c("Rider", "Non-Rider"))

summary(datBart_n)

# Train the model
library(class)
library(caret)
library(rminer)
# Perform 10 fold cross validation.
set.seed(500)
folds <- createFolds(datBart_n$Rider, k = 10)
str(folds)

# Apply the function on each fold and using k=1 for knn
cv_results1 <- lapply(folds, function(x) {   
  datBart_n_train <- datBart_n[-x, ]
  datBart_n_test <- datBart_n[x, ]
  datBart_n_trainLabel <- datBart_n_train[, 14]
  datBart_n_pred <- knn(train = datBart_n_train[,-14], test = datBart_n_test[,-14], cl = datBart_n_trainLabel, k = 1)
  
  return(mmetric(datBart_n_test$Rider,datBart_n_pred,c("PRECISION","TPR","F1")))
})

cv1 = as.data.frame(cv_results1)
rowMeans(cv1)
# PRECISION1 PRECISION2  TPR1       TPR2        F11        F12 
# 81.89598   85.11096   79.73296   86.71517   80.76428   85.88590
rm(cv1)

# Apply the function on each fold and using k=5 for knn
cv_results5 <- lapply(folds, function(x) {   
  datBart_n_train <- datBart_n[-x, ]
  datBart_n_test <- datBart_n[x, ]
  datBart_n_trainLabel <- datBart_n_train[, 14]
  datBart_n_pred <- knn(train = datBart_n_train[,-14], test = datBart_n_test[,-14], cl = datBart_n_trainLabel, k = 5)
  
  return(mmetric(datBart_n_test$Rider,datBart_n_pred,c("PRECISION","TPR","F1")))
})
cv5 = as.data.frame(cv_results5)
rowMeans(cv5)
# PRECISION1 PRECISION2  TPR1       TPR2        F11        F12 
# 85.72595   86.44581   81.17688   89.80586   83.35243   88.07590
rm(cv5)

# Apply the function on each fold and using k=15 for knn
cv_results15 <- lapply(folds, function(x) {   
  datBart_n_train <- datBart_n[-x, ]
  datBart_n_test <- datBart_n[x, ]
  datBart_n_trainLabel <- datBart_n_train[, 14]
  datBart_n_pred <- knn(train = datBart_n_train[,-14], test = datBart_n_test[,-14], cl = datBart_n_trainLabel, k = 15)
  
  return(mmetric(datBart_n_test$Rider,datBart_n_pred,c("PRECISION","TPR","F1")))
})
cv15 = as.data.frame(cv_results15)
rowMeans(cv15)
# PRECISION1 PRECISION2   TPR1       TPR2        F11        F12 
# 87.16860   86.29472   80.66805   91.04831   83.75741   88.59159
rm(cv15)

# Apply the function on each fold and using k=25 for knn
cv_results25 <- lapply(folds, function(x) {   
  datBart_n_train <- datBart_n[-x, ]
  datBart_n_test <- datBart_n[x, ]
  datBart_n_trainLabel <- datBart_n_train[, 14]
  datBart_n_pred <- knn(train = datBart_n_train[,-14], test = datBart_n_test[,-14], cl = datBart_n_trainLabel, k = 25)
  
  return(mmetric(datBart_n_test$Rider,datBart_n_pred,c("PRECISION","TPR","F1")))
})
cv25 = as.data.frame(cv_results25)
rowMeans(cv25)
# PRECISION1 PRECISION2  TPR1       TPR2        F11        F12 
# 87.03432   85.30334   79.01100   91.14385   82.79865   88.11386 
rm(cv25)


# 4
# k=1
# PRECISION1 PRECISION2  TPR1       TPR2        F11        F12 
# 81.89598   85.11096   79.73296   86.71517   80.76428   85.88590

#k=5
# PRECISION1 PRECISION2  TPR1       TPR2        F11        F12 
# 85.72595   86.44581   81.17688   89.80586   83.35243   88.07590

#k=15
# PRECISION1 PRECISION2   TPR1       TPR2        F11        F12 
# 87.16860   86.29472   80.66805   91.04831   83.75741   88.59159

#k=25
# PRECISION1 PRECISION2  TPR1       TPR2        F11        F12 
# 87.03432   85.30334   79.01100   91.14385   82.79865   88.11386 

# I would prefer using k=15 as almost all the metrics are better when compared to the other
# values of k.


# 5
# Train C5.0 model
# Reloading the dataset again so that we can get rid of the dummy variables.
datBart = read.csv("BartRider.csv")

# Classification Evaluation by 10-fold Cross-validation
set.seed(500)
#create the folds
folds <- createFolds(datBart$Rider, k = 10) 
str(folds)

library(C50)
cv_resultsC50 <- lapply(folds, function(x) {   #apply the function on each flod
  datBart_train <- datBart[x, ]
  datBart_test <- datBart[-x, ]
  datBart_model <- C5.0(datBart_train[-11], datBart_train$Rider)
  datBart_pred <- predict(datBart_model, datBart_test)
  datBart_actual <- datBart_test$Rider
  return(mmetric(datBart_test$Rider,datBart_pred,c("PRECISION","TPR","F1")))
})
cvC5.0 = as.data.frame(cv_resultsC50)
rowMeans(cvC5.0)
# PRECISION1 PRECISION2   TPR1       TPR2        F11        F12 
# 86.44076   83.61427   87.95444   81.54429   87.16759   82.52605 
rm(cvC5.0)

# 6
# By choosing k=15 for the knn model in comparison to the C5.0 model, we can say that the knn
# model performs better than the C5.0 model. All the metrics for the knn model is better than
# the C5.0 model.


# PART 2
# 7
# Create a subset with only numeric data
datBartSub = subset(datBartDummy, select=c(Age, DistToWork, Education, Income, NbrInHouseHold,
                                           NbrInHouseholdUnder18, YrsInArea, DualIncDummy, GenderDummy,
                                           OwnRentD1Own, OwnRentD2Parent, LanguageD1English, 
                                           LanguageD2Spanish))

summary(datBartSub)

# 8
# Create k-means cluster model.
library(stats)
set.seed(500)
Subclusters1 <- kmeans(datBartSub, 3)

Subclusters1$size
# [1]  304 3705 1484
Subclusters1$centers
#      Age       DistToWork  Education  Income    NbrInHouseHold NbrInHouseholdUnder18  YrsInArea
#1 -0.3291840  0.0350587063 -0.6985470 -0.5150013      0.6254424             0.5368989 -0.11496570
#2  0.4194713 -0.0002915961  0.3881478  0.3503634     -0.3393608            -0.2572561 -0.01699752
#3 -0.9798311 -0.0064538297 -0.8259631 -0.7692291      0.7191356             0.5322889  0.06598746
#   DualIncDummy GenderDummy OwnRentD1Own OwnRentD2Parent LanguageD1English LanguageD2Spanish
#1   -0.1084480 -0.05083766   -0.3006966       0.3948681       -3.27646529         4.1310981
#2    0.2244564 -0.01415987    0.3697616      -0.5757341        0.23651582        -0.2420223
#3   -0.5381689  0.04576616   -0.8615601       1.3565060        0.08069698        -0.2420223

set.seed(500)
Subclusters2 <- kmeans(datBartSub, 4)
Subclusters2$size
#[1]  304 2326 1326 1537
Subclusters2$centers
#      Age      DistToWork  Education  Income    NbrInHouseHold NbrInHouseholdUnder18  YrsInArea
#1 -0.3291840  0.035058706 -0.6985470 -0.5150013     0.62544242            0.53689893 -0.1149657
#2  0.7078906  0.001348845  0.4449376  0.6760229    -0.08682877           -0.07849066  0.2739581
#3 -1.0218336 -0.001236156 -0.8865453 -0.7567374     0.80286635            0.63219157  0.1585311
#4 -0.1246131 -0.007908991  0.2296633 -0.2683378    -0.68495221           -0.53281328 -0.5286202
#  DualIncDummy GenderDummy OwnRentD1Own OwnRentD2Parent LanguageD1English LanguageD2Spanish
#1   -0.1084480 -0.05083766   -0.3006966       0.3948681       -3.27646529         4.1310981
#2    0.5341045  0.15991067    1.0565449      -0.5846501        0.24971795        -0.2420223
#3   -0.5433963  0.11487714   -0.8626587       1.5118394        0.07556058        -0.2420223
#4   -0.3180322 -0.33105053   -0.7952024      -0.4976206        0.20495002        -0.2420223


set.seed(500)
Subclusters3 <- kmeans(datBartSub, 5)
Subclusters3$size
#[1]  304 1210 1310 1360 1309
Subclusters3$centers
#        Age   DistToWork   Education     Income NbrInHouseHold NbrInHouseholdUnder18  YrsInArea
#1 -0.3291840  0.035058706 -0.6985470 -0.5150013     0.62544242            0.53689893 -0.1149657
#2  0.3257231  0.003822313  0.5062453  0.8079913     0.09139604            0.04108343  0.1517219
#3 -1.0308373 -0.001632276 -0.8913991 -0.7529305     0.79989287            0.63173441  0.1576410
#4 -0.1717202 -0.013699916  0.2181839 -0.3326556    -0.69244422           -0.54000026 -0.5547930
#5  0.9853959  0.004191996  0.3596670  0.4718422    -0.31081682           -0.23384258  0.3050992
#   DualIncDummy GenderDummy OwnRentD1Own OwnRentD2Parent LanguageD1English LanguageD2Spanish
#1   -0.1084480 -0.05083766   -0.3006966       0.3948681       -3.27646529         4.1310981
#2    1.7583853  0.25761880    0.8205638      -0.5799510        0.24595112        -0.2420223
#3   -0.5537599  0.12028573   -0.8670916       1.5322066        0.07275642        -0.2420223
#4   -0.4601240 -0.45183721   -0.8348633      -0.4861657        0.20244327        -0.2420223
#5   -0.5679785  0.12273506    1.0464733      -0.5838842        0.25042845        -0.2420223

# 9
# Taking a look at the centers and size of the clusters, I would prefer clusters of size 3 or 4 compared
# to a cluster of size 5. When we chose k=5, the centers are not as distinct as when k=3 and k=4
# By just looking at the cluster centers, I would prefer a cluster of size 3 since the cluster
# centers are more distinct when k=3 over k=4 for most of the variables.

# 10
# Reload the data to eliminate the dummy variables
datBart = read.csv("BartRider.csv")

# We have already applied C5.0 to this datBart dataset in step 5 above.
# We will now split the dataset based on the cluster id and then apply c5.0 to these
# and verify the difference between them if any after clustering.

# Selecting k=3 for number of clusters.
# Apply the cluster IDs to the original data frame
datBart$Cluster <- Subclusters1$cluster

# Find the ratio of Rider to NonRider for each cluster
table(datBart[datBart$Cluster == 1,]$Rider)
#No Yes 
#91 213
table(datBart[datBart$Cluster == 2,]$Rider)
#No  Yes 
#2803  902
table(datBart[datBart$Cluster == 3,]$Rider)
#No  Yes 
#245 1239

# If we take a look at the ratio of Rider to NonRider for each cluster, we can see that 
# the majority in cluster 2 are NonRiders,the majority in cluster 3 are Riders and in
# cluster 1, there is a mix of both Rider and Non Riders.


# 11
# Assign names - this is in the solution document.

# 12
# For k=3, the cluster 2 and 3 are the two biggest clusters.
datBartC1 = datBart[(datBart$Cluster == 2),]
datBartC2 = datBart[(datBart$Cluster == 3),]

# 13
# Apply C5.0 on datBartC1 model.

# Create the folds first
set.seed(500)
folds <- createFolds(datBartC1$Rider, k = 10) 
str(folds)

# We do not want to use the Cluster ID as a variable for classification.
datBartC1 = datBartC1[-13]

library(C50)
cv_resultsC50_C1 <- lapply(folds, function(x) {   #apply the function on each flod
  datBart_train <- datBartC1[x, ]
  datBart_test <- datBartC1[-x, ]
  datBart_model <- C5.0(datBart_train[-11], datBart_train$Rider)
  datBart_pred <- predict(datBart_model, datBart_test)
  datBart_actual <- datBart_test$Rider
  return(mmetric(datBart_test$Rider,datBart_pred,c("PRECISION","TPR","F1")))
})
cvC5.0_C1 = as.data.frame(cv_resultsC50_C1)
rowMeans(cvC5.0_C1)
# PRECISION1 PRECISION2  TPR1       TPR2        F11        F12 
# 90.13457   71.51964   91.02965   68.85843   90.55089   69.91382
rm(cvC5.0_C1)


# Apply C5.0 on datBartC2 model.
# Create the folds first
set.seed(500)
folds <- createFolds(datBartC2$Rider, k = 10) 
str(folds)

# We do not want to use the Cluster ID as a variable for classification.
datBartC2 = datBartC2[-13]

cv_resultsC50_C2 <- lapply(folds, function(x) {   #apply the function on each flod
  datBart_train <- datBartC2[x, ]
  datBart_test <- datBartC2[-x, ]
  datBart_model <- C5.0(datBart_train[-11], datBart_train$Rider)
  datBart_pred <- predict(datBart_model, datBart_test)
  datBart_actual <- datBart_test$Rider
  
  return(mmetric(datBart_test$Rider,datBart_pred,c("PRECISION","TPR","F1")))
})

cvC5.0_C2 = as.data.frame(cv_resultsC50_C2)
rowMeans(cvC5.0_C2)
# PRECISION1 PRECISION2   TPR1       TPR2        F11        F12 
# 52.40971   88.20417   37.05348   92.78064   42.33436   90.39455
rm(cvC5.0_C2)


# The output of the C5.0 models shows that after clustering, we get better performance in
# classifying the Yes and No classes. The C5.0 model on datBartC1 which had majority of 
# Non Riders performs very well in classifying the Non Riders compared to base model on datBart.
# Similarly the C5.0 model on datBartC2 which had a majority of Riders performs very well
# in classifying the Riders compared to the base model on datBart. These conclusions are based 
# on the F-Measures in each model.

# Results of applying C5.0 on the entire datBart dataset.
# PRECISION1 PRECISION2   TPR1       TPR2        F11        F12 
# 86.44076   83.61427   87.95444   81.54429   87.16759   82.52605


# Results of applying C5.0 on the datBartC1 dataset.
# datBartC1 is based on cluster 2 - majority is Non Riders.

# PRECISION1 PRECISION2  TPR1       TPR2        F11        F12 
# 90.13457   71.51964   91.02965   68.85843   90.55089   69.91382


# Results of applying C5.0 on the datBartC2 dataset.
# datBartC2 is based on cluster 3 - majority is Riders.

# PRECISION1 PRECISION2   TPR1       TPR2        F11        F12 
# 52.40971   88.20417   37.05348   92.78064   42.33436   90.39455

sink()
