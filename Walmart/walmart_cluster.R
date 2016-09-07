# Set the work directory 
setwd("E:/UnivOfUtah/DataMining/Labs")

walmart_basket <- read.csv("Walmart_basket.csv")

departments = as.data.frame(model.matrix( ~ DepartmentDescription - 1, data=walmart_basket ))
days = as.data.frame(model.matrix( ~ Weekday - 1, data=walmart_basket ))

walmart_basket_new = cbind(walmart_basket, departments, days)
walmart_basket_new$Return = ifelse((walmart_basket$ScanCount < 0), 1, 0)


walmart_basket_new = walmart_basket_new[c(-2:-7)]
summary(walmart_basket_new)
str(walmart_basket_new)

walmart_basket_new$TripType <- factor(walmart_basket_new$TripType)

# Normalizing using Z score
set.seed(100)
walmart_basket_new_z <- as.data.frame(lapply(walmart_basket_new[2:78], scale))

# Kmeans
# Try K-means with a couple of centers
set.seed(100)
# k=5
walmart_clusters <- kmeans(walmart_basket_new_z, 5)
walmart_clusters$size
# [1]  28712  43820   8698 556347   9477
walmart_clusters$centers
#library(fpc)
#plotcluster(walmart_basket_new_z, walmart_clusters$cluster)

# k=3
set.seed(100)
walmart_clusters1 <- kmeans(walmart_basket_new_z, 3)
walmart_clusters1$size
#[1] 155828 482528   8698
walmart_clusters1$centers
#plotcluster(walmart_basket_new_z, walmart_clusters1$cluster)


# k=10
set.seed(100)
walmart_clusters2 <- kmeans(walmart_basket_new_z, 10)
walmart_clusters2$size
# [1]  28712  43820   8698  92281   9477  14865 104799    434 334916   9052
walmart_clusters2$centers
#plotcluster(walmart_basket_new_z, walmart_clusters2$cluster)

# k=7
set.seed(100)
walmart_clusters3 <- kmeans(walmart_basket_new_z, 7)
walmart_clusters3$size
# [1]   6107  70031 321535   1318  90386  15025 142652
walmart_clusters3$centers
#plotcluster(walmart_basket_new_z, walmart_clusters2$cluster)

# Add the cluster numbers to the dataset
walmart_basket_new$cluster = walmart_clusters2$cluster

aggregate(data=walmart_basket_new, Return ~ cluster,mean)
aggregate(data=walmart_basket_new, WeekdayFriday ~ cluster,mean)
aggregate(data=walmart_basket_new, WeekdayMonday ~ cluster,mean)
aggregate(data=walmart_basket_new, WeekdayTuesday ~ cluster,mean)
aggregate(data=walmart_basket_new, WeekdayWednesday ~ cluster,mean)
aggregate(data=walmart_basket_new, WeekdayThursday ~ cluster,mean)
aggregate(data=walmart_basket_new, WeekdaySaturday ~ cluster,mean)
aggregate(data=walmart_basket_new, WeekdaySunday ~ cluster,mean)




walmart_basket_new$cluster = walmart_clusters3$cluster

aggregate(data=walmart_basket_new, Return ~ cluster,mean)
aggregate(data=walmart_basket_new, WeekdayFriday ~ cluster,mean)
aggregate(data=walmart_basket_new, WeekdayMonday ~ cluster,mean)
aggregate(data=walmart_basket_new, WeekdayTuesday ~ cluster,mean)
aggregate(data=walmart_basket_new, WeekdayWednesday ~ cluster,mean)
aggregate(data=walmart_basket_new, WeekdayThursday ~ cluster,mean)
aggregate(data=walmart_basket_new, WeekdaySaturday ~ cluster,mean)
aggregate(data=walmart_basket_new, WeekdaySunday ~ cluster,mean)




# KNN

# create training and test data
walmart_basket_train <- walmart_basket_new_z[1:5000, ]
walmart_basket_test <- walmart_basket_new_z[5001:6000, ]

# create labels for training and test data

walmart_basket_train_labels <- walmart_basket_new[1:5000, 1]
walmart_basket_test_labels <- walmart_basket_new[5001:6000, 1]

## Training a model on the data ----

# load the "class" library
library(class)
walmart_basket_test_pred <- knn(train = walmart_basket_train, test = walmart_basket_test,
                                cl = walmart_basket_train_labels, k = 21)

## Evaluating model performance ----

# load the "gmodels" library
library(rminer)

mmetric(walmart_basket_test_labels,walmart_basket_test_pred,c("ACC"))
