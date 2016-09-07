Walmart <- read.csv("Walmart_visit.csv")




Walmart$Weekend[Walmart$dow == "Monday"] <- 0
Walmart$Weekend[Walmart$dow == "Tuesday"] <- 0
Walmart$Weekend[Walmart$dow == "Wednesday"] <- 0
Walmart$Weekend[Walmart$dow == "Thursday"] <- 0
Walmart$Weekend[Walmart$dow == "Friday"] <- 0
Walmart$Weekend[Walmart$dow == "Saturday"] <- 1
Walmart$Weekend[Walmart$dow == "Sunday"] <- 1

Walmart$visit_id = NULL
Walmart$X2nd_most_quantity_dept = NULL
Walmart$X3rd_most_quantity_dept = NULL
Walmart$dow = NULL
Walmart$trip_type = NULL
Walmart$net_quantity = NULL
Walmart$most_quantity_dept = NULL


Walmart_n <- scale(Walmart) # filters to only the attributes we need and scales them
center_factor_list <- attr(Walmart_n, "scaled:center") # keep the centering factors
center_factor_list
scale_factor_list <- attr(Walmart_n, "scaled:scale") # keep the scaling factors
scale_factor_list

set.seed(500)
cluster_model <- kmeans(Walmart_n, centers=5)

# Analyze the cluster model - numeric techniques ####
cluster_model$centers
cluster_model$size
groups <- cluster_model$cluster




# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(Walmart_n, cluster_model$cluster)

