# Set the work directory 
setwd("E:/UnivOfUtah/DataMining/Labs")
sink("mahalingam.krushnan.A5.output")

# 2
# Load the data into a sparse matrix.
install.packages("arules")
library("arules")
datBake <- read.transactions("BakerySales.csv", format="single", sep = ",", cols=c("ReceiptNo","Product"))
summary(datBake)
# Coffee Eclair is the most common item in the transaction list. 
# 75000 individual transactions are there in the dataset. 
# The maximum number of products purchased in a single transaction is 8.

# 3
# Inspect the first five transactions
inspect(datBake[1:5])
# The maximum number of items in a single transaction in the 
# first five transactions inspected is 3 items.

# 4
# Examine the frequency of items
itemFrequency(datBake[, 1:3])

# Plot the frequency of items with a support of 0.09
itemFrequencyPlot(datBake, support = 0.09)
# 8 items satisfy the criteria of a support of 0.09

# 5
itemFrequencyPlot(datBake, topN = 5)
# Coffee Eclair is the most frequent item purchased.
# Yes, all five items in this plot are also in the previous
# plot based on support. This is expected since support is the
# ratio of number of transactions of X to the total numnber of 
# transactions in the dataset. For a support of 0.09, we got 8 items
# satisfying the condition which means these 8 items had the highest
# support in the dataset. So it makes sense for the top 5 items to 
# in that list too since these are the items that have high probability
# to be in the transactions.


# 6
bakeRules <- apriori(datBake, parameter = list(support =
                                                      0.04, confidence = 0.40, minlen = 2))
inspect(bakeRules)
# A total of 21 rules are generated.

summary(bakeRules)

# Inspect the first 3 rules.
inspect(bakeRules[1:3])

# Print all the rules sorted by lift.
inspect(sort(bakeRules, by = "lift"))

# Rule with the highest Lift value
#lhs                             rhs                    support    confidence lift     
#19 {Apricot Danish,Opera Cake}  => {Cherry Tart}       0.04110667 0.9553765  10.255222

# 7
bakeRules1 <- apriori(datBake, parameter = list(support =
                                                 0.03, confidence = 0.60, minlen = 2))

inspect(bakeRules1)
# A total of 12 rules are generated.

summary(bakeRules1)

# Inspect the first 3 rules.
inspect(bakeRules1[1:3])

# Print all the rules sorted by lift.
inspect(sort(bakeRules1, by = "lift"))

# Rule with the highest Lift value
#lhs                                   rhs                     support    confidence lift     
#6  {Almond Twist,Coffee Eclair}       => {Apple Pie}         0.03432000 0.9245690  11.988705

sink()
