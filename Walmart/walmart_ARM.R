# Set the work directory 
setwd("E:/UnivOfUtah/DataMining/Labs")

library(arules)
walmart <- read.transactions("Walmart_basket_dept_sparse.csv", sep = ",")
summary(walmart)
# 95674 transactions/itemsets
# 69 rows/items
# density 0.04153704 (number of non-zero cells)

95674*69
# 95674 * 69 = 6601506 positions in the matrix

95674*69*0.04153704
# 95674*69*0.04153704 = 274207 is the sum of departments from the transactions

(95674*69*0.04153704)/95674
# Average transaction contains 2.866056 departments.

# look at the first five transactions
inspect(walmart[1:5])

# examine the frequency of items
itemFrequency(walmart[, 1:10])


# plot the frequency of items
itemFrequencyPlot(walmart, support = 0.1)
itemFrequencyPlot(walmart, support = 0.06)


itemFrequencyPlot(walmart, topN = 10)
itemFrequencyPlot(walmart, topN = 20)

walmartrules <- apriori(walmart, 
                        parameter = list(support =0.03, confidence = 0.25, minlen = 2))

inspect(walmartrules)

inspect(sort(walmartrules, by = "lift")[1:25])


walmartrules1 <- apriori(walmart, 
                        parameter = list(support =0.01, confidence = 0.25, minlen = 2),
                        appearance = list(none = c("DSD GROCERY", "GROCERY DRY GOODS")))

inspect(walmartrules1)

inspect(sort(walmartrules1, by = "lift")[1:25])


walmartrules2 <- apriori(walmart, 
                         parameter = list(support =0.01, confidence = 0.15, minlen = 2),
                         appearance = list(rhs = c("IMPULSE MERCHANDISE"), default="lhs"))

inspect(walmartrules2)

inspect(sort(walmartrules2, by = "lift")[1:14])

