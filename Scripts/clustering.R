##############
# Clustering #
##############
library(cluster)
library(fpc)

sales_data <- read.csv('sales.csv', header = TRUE, sep = ",")

data <-sales_data[,c('QTY_ORDER','REVENUE')]

km <- kmeans(data,centers = 9)

table(km$cluster)

# print components of km
print(km$cluster)
# plot clusters
plot(data, col = km$cluster)
# plot centers
points(km$centers, col = 1:2, pch = 8)
