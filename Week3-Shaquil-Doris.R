# Load Libaries
library("imputeTS")
library("ggplot2")
library("tidyverse")
library("cluster")
library("factoextra")
library("gridExtra")
library("magrittr")

# Read CSV file
data <- read.csv("two_label_with_selected_features_rn.csv")
continuous <- data[c(2:971)]

# Replace NAs with mean
dataset <- na_mean(continuous, option = "mode", maxgap = Inf)

# Run PCA
myPCA <- prcomp(dataset)

# Plot variance in dataset
plot(myPCA, type = "l") # Most variance in 1st through 4th principle components

# Extract out PC1 and PC2
PCdata <- cbind(data, myPCA$x[,1:2])

# Plot PC1 and PC2
ggplot(PCdata, aes(PC1, PC2, col = pheno, fill = pheno)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21, col = "black")


# Scale dataset for clustering
dataset_scaled <- scale(dataset)
head(dataset_scaled)

######## Clustering with K-Means 1 ###########

# Determine optimal number of clusters
set.seed(123)
fviz_nbclust(dataset_scaled, kmeans, method = "wss")
fviz_nbclust(dataset_scaled, kmeans, method = "silhouette")

# Create cluster plot of dataset
kmeans2 <- kmeans(dataset_scaled, centers = 2, nstart = 25)
kmeans3 <- kmeans(dataset_scaled, centers = 3, nstart = 25)  
kmeans4 <- kmeans(dataset_scaled, centers = 4, nstart = 25)  
kmeans5 <- kmeans(dataset_scaled, centers = 5, nstart = 25)  

# Comparing the Plots
plot1 <- fviz_cluster(kmeans2, geom = "point", data = dataset_scaled) + ggtitle("k = 2")
plot2 <- fviz_cluster(kmeans3, geom = "point", data = dataset_scaled) + ggtitle("k = 3")
plot3 <- fviz_cluster(kmeans4, geom = "point", data = dataset_scaled) + ggtitle("k = 4")
plot4 <- fviz_cluster(kmeans5, geom = "point", data = dataset_scaled) + ggtitle("k = 5")
grid.arrange(plot1, plot2, plot3, plot4, nrow = 2)

######## Clustering with K-Means 2 ###########
comp <- data.frame(myPCA$x[,1:4])
plot(comp, pch=16, col=rgb(0,0,0,0.5))
# Apply k-means with k = 4
k <- kmeans(comp, 4, nstart=25, iter.max=1000)
sort(table(k$clust))
clust <- names(sort(table(k$clust)))
row.names(data[k$clust==clust[1],])
row.names(data[k$clust==clust[2],])
row.names(data[k$clust==clust[3],])
row.names(data[k$clust==clust[4],])

