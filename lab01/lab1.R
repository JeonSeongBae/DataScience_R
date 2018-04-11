# Load the Iris dataset
data(iris)

# Remove the class label
newiris <- iris
newiris$Species <- NULL

# Perform K-Means Clustering with K=3m
kc <- kmeans(newiris, 3)

str(kc)
kc$centers
kc$size
kc$cluster

# Compare the assigned clusters and the Species
table(iris$Species, kc$cluster)

plot(newiris[,c("Sepal.Length", "Sepal.Width")], col = kc$cluster)
points(kc$centers[,c("Sepal.Length", "Sepal.Width")], col = 1:3, cex=2)

# sillhouette
library(cluster)
dissE <- daisy(newiris)
sk <- silhouette(kc$cl, dissE)
plot(sk)