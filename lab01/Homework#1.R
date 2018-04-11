# 1. Create a two-dimensional dataset which has two-Gaussian distributions
# Requirements: n>100
# Hint rnorm(n, mean, sd)
# SHOW me the plot of your dataset
n = 300
size = 200
m <- matrix(rnorm(n, 2, 1), size, size, dimnames=list(paste("row", 1:size, sep = ""), paste("colum", 1:size, sep = "")))
n <- matrix(rnorm(n, -2, 1), size, size, dimnames=list(paste("row", 1:size, sep = ""), paste("colum", 1:size, sep = "")))

mn <- rbind(m,n)

par(mfrow = c(1, 4))

plot(mn, xlab = "", ylab = "", xlim = c(-8,8), ylim = c(-8,8), cex = 1)
points(m, col = "Red")
points(n, col = "blue")
points(c(2,2),c(2,2), pch = 4, cex = 2, col = "black", lwd = 3)
points(c(-2,-2), c(-2,-2), pch = 4, cex = 2, col = "black", lwd = 3)

# 2. Do k-Means with the dataset
# k=2, k=5
# SHOW me the clustering result
# Plot will be a better option for you
kc <- kmeans(mn, 2)
plot(mn[,c(1, size)], col = kc$cluster, xlab = "", ylab = "", xlim = c(-8,8), ylim = c(-8,8),cex = 1)
points(kc$centers[,c(1, size)], pch = 4, col = "black", cex = 2, lwd = 3)

kc <- kmeans(mn, 5)
plot(mn[,c(1, size)], col = kc$cluster, xlab = "", ylab = "", xlim = c(-8,8), ylim = c(-8,8), cex = 1)
points(kc$centers[,c(1, size)], pch = 4, col = "black", cex = 2, lwd = 3)

# 3. Do hierarchical clustering with the dataset
# SHOW me the dendrogram
# Compute the similarity using the spearman coefficient
size = size/10
m <- matrix(rnorm(n, 2, 1), size, size)
n <- matrix(rnorm(n, -2, 1), size, size)
mn <- rbind(m,n)

c <- cor(t(mn), method="spearman")
d <- as.dist(1-c)

# Perform hierarchical clustering
hr <- hclust(d, method = "complete", members = NULL)

plot(as.dendrogram(hr), edgePar = list(col = 1, lwd = 0.01), horiz= F, xlab = "", ylab = "")
# 4. Discuss clustering results from 2, 3

