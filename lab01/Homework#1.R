# 1. Create a two-dimensional dataset which has two-Gaussian distributions
# Requirements: n>100
# Hint rnorm(n, mean, sd)
# SHOW me the plot of your dataset
n = 200
size = 100
m <- matrix(rnorm(n, 2, 0.7), size, size, dimnames=list(paste("g", 1:size, sep = ""), paste("t", 1:size, sep = "")))
n <- matrix(rnorm(n, -2, 0.7), size, size, dimnames=list(paste("g", 1:size, sep = ""), paste("t", 1:size, sep = "")))
mn <- rbind(m,n)
plot(rbind(m,n), xlim = c(-8, 8), ylim = c(-8, 8), col = 2:1, cex = 1)

# 2. Do k-Means with the dataset
# k=2, k=5
# SHOW me the clustering result
# Plot will be a better option for you

# 3. Do hierarchical clustering with the dataset
# SHOW me the dendrogram

# 4. Discuss clustering results from 2, 3