# Creates a sample dataset
y <- matrix(rnorm(50), 10, 5, dimnames=list(paste("g", 1:10, sep = ""), paste("t", 1:5, sep = "")))

# Compute the similarity using the spearman coefficient
c <- cor(t(y), method="spearman")
d <- as.dist(1-c)

# Perform hierarchical clustering
hr <- hclust(d, method = "complete", members = NULL)

# plot the results
par(mfrow = c(2,2))
plot(hr, hang = 0.1)
plot(hr, hang = -1)
plot(as.dendrogram(hr), edgePar = list(col = 3, lwd = 4), horiz = T)

# Prints dendrogram structure as text
str(as.dendrogram(hr))

# Prints the row labels in the order they appear in the tree
hr$labels[hr$order]

# Find the clusters
mycl <- cutree(hr, h = max(hr$height) / 2)
mycl[hr$labels[hr$order]]
plot(hr)
rect.hclust(hr, k = 5, border = "red")