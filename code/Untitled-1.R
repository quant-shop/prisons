# To build a database of k datasets containing n samples each over fields i and j in R,
# you can use a combination of lists and data frames. 

# Set the number of datasets and samples
k <- 3  # Number of datasets
n <- 100  # Number of samples

# Create an empty list to store the datasets
df <- vector("list", k)

# Generate the datasets
for (i in 1:k) {
  df[[i]] <- data.frame(i = rnorm(n), j = rnorm(n))
}

df
# Accessing a specific dataset
dataset_index <- 2  # Index of the dataset to access
dataset <- df[[dataset_index]]





# Perform analysis on the dataset using mathematical models


library(cluster)  # Required for some clustering algorithms

# Set the number of observations
n <- 100

# Set the seed for reproducibility
set.seed(123)

# Generate the sample dataset
race <- sample(c("black", "white", "latinx", "asian", "other"), n, replace = TRUE)
education <- sample(0:20, n, replace = TRUE)
income <- sample(0:450000, n, replace = TRUE)
region <- sample(c("southeast", "west", "northeast", "midwest", "south", "central"), n, replace = TRUE)
politic <- sample(-3:3, n, replace = TRUE)

# Combine variables into a data frame
df <- data.frame(race, education, income, region, politic)
df
# Perform clustering using different algorithms
summary(df)

# in this universe, the following hold:

cor(df$education, df$income) # education is not associated with income

cor(df$education, df$politic) # education is not correlated with one's political perspective
plot(df$education, df$politic) # education is not correlated with one's political perspective

cor(df$income, df$politic) # income is not related to one's political perspective
plot(df$income, df$politic) # income is not related to one's political perspective





require(cluster)

# 1. K-means
kmeans_clusters <- kmeans(dataset[, c("education", "income", "politic")], centers = 3)
print(kmeans_clusters)

# 2. Hierarchical Clustering
hierarchical_clusters <- hclust(dist(dataset[, c("education", "income", "politic")]))
hierarchical_cut <- cutree(hierarchical_clusters, k = 3)

# 3. DBSCAN
dbscan_clusters <- dbscan(dataset[, c("education", "income", "politic")], eps = 10000, MinPts = 5)

# 4. Agglomerative Clustering
agglomerative_clusters <- agnes(dataset[, c("education", "income", "politic")], diss = FALSE, method = "ward")
agglomerative_cut <- cutree(agglomerative_clusters, k = 3)

# 5. Gaussian Mixture Models (GMM)
library(mclust)
gmm_model <- Mclust(dataset[, c("education", "income", "politic")], G = 3)
gmm_clusters <- classification(gmm_model)

# 6. Spectral Clustering
spectral_clusters <- specc(dataset[, c("education", "income", "politic")], k = 3)

# 7. Mean Shift
mean_shift_clusters <- meanShift(dataset[, c("education", "income", "politic")])

# 8. Affinity Propagation
affinity_clusters <- apcluster(dataset[, c("education", "income", "politic")], preference = -10)

# Evaluate and explore the clustering results

# Example: Print the cluster assignments for K-means
kmeans_clusters$cluster

# Example: Print the dendrogram for Hierarchical Clustering
plot(hierarchical_clusters, hang = -1)

# Example: Print the number of clusters found by DBSCAN
length(unique(dbscan_clusters$cluster))

# Example: Print the cluster assignments for Agglomerative Clustering
agglomerative_cut

# Example: Print the cluster assignments for GMM
gmm_clusters$classification

# Example: Print the cluster assignments for Spectral Clustering
spectral_clusters$clustering

# Example: Print the cluster assignments for Mean Shift
mean_shift_clusters$cluster

# Example: Print the cluster assignments for Affinity Propagation
affinity_clusters@clusters



