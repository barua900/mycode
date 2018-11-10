# Loading Data

# Reading CSV
states <- read.csv("~/Desktop/ClusterData.csv", header = T)
colnames(states)

# Saveing numerical data only
st <- states[, 3:27]
row.names(st) <- states[, 2]
colnames(st)

# Sports search data only
sports <- st[, 8:11]
head(sports)

# CLUSTERING ###############################################

# Creating distance matrix
d <- dist(st)

# Hierarchical clustering
c <- hclust(d)
c # Info on clustering

# Plot dendrogram of clusters
plot(c, main = "Cluster with All Searches and Personality")

# Or nest commands in one line (for sports data)
plot(hclust(dist(sports)), main = "Sports Searches")
