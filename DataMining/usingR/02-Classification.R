# Loding packages

pacman::p_load("class")  # class has knn function

# Loading Data

# Reading CSV
df <- read.csv("~/Desktop/ccdefault.csv", header = T)
colnames(df)
head(df)  # Show first six cases

# Defining function for normalizing data
normalize <- function(x) {
  norm <- ((x - min(x))/(max(x) - min(x)))
  return (norm)
}

# Applying function to data frame (but not index or outcome)
dfn <- as.data.frame(lapply(df[, 2:24], normalize))
head(dfn)

# Putting outcome variable back on and rename
dfn <- cbind(dfn, df[, 25])
names(dfn)[24] <- "DEFAULT"

# Checking data
colnames(dfn)
head(dfn)

# Splitting data into training set (2/3) and testing set (1/3)
set.seed(2786)  # Random seed
dfn.split <- sample(2, nrow(dfn), 
                    replace = TRUE,
                    prob = c(2/3, 1/3))

# Creating training and testing datasets without outcome
# labels. Use just the first 23 variables.
dfn.train <- dfn[dfn.split == 1, 1:23]
dfn.test  <- dfn[dfn.split == 2, 1:23]

# Creating outcome labels
dfn.train.labels <- dfn[dfn.split == 1, 24]
dfn.test.labels  <- dfn[dfn.split == 2, 24]

# Building classifier for test data.
# k = number of neighbors to compare; odd n avoids ties.
# Try with several values of k and check accuracy on
# following table.
dfn.pred <- knn(train = dfn.train,
                test = dfn.test, 
                cl = dfn.train.labels,  # true class
                k = 9)                  # n neighbors

# Compare predicted outcome to observed outcome
table(dfn.pred, dfn.test.labels)


