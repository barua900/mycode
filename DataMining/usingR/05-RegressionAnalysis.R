# Installing and loading packages

pacman::p_load(lars, caret) # Importing libraries

# Importing the data
data = read.csv("~/Desktop/winequality-red.csv")

# Defining variable groups
x <- as.matrix(data[-12])
y <- data[, 12]

# Backward feature selection with the recursive feature 
# elimination (RFE) algorithm (an embedded method often
# used with support vector machines)

ctrl <- rfeControl(method = "repeatedcv",
                   repeats = 5,
                   verbose = TRUE,
                   functions = lmFuncs)

# This takes a while.
rfe <- rfe(x, y ,
           sizes = c(1:11),
           rfeControl = ctrl)

# See results
rfe

# Keep features identified by RFE
x <- as.matrix(data[rfe$optVariables])

# ADDITIONAL MODELS

# Conventional stepwise regression
stepwise <- lars(x,y, type = "stepwise")

# Stagewise: Like stepwise but with better generalizability
forward <- lars(x,y, type = "forward.stagewise")

# LAR: Least Angle Regression
lar <- lars(x,y, type= "lar")

# LASSO: Least Absolute Shrinkage and Selection Operator
lasso <- lars(x, y, type = "lasso")

# Comparison of models
r2comp <- c(stepwise$R2[6], forward$R2[6], 
            lar$R2[6], lasso$R2[6]) 
names(r2comp) <- c("stepwise", "forward", "lar", "lasso") 
r2comp



