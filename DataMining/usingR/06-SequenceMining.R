# Installing and loading packages

pacman::p_load(pacman, depmixS4) 

# LOADING & EXAMINEING DATA 

# Using the sample dataset "speed" from depmixS4
data(speed)
str(speed)

# Ploting the data
plot(ts(speed[, 1:3]), main = "speed data")

# MODELING DATA 

# Comparing models with different numbers of hidden states.

# Model 1: Joint Gaussian-binomial response with 1 state 
model1 <- depmix(list(rt ~ 1, corr ~ 1), 
                 data = speed, 
                 nstates = 1,
                 family = list(gaussian(), 
                               multinomial("identity")))
fm1 <- fit(model1, verbose = FALSE)

# Model 2: HMM with 2 states and Pacc as covariate
model2 <- depmix(list(rt ~ 1, corr ~ 1), 
                 data = speed, 
                 nstates = 2,
                 family = list(gaussian(), 
                               multinomial("identity")), 
                 transition = ~ scale(Pacc),
                 ntimes=c(168, 134, 137))
fm2 <- fit(model2, verbose = FALSE)

# Model 3: HMM with 3 states and Pacc as covariate 
model3 <- depmix(list(rt ~ 1,corr ~ 1), 
                 data = speed, 
                 nstates = 3,
                 family = list(gaussian(), 
                               multinomial("identity")), 
                 transition = ~ scale(Pacc),
                 ntimes=c(168, 134, 137))
fm3 <- fit(model3, verbose = FALSE)

# COMPARE MODELS ###########################################

# Want lowest BIC (Bayesian Information Criterion)
plot(1:3, c(BIC(fm1), BIC(fm2), BIC(fm3)),
     ty = "b", xlab = "Model", ylab = "BIC")




