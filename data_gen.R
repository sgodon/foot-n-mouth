n <- 100 # number of predictor variables
p <- 200 # number of observations
t <- 0.3 # proportion of X values that are zero

X <- runif(n*p, 0, 1)  # create a vector of random ones and zeroes
X <- ifelse(pred_var > t,1,0)
X

X_mat <- matrix(pred_var, p, n) # convert to a matrix
X_mat

beta <- rnorm(n, 0.5, 0.5)      #create a random vector of coefficients normally distributed, mean 0.5 var 0.5
beta
a <- 0.3                        # randomly set proportiona of the co-efficients to zero
for (i in 1:n) {
  u <- runif(1,0,1)
  beta[i] <- ifelse(u < a,0,beta[i] )
}
beta

epsilon <- rnorm(p, 0.1, 0.2)

Y = X_mat %*% beta + epsilon
Y
