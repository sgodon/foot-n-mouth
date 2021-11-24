

#############  Function to create random matrix of ones and zeroes #######################

sim_X <- function (p = 10, n = 20, t = 0.5) {    # p: number of variables, n: number of observations, t: proportion of X values randomly set to zero, 
  X <- runif(n*p, 0, 1)                                         # create vector of values between zero and one
  for (i in 1:(n*p)) {                                        
    X[i] <- ifelse(X[i] < t, 0, 1)                               # round to zro or one depending on value of X[i] and t
  }
  matrix(X, n, p)                                                # convert to a matrix
  
}
XM <- sim_X(p = 15, n = 25, t = 0.4)                            # checking code
XM

#############  Function to create random vector of beta coefficients #######################

sim_beta <- function (p = 10, mu_beta = 0.5, sd_beta = 0.707, a = 0.5) {    # p: number of variables with . Fraction a randomly set to zero
  beta <- rnorm(p, mu_beta, sd_beta)                                        #  create random normally distributed beta vector 
  for (i in 1:p) {
    u <- runif(1,0,1)                                                       #  randomly set betas to zero
    beta[i] <- ifelse(u < a, 0, beta[i] )                                     
  }
  beta
}
beta <- sim_beta(a=0.1)                                                     # check code
beta

  

Data <- function(p = 15, n = 20, sigma2y = 0.2, t = 0.5, mu_beta = 0.5, sd_beta = 0.707, a = 0.5) {             # function to use previous functions to produce data and calculate 
  X_mat <- sim_X(p,n,t)                                                                                         # output data y
  beta <- sim_beta(p,mu_beta, sd_beta, a)
  y_true = X_mat %*% beta
  y <- rnorm(n, X_mat %*% beta, sigma2y)
  list(X = X_mat,beta = beta,y = y, y_true = y_true)
}
D <- Data()

X <- D$X
beta <- D$beta
y <- D$y
y_true <- D$y_true

########### run stan file #############
library("rstan")

sim_data <- list(n=length(y), p=ncol(X), y=y, X=X, tau = 0,1, sigma = 0.1)
rstan_options(auto_write = TRUE)
samples <- stan(file = 'sim_data.stan', data = sim_data)
stan_plot(samples)
beta

