#############  Function to create random matrix of ones and zeroes #######################

sim_X <- function (p = 10, n = 20, t = 0.5) {    # p: number of variables, n: number of observations, t: proportion of X values randomly set to zero, 
  X <- rbinom(n*p, 1, t)                         # create vector of ones and zeroes. proportion of ones = t
  matrix(X, n, p)                                # convert to a matrix
  
}


#############  Function to create random vector of beta coefficients #######################

sim_beta <- function (p = 10, mu_beta = 0.5, sd_beta = 0.707, a = 0.5) {    # p: number of variables with mean mu_beta and variance 0.5. Fraction a are randomly set to zero
  beta <- rnorm(p, mu_beta, sd_beta)*rbinom(p, 1, 1-a)                        # normal vector multiplied by binomial to set a/p betas to zero
  beta
}

###############   Function to create data using two functions defined above ############################ 

Data <- function(p = 10, n = 20, sigma2y = 0.2, t = 0.5, mu_beta = 0.5, sd_beta = 0.707, a = 0.9) {
  X_mat <- sim_X(p,n,t)
  beta <- sim_beta(p,mu_beta, sd_beta, a)
  y_true = X_mat %*% beta
  y <- rnorm(n, X_mat %*% beta, sigma2y)
  list(X = X_mat,beta = beta,y = y, y_true = y_true)
}

######### Run Data function  ####################################

D <- Data(p = 100, n = 200, a= 0.9)

X <- D$X
beta <- D$beta
y <- D$y
y_true <- D$y_true


########### Run Finnish Horsehoe in RSTAN     ###############################

sim_data <- list(n=length(y), p=nrow(Y), y=y, Y=Y, tau = 0,1, sigma = 0.1)
rstan_options(auto_write = TRUE)
finnish_fit <- stan(file='finnish_horseshoe.stan', data=sim_data)
