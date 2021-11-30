

#############  Function to create random matrix of ones and zeroes #######################

sim_X <- function (p = 10, n = 20, t = 0.5) {    # p: number of variables, n: number of observations, t: proportion of X values randomly set to zero, 
  X <- rbinom(n*p, 1, t)                                         # create vector of values between zero and one. proportion of ones = t
  matrix(X, n, p)                                                # convert to a matrix
  
}


#############  Function to create random vector of beta coefficients #######################

sim_beta <- function (p = 10, mu_beta = 0.5, sd_beta = 0.707, a = 0.5) {    # p: number of variables with mean mu_beta and variance 0.5. Fraction a randomly set to zero
  beta <- rnorm(p, mu_beta, sd_beta)*rbinom(p, 1, a)
  beta
}


###############   Function to create data   ############################ 

Data <- function(p = 15, n = 20, sigma2y = 0.2, t = 0.5, mu_beta = 0.5, sd_beta = 0.707, a = 0.5) {
  X_mat <- sim_X(p,n,t)
  beta <- sim_beta(p,mu_beta, sd_beta, a)
  y_true = X_mat %*% beta
  y <- rnorm(n, X_mat %*% beta, sigma2y)
  list(X = X_mat,beta = beta,y = y, y_true = y_true)
}

######### Run Data function  ####################################

D <- Data(p = 100, n = 200)

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

######  extracting data from the Stan file  #########################


p<- 100                           # Set p to number of variables
lowerq <- 0.025                   # Set alue of percentiles for selection of beta predicted = 0
upperq <- 0.975
E <- extract(samples)             # Extract variables from STAN file





ROC_mat <- matrix(0, ncol = 5, nrow = p)                # Create matrix which will be populated with values relating to beta and predicted beta
for(i in (1:p)) {
  ROC_mat[i,1] <- abs(mean(E$beta[,i]))                                      # Mean value of the predicted beta. This will be used to order the data
  ROC_mat[i,2] <- quantile(E$beta[,i], probs = c(lowerq))
  ROC_mat[i,3] <- quantile(E$beta[,i], probs = c(upperq))               # Calculate upper and lower percentiles of beta
  ROC_mat[i,4] <- ifelse( ROC_mat[i,2] < 0 & ROC_mat[i,3] > 0, 0, 1)    # Set predicted beta to zero if confidence interval set by above contains zero.
  ROC_mat[i,5] <- ifelse(beta[i] == 0, 0, 1)                            # Set all non zero true betas to one for model evaluation
}


ROC_mat[order(ROC_mat[,1],decreasing=TRUE),]                            # Order data by absolute value of the mean of the predicted beta








beta_condition <- ROC_mat[,5]                                         # create vector of binary version of beta 
beta_predicted_condition <- rep(0,p)                                  # create vector predicted beta where each vaue is zoro (N)
TPR <- rep(0,p+1)                                                     # Create variables to record values of true positive and false positive rates
FPR <- rep(0,p+1)
for (i in (1:p+1)) {                                                  # Calculate TPR and FPR for versions of a predicted beta vector where predictions are added to the vector one at a time
  TP <- 0                                                             # resulting in p+1 vectors and associated TPR and FPR
  FP <- 0
  TN <- 0
  FN <- 0
 for (j in (1:p)) {
   TP <- ifelse(beta_condition[j] == 1 & beta_predicted_condition[j] ==1, TP + 1, TP) 
   FP <- ifelse(beta_condition[j] == 0 & beta_predicted_condition[j] ==1, FP + 1, FP) 
   TN <- ifelse(beta_condition[j] == 0 & beta_predicted_condition[j] ==0, TN + 1, TN) 
   FN <- ifelse(beta_condition[j] == 1 & beta_predicted_condition[j] ==0, FN + 1, FN) 
 }
  TPR[i] <- TP/(TP + FN)                                              # calculate TPR and FPR for each iteration
  FPR[i] <- FP/(FP + TN)
 if (i<p +1){
   beta_predicted_condition[i] <- ROC_mat[i,5]                        # add value from "binarised" predicted beta to beta_predicted condition
  
 } 
}
TPR
FPR
beta_condition
beta_predicted_condition


