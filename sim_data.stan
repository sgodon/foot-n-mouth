data {
  int<lower=1> n; // Number of data
  int<lower=1> p; // Number of covariates
  matrix[n,p] X;  // n-by-p design matrix
  real y[n];      // n-dimensional response vector
}

parameters {
  vector[p] beta;
  vector<lower=0>[p] lambda;
  real<lower=0> tau;
  real<lower=0> sigma;
}

transformed parameters {
  vector[n] theta ;
  theta = X * beta;
}

model {
  lambda ~ cauchy(0, 1);
  tau ~ cauchy(0, 1);
  beta ~ normal(0, sigma * tau * lambda); 
  y ~ normal(theta, sigma);
}

