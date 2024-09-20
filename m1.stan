//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  vector[N] y; 
  vector<lower=0>[N] se;
  vector[N] x;
  vector[N] sample;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  real mu;
  real delta;
  real<lower=0> sigma_within;
//  real<lower=0> sigma_between;
}
transformed parameters {
  vector[N] theta;
//  vector[N] eta;
  theta = mu + x*delta  ;
  vector[N] rss;
  
  for( i in 1:N){
  rss[i]=sample[i]^2*se[i]^2/sigma_within^2 ;
  }
}


// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  y ~ normal(theta, sigma_within/sqrt(sample));
  rss ~ chi_square(sample-1);
  // eta~ normal(0, sigma_between);
}


