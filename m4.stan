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
  vector<lower=0>[N] sigma_study;
  vector[N] eta;
  real<lower=0> sigma_between;
  real<lower=0> nu;
  real<lower=0> sigma_within;
  
}
transformed parameters {
  vector[N] theta;
  theta = mu + x*delta + eta;
  vector[N] rss;
  vector[N] se_study;
  for( i in 1:N){
    rss[i]=sample[i]^2*se[i]^2/sigma_study[i]^2 ;
    se_study[i]= sigma_study[i]/ sqrt(sample[i]);
  }
  real sigma_within_se;
  sigma_within_se= sqrt( sigma_within/nu);
}


// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  y ~ normal(theta, se_study);
  rss ~ chi_square(sample-1);
  eta ~ normal(0, sigma_between);
  sigma_study ~ gamma( sigma_within*nu, nu );
 // sigma_within~ gamma( 10*0.5,0.5);
}


