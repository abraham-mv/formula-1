data {
  int<lower=0> N; 
  int<lower=0> K;
  int<lower=0, upper=1> y[N];
  matrix[N, K] X;
}
parameters {
  vector[K] beta;
}
transformed parameters{
  vector[N] theta;
  theta = X * beta;
}

model {
  beta ~ normal(0,1);
  y ~ bernoulli_logit(theta);
}
generated quantities {
  // simulate data from the posterior
  vector[N] y_rep;
  // log-likelihood posterior
  vector[N] log_lik;
  for (i in 1:num_elements(y_rep)) {
    y_rep[i] = bernoulli_rng(inv_logit(theta[i]));
  }
  for (i in 1:num_elements(log_lik)) {
    log_lik[i] = bernoulli_logit_lpmf(y[i] | theta[i]);
  }
}
