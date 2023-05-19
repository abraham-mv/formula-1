data {
  int<lower=0> N;  // number of observations
  int<lower=1> D;  // number of drivers
  int<lower=1> T;  // number of teams
  int<lower=1> E;  // number of engines
  int<lower=1> Y;  // number of years
  int<lower=1> K_driver;  // maximum number of races in a year
  int year[N];      // year of each observation
  int race[N];      // race number of each observation
  int driver[N];    // driver ID of each observation
  int team[N];      // team ID of each observation
  int engine[N];    // engine ID of each observation
  matrix[N, K_driver] X_driver;      // driver's covariates
  int<lower=0, upper=1> finished[N];  // binary outcome of each observation (0 = outside top 5, 1 = inside top 5)
}

parameters {
  real Intercept;
  vector[K_driver] beta1;
  vector[D] driver_effect;
  vector[T] team_effect;
  vector[E] engine_effect;
  real driver_mu;
  real<lower=0> driver_sd;
  //real team_mu;
  //real<lower=0> team_sd;
  real engine_mu;
  real<lower=0> engine_sd;
  // real<lower=0> sigma;
}

transformed parameters{
 // Varying intercepts
 real beta_driver[D]; // account for driver effects
 real beta_engine[E]; // account for engine effects
 real beta_team[T];
 
 real theta[N]; // estimate probabilites
 
 for (k in 1:E){
   beta_engine[k] = Intercept + engine_effect[k];
 }
 for (i in 1:D){
   beta_driver[i] =  driver_effect[i];
 }
 for (n in 1:N){
   theta[n] = beta_driver[driver[n]] + beta_engine[engine[n]] + X_driver[n,] * beta1;
 }
 
 
}

model {
  // Priors
  Intercept ~ normal(0, 1);
  driver_effect ~ normal(0, driver_sd);
  //team_effect ~ normal(team_mu, team_sd);
  engine_effect ~ normal(0, engine_sd);
  //driver_mu ~ normal(0, 1);
  driver_sd ~ normal(0, 1);
  //team_mu ~ normal(0, 1);
  //team_sd ~ normal(0, 1);
  
  beta1 ~ normal(0,1);
  engine_mu ~ normal(0, 1);
  engine_sd ~ normal(0, 1);
 // sigma ~ normal(0, 1);

  // Likelihood
  finished ~ bernoulli_logit(theta);
}

generated quantities {
  real y_rep[N];
  vector[N] log_lik;
  for (n in 1:N) {
    y_rep[n] = bernoulli_logit_rng(theta[n]);
    
    log_lik[n] = bernoulli_logit_lpmf(finished[n] | theta[n]);
  }
  
}