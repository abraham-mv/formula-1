data {
  int<lower=0> N;  // number of observations
  int<lower=1> D;  // number of drivers
  int<lower=1> T;  // number of teams
  int<lower=1> E;  // number of engines
  int<lower=1> Y;  // number of years
  int<lower=1> R;  // maximum number of races in a year
  int year[N];      // year of each observation
  int race[N];      // race number of each observation
  int driver[N];    // driver ID of each observation
  int team[N];      // team ID of each observation
  int engine[N];    // engine ID of each observation
  int grid[N];      // starting grid position of each observation
  int finished[N];  // binary outcome of each observation (0 = outside top 5, 1 = inside top 5)
  int num_races[N]; // number of races each driver participated in
}

parameters {
  real Intercept;
  vector[D] driver_effect;
  vector[T] team_effect;
 // vector[E] engine_effect;
  real driver_mu;
  real<lower=0> driver_sd;
  real team_mu;
  real<lower=0> team_sd;
 // real engine_mu;
  //real<lower=0> engine_sd;
  // real<lower=0> sigma;
}

transformed parameters{
  vector[N] theta;
  for (n in 1:N){
    theta[n] = Intercept + driver_effect[driver[n]] +
               team_effect[team[n]];// + engine_effect[engine[n]];
  }
}

model {
  // Priors
  Intercept ~ normal(0, 10);
  driver_effect ~ normal(driver_mu, driver_sd);
  team_effect ~ normal(team_mu, team_sd);
  //engine_effect ~ normal(engine_mu, engine_sd);
  driver_mu ~ normal(0, 1);
  driver_sd ~ normal(0, 1);
  team_mu ~ normal(0, 1);
  team_sd ~ normal(0, 1);
  //engine_mu ~ normal(0, 1);
  //engine_sd ~ normal(0, 1);
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