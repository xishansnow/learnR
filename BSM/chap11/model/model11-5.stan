data {
  int T;
  vector[T] Y;
  int L;
}

parameters {
  vector[T] trend;
  vector[T] season;
  real<lower=0> s_trend;
  real<lower=0> s_season;
  real<lower=0> s_y;
}

transformed parameters {
  vector[T] mu = trend[1:T] + season[1:T];
}

model {
  trend[3:T] ~ normal(2*trend[2:(T-1)] - trend[1:(T-2)], s_trend);
  for (t in L:T) {
    season[t] ~ normal(-sum(season[(t-L+1):(t-1)]), s_season);
  }
  Y[1:T] ~ normal(mu[1:T], s_y);
}
