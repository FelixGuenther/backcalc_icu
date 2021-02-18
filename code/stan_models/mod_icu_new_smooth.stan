data {
  int<lower=1> T; // Number days
  int<lower=0> icu_occu[T]; // Number occupied beds (observed)
  int<lower=1> S; // maximum stay at ICU
  vector[S+1] rev_surv_s; // reverse vector of discrete survival dist (P(S>=S, P(S>=S-1), ...))
  real<lower=0> lambda_1_e;
}
parameters {
  row_vector<lower=0>[T+S] lambda; // Expected value poisson distribution new icu admissions
  real<lower=0> sd_first_order;
}
model {
  real lambda_t;
  sd_first_order ~ normal(0,10);
  lambda[1] ~ normal(lambda_1_e, 10);
  for (i in 2:(T+S)) {
    lambda[i] ~ normal(lambda[i-1], sd_first_order);
  }
  lambda_t = lambda[1]*rev_surv_s[S+1]; // Expected value occupied beds day 1
  for (t in (S+1):(T+S)) {
    lambda_t = lambda[(t-S):t]*rev_surv_s[1:(S+1)]; // Expected value occopied beds given new icu cases prev days
    icu_occu[t-S] ~ poisson(lambda_t); // likelihood observed data
  }
}

