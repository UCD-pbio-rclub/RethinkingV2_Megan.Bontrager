data {
  int<lower=1> n;
  real <lower=0, upper=1> germ[n];
  real temp[n];
  real<lower=0> day[n];
}

parameters {
  real bt_gam;
  real bt_zi;
  real b0;
  real log_scale;
}

// transformed parameters {
//   real<lower=0, upper=1> theta; // probability of zero
//   real<lower=0> mu; // mean 
//   real<lower=0> va; // variance
//   
//   theta = inv_logit(t);
//   mu = exp(m);
//   va = exp(v);
// }

model {
  
  real p;
  real a;
  real theta;
  real alpha;
  real beta;
  
  // priors
  b0 ~ normal(0, 2);
  bt_zi ~ normal(0, 2);
  bt_gam ~ normal(0, 2);
  log_scale ~ normal(1, 10);
  
  
  for (i in 1:n) {
    
    p = b0 + bt_zi*temp[n];
    theta = inv_logit(p);
    
    a = bt_gam*temp[n];
    alpha = log(a);
    beta = exp(log_scale);
  
  
  // 1 and 0 seem backwards here?
    if (germ[i] == 0) {
      target += bernoulli_lpmf(1 | theta);
    } else {
      target += bernoulli_lpmf(0 | theta) +
                gamma_lpdf(day[i] | alpha, beta);
                // gamma_lpdf(y[i] | mu*mu/va, mu/va);
                // gamma_lpdf(y | inverse(va), inverse(va)/mu);
    }
  }
}