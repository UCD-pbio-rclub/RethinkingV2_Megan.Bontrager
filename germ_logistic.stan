data {
  // int<lower=1> n;
  // // int<lower=1> n_temp;
  // real <lower=0, upper=1> germ[n];
  // // real temp[n_temp];
  // real temp[n];
  // real<lower=0> day[n];
}

parameters {
  // real bt_gam;
  // real bt_zi;
  // real b0;
  // real log_scale;
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
  
  real p[n];
  real a[n];
  real theta[n];
  real alpha[n];
  real beta[n];
  
  // priors
  D_temp ~ normal(0, 2);
  C_temp ~ normal(0, 2);
  B_temp ~ normal(0, 2);
  
  
  for (i in 1:n) {
    
// F(x) = D_temp*temp/(1+(prop_germ/C_temp*temp)^(-B_temp*temp))

mu = D_temp*temp/(1+exp(-B_temp*temp(log(prop_germ)-log(C_temp*temp)))

// C = inflection point
// D = upper limit
// B = steepness of curve??
  
    if (germ[i] == 0) {
      target += bernoulli_lpmf(1 | theta[i]);
    } else {
      target += bernoulli_lpmf(0 | theta[i]) +
                gamma_lpdf(day[i] | alpha[i], beta[i]);
                // gamma_lpdf(y[i] | mu*mu/va, mu/va);
                // gamma_lpdf(y | inverse(va), inverse(va)/mu);
    }
  }
}


