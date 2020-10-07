data {
  
  // 'data' block is for defining input data

    // Data lengths
    int<lower = 0> n;   // total number of observations
  
    // Data
    real temps[n];      // temperature
    real wps[n];        // water potential
    real prop_germ[n];  // maximum germination acheived suring experiment 

  // }

} 
parameters {
  
  // 'parameter' block is for defining parameters to fit during sampling
    
    real b0;                      // prop germ when centered temp = 0
    real b1_t;                    // temperature value where germ is maximized
    real<lower = 0> b2_t;         // "niche breadth": how germination declines with temperature deviations
    
    real b_w;                     // effect of water potential
    real<lower = 0> sigma_resid;  // standard deviation of residuals
    
  // }

} 
model {
  
  // sampling occurs in 'model' block
  
  // additional convenience variables
    real mu;           // predicted response in Model B

  // priors

    // Variances: half-cauchy priors
    sigma_resid ~ cauchy(0, 1);
    
    // Fixed effects: diffuse normal priors
    b0 ~ normal(0, 10);
    b1_t ~ normal(0, 10);
    b2_t ~ normal(0, 100);
    b_w ~ normal(0, 10);
  
  // }  
  // likelihood calculation

    // for each iteration, loop through 1:nB observations and calculate likelihood
    for (i in 1:n) {
    
      mu = exp(
           b0 +
           b_w * wps[i]
           ) * exp(-(temps[i] - b1_t) ^ 2 / b2_t);
            
      target += normal_lpdf(prop_germ[i] | mu, sigma_resid);
    
    }
    
}
generated quantities {
  
  real mu;           
  real log_lik[n];
  
    for (i in 1:n) {
    
      mu = exp(
              b0 +
              b_w * wps[i]
              ) * exp(-(temps[i] - b1_t) ^ 2 / b2_t);
            
      log_lik[i] = normal_lpdf(prop_germ[i] | mu, sigma_resid);
    
    }
  
}

