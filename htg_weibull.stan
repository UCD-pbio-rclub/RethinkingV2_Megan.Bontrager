data {
  
  // 'data' block is for defining input data

    // Data lengths
    int<lower = 0> n;   // total number of observations
  
    // Data
    real temps[n];      // temperature
    real wps[n];        // water potential
    real prop_germ[n];  // maximum germination acheived during experiment 
    
  // }

} 
parameters {
  
  // 'parameter' block is for defining parameters to fit during sampling
  
    real b0_t;            // a: average germ when temperature = 0
    real b1_t;            // b: temperature that maximimizes germ 
    real<lower = 0> b2_t; // c: how germ declines with more extreme temperatures
    real b3_t;            // d: skewness    
    
    real b_w;                        // effect of water potential
    real<lower = 0> sigma_resid;     // standard deviation of residuals
    

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
    b_w ~ normal(0, 10);

    b0_t ~ normal(0, 10);
    b1_t ~ normal(0, 10);
    b2_t ~ normal(0, 100);
    b3_t ~ normal(0, 10);
    
  // }  
  // likelihood calculation

    // for each iteration, loop through 1:nB observations and calculate likelihood
    for (i in 1:n) {
    
      mu = exp(
              b0_t +
              b_w * wps[i]
              ) * exp(-(((temps[i] - b1_t)/ b2_t)+((b3_t-1)/b3_t)^(1/b3_t))^b3_t);
              
            
      target += normal_lpdf(prop_germ[i] | mu, sigma_resid);
    
    }
    
              // a*((d-1)/d)^((1-d)/d)*(((temp-b)/c + (d-1)/d)^(1/d))^(d-1)*exp(-(())) 
}
generated quantities {
  
  real mu;
  real log_lik[n];
  
      for (i in 1:n) {
    
      mu = exp(
              b0_t +
              b_w * wps[i]
              ) * exp(-(((temps[i] - b1_t)/ b2_t)+((b3_t-1)/b3_t)^(1/b3_t))^b3_t);
            
      log_lik[i] = normal_lpdf(prop_germ[i] | mu, sigma_resid);
    
    }


}

