data {
  
  // 'data' block is for defining input data

    // Data lengths
    int<lower = 0> n;   // total number of observations
    int<lower = 1, upper = 8> n_temps;      
    
    // Data
    int<lower = 1, upper = 8> temps[n];      // index of temperature
    int<lower = 1> day[n];        // day
    real cumulative_prop_germ[n];  // maximum germination acheived during experiment 

  // }

} 
parameters {
  
  // 'parameter' block is for defining parameters to fit during sampling
    
    // F(x) = D/(1+(x/C)^(-B)) 
    
    real b1_t[n_temps];           // asymptote (D)
    real b2_t;           // inflection point (C)
    real b3_t;           // steepness (B)
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
    b1_t[n_temps] ~ normal(0, 10);
    b2_t ~ normal(0, 10);
    b3_t ~ normal(0, 10);
  
  // }  
  // likelihood calculation

    // for each iteration, loop through 1:nB observations and calculate likelihood
    for (i in 1:n) {
      
    // F(x) = D/(1+(x/C)^(-B)) 
    
      mu = b1_t[temps[i]]*(1+(day[i]/b2_t)^(-b3_t));
            
      target += normal_lpdf(cumulative_prop_germ[i] | mu, sigma_resid);
    
    }
    
}
generated quantities {
  
  real mu;           
  real log_lik[n];
  
    for (i in 1:n) {
    
      mu = b1_t[temps[i]]*(1+(day[i]/b2_t)^(-b3_t));
            
      log_lik[i] = normal_lpdf(cumulative_prop_germ[i] | mu, sigma_resid);
    
    }
  
}

