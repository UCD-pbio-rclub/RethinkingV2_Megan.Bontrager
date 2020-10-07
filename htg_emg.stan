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
    
    real b0_t;           
    real b1_t;            
    real<lower = 0> b2_t; 
    real b3_t;                   
    
    real b_w;                       // effect of water potential
    real<lower = 0> sigma_resid;    // standard deviation of residuals
    
  // }

} 
model {
  
  // sampling occurs in 'model' block
  
  // additional convenience variables
  real mu;           

  // priors
  
    // Variances: half-cauchy priors
    sigma_resid ~ cauchy(0, 1);
    
    // Fixed effects: diffuse normal priors
    b_w ~ normal(0, 10);
    b0_t ~ normal(0, 10);
    b1_t ~ normal(0, 10);
    b2_t ~ normal(0, 10);
    b3_t ~ normal(0, 10);
  
  // }  
  // likelihood calculation

    // for each iteration, loop through 1:n observations and calculate likelihood
    for (i in 1:n) {
    
      mu = exp(
              b0_t +
              b_w * wps[i]
              ) * exp(-(temps[i] - b1_t) ^ 2 / (b2_t^2)) * erfc((-b3_t*(temps[i] - b1_t))/(sqrt(2)*b2_t));
            
      target += normal_lpdf(prop_germ[i] | mu, sigma_resid);
    
    }
    
}
generated quantities {
  
  real mu;           // predicted response in Model B
  real log_lik[n];
  real skewness; //
  real delta; //
  real mo; //
  real optimal_mismatch; //

    for (i in 1:n) {
    
      mu = exp(
              b0_t +
              b_w * wps[i]
              ) * exp(-(temps[i] - b1_t) ^ 2 / (b2_t^2)) * erfc((-b3_t*(temps[i] - b1_t))/(sqrt(2)*b2_t));
       
      log_lik[i] = normal_lpdf(prop_germ[i] | mu, sigma_resid);
    
    }

  // delta = b3_t/sqrt(1+b3_t^2);
  // skewness = ((4-pi())/2)*(delta*sqrt(2/pi()))^3*1/(1-2*delta^2/pi())^3/2;
  // mo = b1_B - skewness*b2_B/2 - exp(-(2*pi())/fabs(b3_B))/2;
  // optimal_mismatch = b1_B + b2_B*mo;


}

