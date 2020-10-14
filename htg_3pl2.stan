data {
  
  // 'data' block is for defining input data

    // Data lengths
    int<lower = 0> n;   // total number of observations
    int<lower = 1, upper = 8> n_temps;      
    
    // Data
    int<lower = 1, upper = 8> temps[n];      // index of temperature
    int<lower = 1> day[n];        // day
    int<lower = 0> germ[n];  // maximum germination acheived during experiment 

  // }

} 
parameters {
  
  // 'parameter' block is for defining parameters to fit during sampling
    
    // F(x) = D/(1+(x/C)^(-B)) 
    
    real<lower = 0, upper = 1> b1_t[n_temps];           // asymptote (c)
    real b2_t;                    // inflection point (b)
    real <lower = 0> b3_t;                    // steepness (a)
    
    
    
  // }

} 
model {
  
  // sampling occurs in 'model' block

  // priors
  
    // Fixed effects: diffuse normal priors
    b2_t ~ lognormal(0, 2);
    b3_t ~ normal(0, 2);
    b1_t[n_temps] ~ beta(5, 20);
  
  // }  
  // likelihood calculation

    // for each iteration, loop through 1:nB observations and calculate likelihood
    for (i in 1:n) {
      real theta;
      // F(x) = D/(1+(x/C)^(-B)) 
      theta = b1_t[temps[i]]/(1+(day[i]/b2_t)^(-b3_t));
            
      germ[i] ~ bernoulli(theta);
    
    }
    
}
// generated quantities {
//   
//   real theta;           
//   real log_lik[n];
//   
//     for (i in 1:n) {
//     
//       theta = b1_t[temps[i]]*(1+(day[i]/b2_t)^(-b3_t));
//             
//       log_lik[i] = bernoulli_log();
//     
//     }
//   
// }


