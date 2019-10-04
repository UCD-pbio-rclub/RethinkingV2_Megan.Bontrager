---
title: "Chapter9"
author: "Megan Bontrager"
date: "9/27/2019"
output: 
  html_document: 
    keep_md: yes
---



## 8E1

Which of the following is a requirement of the simple Metropolis algorithm?
(1)  The parameters must be discrete. <- No
(2)  The likelihood function must be Gaussian. <- No
(3)  The proposal distribution must be symmetric. <- Yes

## 8E2

Gibbs sampling is more efficient than the Metropolis algorithm. How does it achieve this extra efficiency? Are there any limitations to the Gibbs sampling strategy?

Gibbs sampling uses "smarter" proposals to characterize the posterior distribution with fewer samples than a classic metropolis algorithm. It does this with conjugate prior-likelihood pairs, which are analytically tractable.


## 8E3 

Which sort of parameters can Hamiltonian Monte Carlo not handle? Can you explain why?

HMC can't handle discrete parameters, because it can't calculate a gradient across categories. 


## 8M1

Re-estimate the terrain ruggedness model from the chapter, but now using a uniform prior and an exponential prior for the standard deviation, sigma. The uniform prior should be dunif(0,10) and the exponential should be dexp(1). Do the different priors have any detectible influence on the posterior distribution?


```r
data(rugged)
d <- rugged
d$log_gdp <- log(d$rgdppc_2000)
dd <- d[ complete.cases(d$rgdppc_2000) , ]
dd$log_gdp_std <- dd$log_gdp / mean(dd$log_gdp)
dd$rugged_std <- dd$rugged / max(dd$rugged)
dd$cid <- ifelse( dd$cont_africa==1 , 1 , 2 )

dat_slim <- list(
    log_gdp_std = dd$log_gdp_std,
    rugged_std = dd$rugged_std,
    cid = as.integer( dd$cid )
)
str(dat_slim)
```

```
## List of 3
##  $ log_gdp_std: num [1:170] 0.88 0.965 1.166 1.104 0.915 ...
##  $ rugged_std : num [1:170] 0.138 0.553 0.124 0.125 0.433 ...
##  $ cid        : int [1:170] 1 2 2 2 2 2 2 2 2 1 ...
```

```r
m9.1a <- ulam(
    alist(
        log_gdp_std ~ dnorm( mu , sigma ) ,
        mu <- a[cid] + b[cid]*( rugged_std - 0.215 ) ,
        a[cid] ~ dnorm( 1 , 0.1 ) ,
        b[cid] ~ dnorm( 0 , 0.3 ) ,
        sigma ~ dexp( 1 )
),
data=dat_slim , chains=4 , cores=4 , iter=1000 )

m9.1b <- ulam(
    alist(
        log_gdp_std ~ dnorm( mu , sigma ) ,
        mu <- a[cid] + b[cid]*( rugged_std - 0.215 ) ,
        a[cid] ~ dnorm( 1 , 0.1 ) ,
        b[cid] ~ dnorm( 0 , 0.3 ) ,
        sigma ~ dcauchy( 0 , 1 )
),
data=dat_slim , chains=4 , cores=4 , iter=1000 )

m9.1c <- ulam(
    alist(
        log_gdp_std ~ dnorm( mu , sigma ) ,
        mu <- a[cid] + b[cid]*( rugged_std - 0.215 ) ,
        a[cid] ~ dnorm( 1 , 0.1 ) ,
        b[cid] ~ dnorm( 0 , 0.3 ) ,
        sigma ~ dunif( 0, 10 )
),
data=dat_slim , chains=4 , cores=4 , iter=1000 )

precis(m9.1a, depth = 2)
```

```
##             mean         sd        5.5%       94.5%    n_eff      Rhat
## a[1]   0.8866666 0.01590347  0.86110331  0.91158283 2588.682 1.0000233
## a[2]   1.0505345 0.01032583  1.03372131  1.06679772 3113.128 1.0001451
## b[1]   0.1303322 0.07546055  0.01455204  0.25370717 2421.596 0.9987325
## b[2]  -0.1413882 0.05622922 -0.22892846 -0.05077222 2498.149 0.9995092
## sigma  0.1117086 0.00607554  0.10240347  0.12165532 2639.240 0.9991751
```

```r
precis(m9.1b, depth = 2)
```

```
##             mean          sd       5.5%       94.5%    n_eff      Rhat
## a[1]   0.8859950 0.015567120  0.8619809  0.91214830 2954.821 0.9997398
## a[2]   1.0506406 0.009823302  1.0351340  1.06600566 2630.769 0.9993943
## b[1]   0.1336749 0.076193381  0.0147701  0.25508557 2236.072 1.0002880
## b[2]  -0.1429393 0.054217767 -0.2288805 -0.05570153 1811.689 1.0016131
## sigma  0.1115749 0.006285388  0.1020717  0.12202037 3233.761 0.9983887
```

```r
precis(m9.1c, depth = 2)
```

```
##             mean          sd        5.5%       94.5%    n_eff      Rhat
## a[1]   0.8869833 0.015801118  0.86120686  0.91213759 2846.637 0.9992940
## a[2]   1.0503325 0.009992467  1.03450907  1.06669418 2273.491 1.0002529
## b[1]   0.1323829 0.074785138  0.01127212  0.25519585 2785.902 0.9986025
## b[2]  -0.1434927 0.053586870 -0.22837548 -0.05769235 2612.944 0.9987209
## sigma  0.1116640 0.006053857  0.10258810  0.12215851 3224.745 0.9992562
```

Altering these priors does not have any impact on the estimates of the posterior.


## 8M2

The Cauchy and exponential priors from the terrain ruggedness model are very weak. They can be made more informative by reducing their scale. Compare the dcauchy and dexp priors for progressively smaller values of the scaling parameter. As these priors become stronger, how does each influence the posterior distribution?





## 8H1

Run the model below and then inspect the posterior distribution and explain what it is accomplishing.


```r
# R code 9.26
mp <- map2stan(alist(a ~ dnorm(0,1),
                     b ~ dcauchy(0,1)),
               data=list(y=1),
               start=list(a=0,b=0),
               iter=1e4, warmup=100, WAIC=FALSE)
```

```
## 
## SAMPLING FOR MODEL '522673512f2b1a8d1b770e2f4daab71f' NOW (CHAIN 1).
## Chain 1: 
## Chain 1: Gradient evaluation took 9e-06 seconds
## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.09 seconds.
## Chain 1: Adjust your expectations accordingly!
## Chain 1: 
## Chain 1: 
## Chain 1: WARNING: There aren't enough warmup iterations to fit the
## Chain 1:          three stages of adaptation as currently configured.
## Chain 1:          Reducing each adaptation stage to 15%/75%/10% of
## Chain 1:          the given number of warmup iterations:
## Chain 1:            init_buffer = 15
## Chain 1:            adapt_window = 75
## Chain 1:            term_buffer = 10
## Chain 1: 
## Chain 1: Iteration:    1 / 10000 [  0%]  (Warmup)
## Chain 1: Iteration:  101 / 10000 [  1%]  (Sampling)
## Chain 1: Iteration: 1100 / 10000 [ 11%]  (Sampling)
## Chain 1: Iteration: 2100 / 10000 [ 21%]  (Sampling)
## Chain 1: Iteration: 3100 / 10000 [ 31%]  (Sampling)
## Chain 1: Iteration: 4100 / 10000 [ 41%]  (Sampling)
## Chain 1: Iteration: 5100 / 10000 [ 51%]  (Sampling)
## Chain 1: Iteration: 6100 / 10000 [ 61%]  (Sampling)
## Chain 1: Iteration: 7100 / 10000 [ 71%]  (Sampling)
## Chain 1: Iteration: 8100 / 10000 [ 81%]  (Sampling)
## Chain 1: Iteration: 9100 / 10000 [ 91%]  (Sampling)
## Chain 1: Iteration: 10000 / 10000 [100%]  (Sampling)
## Chain 1: 
## Chain 1:  Elapsed Time: 0.005293 seconds (Warm-up)
## Chain 1:                0.18888 seconds (Sampling)
## Chain 1:                0.194173 seconds (Total)
## Chain 1:
```
Compare the samples for the parametersaandb. Can you explain the different trace plots, usingwhat you know about the Cauchy distribution?



## 8H2

Recall the divorce rate example from Chapter5. Repeat that analysis, usingmap2stanthistime, fitting modelsm5.1,m5.2, andm5.3.  Usecompareto compare the models on the basis ofWAIC. Explain the results.

## optional: 8H6

Modify the Metropolis algorithm code from the chapter to write your own simple MCMCestimator for globe tossing data and model from Chapter2.





