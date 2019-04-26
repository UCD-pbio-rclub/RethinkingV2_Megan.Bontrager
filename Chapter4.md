---
title: "Chapter4"
author: "Megan Bontrager"
date: "4/25/2019"
output: 
  html_document: 
    keep_md: yes
---



### 4E1

The first line is the likelihood. 


### 4E2

Two parameters are in the posterior distribution: the mean and the variance.


### 4E3

Pr($\mu$, $\sigma$ | y) = $\frac{\Pi i Normal(y | \mu, \sigma) Normal(\mu | 0, 10) Uniform (\sigma | 0, 10)}{\int \int \Pi Normal(y | \mu, \sigma) Normal(\mu | 0, 10) Uniform (\sigma | 0, 10) d\mu d\sigma}$


### 4E4

The second line, which describes a linear effect of x on the mean.


### 4E5

Three: the intercept, the slope, and the standard deviation.


### 4M1

For the model definition below, simulate observed heights from the prior (not the posterior).

yi ∼ Normal(\mu, \sigma) 
\mu ∼ Normal(0, 10) 
\sigma ∼ Uniform(0, 10)



```r
sample_sigma = runif(1e4, 0, 10)
sample_mu = rnorm(1e4, 0, 10)
prior_h = rnorm(1e4, sample_mu, sample_sigma)
dens(prior_h)
```

![](Chapter4_files/figure-html/unnamed-chunk-1-1.png)<!-- -->


### 4M2

Translate the model just above into a quap formula.


```r
# m1 = quap(
#     alist(
#         y ~ dnorm(mu, sigma),
#         mu ~ dnorm(0, 10),
#         sigma ~ dunif(0, 10)
#     ), data = nA)
```




### 4M3

Translate the quap model formula below into a mathematical model definition.



```r
# flist <- alist(
#     y ~ dnorm( mu , sigma ),
#     mu <- a + b*x,
#     a ~ dnorm( 0 , 50 ),
#     b ~ dunif( 0 , 10 ),
#     sigma ~ dunif( 0 , 50 )
# )
```


yi ~ Normal($\mu$, $\sigma$)

$\mu$ i = $\alpha$ + $\beta$ * xi

$\alpha$ ~ Normal(0, 50)

$\beta$ ~ Normal(0, 10)

$\sigma$ ~ Uniform(0, 50)

