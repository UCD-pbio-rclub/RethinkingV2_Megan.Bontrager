---
title: "Chapter13.2"
author: "Megan Bontrager"
date: "1/24/2020"
output: 
  html_document: 
    keep_md: yes
---



## 12M4





```r
data(chimpanzees)
d = chimpanzees %>% 
  mutate(treatment = 1 + prosoc_left + 2*condition)

d_list = list(actor = d$actor,
              block_id = as.integer(d$block),
              treatment = as.integer(d$treatment),
              pulled_left = d$pulled_left
              )
```


First fit model from chapter:


```r
m13.4 <- ulam(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a[actor] + g[block_id] + b[treatment], 
    b[treatment] ~ dnorm(0, 0.5),
    a[actor] ~ dnorm(a_bar, sigma_a),
    g[block_id] ~ dnorm(0, sigma_g),
    a_bar ~ dnorm(0, 1.5),
    sigma_a ~ dexp(1),
    sigma_g ~ dexp(1)),
  data = d_list, cores = 2, log_lik = TRUE
  )
```

```
## 
## SAMPLING FOR MODEL '2aff399e3fa32546f1ac56f43c126f59' NOW (CHAIN 1).
## Chain 1: 
## Chain 1: Gradient evaluation took 0.000533 seconds
## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 5.33 seconds.
## Chain 1: Adjust your expectations accordingly!
## Chain 1: 
## Chain 1: 
## Chain 1: Iteration:   1 / 1000 [  0%]  (Warmup)
## Chain 1: Iteration: 100 / 1000 [ 10%]  (Warmup)
## Chain 1: Iteration: 200 / 1000 [ 20%]  (Warmup)
## Chain 1: Iteration: 300 / 1000 [ 30%]  (Warmup)
## Chain 1: Iteration: 400 / 1000 [ 40%]  (Warmup)
## Chain 1: Iteration: 500 / 1000 [ 50%]  (Warmup)
## Chain 1: Iteration: 501 / 1000 [ 50%]  (Sampling)
## Chain 1: Iteration: 600 / 1000 [ 60%]  (Sampling)
## Chain 1: Iteration: 700 / 1000 [ 70%]  (Sampling)
## Chain 1: Iteration: 800 / 1000 [ 80%]  (Sampling)
## Chain 1: Iteration: 900 / 1000 [ 90%]  (Sampling)
## Chain 1: Iteration: 1000 / 1000 [100%]  (Sampling)
## Chain 1: 
## Chain 1:  Elapsed Time: 1.39702 seconds (Warm-up)
## Chain 1:                1.2098 seconds (Sampling)
## Chain 1:                2.60682 seconds (Total)
## Chain 1:
```

Now fit new model:



```r
d_list_new = list(actor = d$actor,
              block_id = as.integer(d$block),
              condition = d$condition,
              prosoc_left = d$prosoc_left,
              pulled_left = d$pulled_left
              )

m13.new <- ulam(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a[actor] + g[block_id] + (b_p + b_pc*condition)*prosoc_left, 
    b[treatment] ~ dnorm(0, 0.5),
    a[actor] ~ dnorm(a_bar, sigma_a),
    g[block_id] ~ dnorm(0, sigma_g),
    a_bar ~ dnorm(0, 10),
    b_p ~ dnorm(0, 10),
    b_pc ~ dnorm(0, 10),
    sigma_a ~ cauchy(0, 1),
    sigma_g ~ cauchy(0, 1)),
  data = d_list_new, cores = 2, log_lik = TRUE
  )
```

```
## 
## SAMPLING FOR MODEL '99cfdf04812fa3d6d849924c3e0d69a3' NOW (CHAIN 1).
## Chain 1: 
## Chain 1: Gradient evaluation took 0.000189 seconds
## Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 1.89 seconds.
## Chain 1: Adjust your expectations accordingly!
## Chain 1: 
## Chain 1: 
## Chain 1: Iteration:   1 / 1000 [  0%]  (Warmup)
## Chain 1: Iteration: 100 / 1000 [ 10%]  (Warmup)
## Chain 1: Iteration: 200 / 1000 [ 20%]  (Warmup)
## Chain 1: Iteration: 300 / 1000 [ 30%]  (Warmup)
## Chain 1: Iteration: 400 / 1000 [ 40%]  (Warmup)
## Chain 1: Iteration: 500 / 1000 [ 50%]  (Warmup)
## Chain 1: Iteration: 501 / 1000 [ 50%]  (Sampling)
## Chain 1: Iteration: 600 / 1000 [ 60%]  (Sampling)
## Chain 1: Iteration: 700 / 1000 [ 70%]  (Sampling)
## Chain 1: Iteration: 800 / 1000 [ 80%]  (Sampling)
## Chain 1: Iteration: 900 / 1000 [ 90%]  (Sampling)
## Chain 1: Iteration: 1000 / 1000 [100%]  (Sampling)
## Chain 1: 
## Chain 1:  Elapsed Time: 1.4224 seconds (Warm-up)
## Chain 1:                1.13467 seconds (Sampling)
## Chain 1:                2.55707 seconds (Total)
## Chain 1:
```

Compare estimates.


```r
coeftab(m13.4, m13.new)
```

```
##         m13.4   m13.new
## b[1]      -0.15      NA
## b[2]       0.39      NA
## b[3]      -0.48      NA
## b[4]       0.29      NA
## a[1]      -0.34   -0.72
## a[2]       4.64    4.47
## a[3]      -0.65   -1.00
## a[4]      -0.67   -1.01
## a[5]      -0.34   -0.68
## a[6]       0.61    0.23
## a[7]       2.12    1.75
## g[1]      -0.20   -0.16
## g[2]       0.05    0.05
## g[3]       0.06    0.06
## g[4]       0.00    0.01
## g[5]      -0.03   -0.02
## g[6]       0.12    0.10
## a_bar      0.58    0.37
## sigma_a    2.01    2.14
## sigma_g    0.25    0.20
## b_p          NA    0.81
## b_pc         NA   -0.13
## nobs        504     504
```

```r
precis(m13.new, depth = 2)
```

```
##                mean        sd        5.5%       94.5%     n_eff      Rhat
## a[1]    -0.71770684 0.2804277 -1.17221141 -0.30024483 281.53949 1.0004655
## a[2]     4.47487723 1.3571331  2.79828508  7.05829175 267.44813 0.9980613
## a[3]    -1.00394242 0.2651197 -1.45142187 -0.59804570 285.32590 1.0010272
## a[4]    -1.00675612 0.2787709 -1.47815126 -0.56861106 279.81147 0.9983586
## a[5]    -0.68218108 0.3143356 -1.20127014 -0.18175732 374.40570 0.9980278
## a[6]     0.23003713 0.2930832 -0.23880500  0.72348658 296.94580 0.9985066
## a[7]     1.75193331 0.4039417  1.15207676  2.42953393 366.47819 0.9982949
## g[1]    -0.15917120 0.2341539 -0.60485132  0.09727792 197.03278 1.0088639
## g[2]     0.04810014 0.1803452 -0.18730607  0.38053378 274.02320 1.0049586
## g[3]     0.05838658 0.1727454 -0.16563289  0.38753773 286.61963 1.0000339
## g[4]     0.01408994 0.1488942 -0.22116857  0.25112789 362.05425 0.9985862
## g[5]    -0.02418776 0.1538365 -0.26828420  0.19550150 288.06738 0.9994950
## g[6]     0.09973521 0.1868197 -0.12602040  0.43287196 225.01627 0.9980145
## a_bar    0.36883500 0.7942265 -0.96344939  1.60809682 402.36538 0.9980680
## b_p      0.81315146 0.2627908  0.38987886  1.23299129 185.57686 1.0029945
## b_pc    -0.12846529 0.2907796 -0.61929073  0.33232815 244.46096 0.9990829
## sigma_a  2.14102575 0.8082458  1.21366912  3.42903493 232.16490 0.9991444
## sigma_g  0.19886417 0.1517398  0.03203566  0.50409911  88.01062 1.0026183
```



## 12H3

Model with no varying effects:


```r
data("Trolley")
d = Trolley

d_list1 = list(contact = d$contact,
              id = as.integer(d$id),
              action = d$action,
              intention = d$intention,
              story = as.integer(d$story),
              response = d$response
              )

t_mod1 = ulam(
  alist(
    response ~ ordered_logistic(phi, kappa),
    phi <-  bA*action + bI*intention + bC*contact,
    kappa ~ dnorm(0, 1.5),
    c(bA, bI, bC) ~ dnorm(0,1),
    a ~ dnorm(0, 1)
  ),
  data = d_list1, cores = 2, log_lik = TRUE
)
```


Model with varying effect of id only.


```r
d_list2 = list(contact = d$contact,
              id = as.integer(d$id),
              action = d$action,
              intention = d$intention,
              response = d$response
              )

t_mod2 = ulam(
  alist(
    response ~ ordered_logistic(phi, kappa),
    phi <- aI[id] + bA*action + bI*intention + bC*contact,
    kappa ~ dnorm(0, 1.5),
    c(bA, bI, bC) ~ dnorm(0,1),
    aI[id] ~ dnorm(a, sigma_i), 
    a ~ dnorm(0, 1),
    sigma_i ~ dexp(1)
  ),
  data = d_list2, cores = 2, log_lik = TRUE
)
```


Model with varying effect of id and story.


```r
data("Trolley")
d = Trolley

d_list3 = list(contact = d$contact,
              id = as.integer(d$id),
              action = d$action,
              intention = d$intention,
              story = as.integer(d$story),
              response = d$response
              )

t_mod3 = ulam(
  alist(
    response ~ ordered_logistic(phi, kappa),
    phi <- aI[id] + aS[story] + bA*action + bI*intention + bC*contact,
    kappa ~ dnorm(0, 1.5),
    c(bA, bI, bC) ~ dnorm(0,1),
    aI[id] ~ dnorm(a, sigma_i), 
    aS[story] ~ dnorm(0, sigma_s), 
    a ~ dnorm(0, 1),
    sigma_i ~ dexp(1),
    sigma_s ~ dexp(1)
  ),
  data = d_list3, cores = 2, log_lik = TRUE
)
```
