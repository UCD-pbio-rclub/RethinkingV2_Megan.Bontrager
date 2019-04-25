---
title: "Chapter3"
author: "Megan Bontrager"
date: "4/19/2019"
output: 
  html_document: 
    keep_md: yes
---



### Example code to start with:


```r
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 6 , size=9 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
dens(samples)
```

![](Chapter3_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

### 3E1 

How much of the posterior probability lies below p = 0.2?


```r
sum(samples < 0.2) / 1e4
```

```
## [1] 5e-04
```

### 3E2 

How much of the posterior probability lies above p = 0.8?


```r
sum(samples > 0.8) / 1e4
```

```
## [1] 0.1117
```

### 3E3 

How much of the posterior probability lies between p = 0.2 and p = 0.8?


```r
sum(samples > 0.2 & samples < 0.8) / 1e4
```

```
## [1] 0.8878
```

### 3E4 

20% of the posterior probability lies below which value of p?


```r
quantile(samples, 0.2)
```

```
##       20% 
## 0.5195195
```

### 3E5 

20% of the posterior probability lies above which value of p?


```r
quantile(samples, 0.8)
```

```
##       80% 
## 0.7567568
```

### 3E6 

Which values of p contain the narrowest interval equal to 66% of the posterior probability?


```r
HPDI(samples, p = 0.66)
```

```
##     |0.66     0.66| 
## 0.5205205 0.7847848
```

### 3E7

Which values of p contain 66% of the posterior probability, assuming equal posterior probability both below and above the interval?


```r
PI(samples, p = 0.66)
```

```
##       17%       83% 
## 0.5005005 0.7687688
```


### 3M1

Suppose the globe tossing data had turned out to be 8 water in 15 tosses. Construct the posterior distribution, using grid approximation. Use the same flat prior as before.


```r
p_grid = seq(0, 1, length.out = 1000)
prior = rep(1, 1000)
likelihood = dbinom(8 , size = 15, prob = p_grid)
posterior = likelihood*prior
posterior = posterior/sum(posterior)
plot(posterior)
```

![](Chapter3_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

### 3M2

Draw 10,000 samples from the grid approximation from above.  en use the samples to cal-
culate the 90% HPDI for p.


```r
samples = sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)
dens(samples)
```

![](Chapter3_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
HPDI(samples, 0.9)
```

```
##      |0.9      0.9| 
## 0.3383383 0.7317317
```

### 3M3

Construct a posterior predictive check for this model and data. This means simulate the distribution of samples, averaging over the posterior uncertainty in p. What is the probability of observing 8 water in 15 tosses?


```r
preds = rbinom(1e4, size = 15, prob = samples)
hist(preds)
```

![](Chapter3_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
length(preds[preds == 8])/1e4
```

```
## [1] 0.1428
```

### 3M4


```r
preds = rbinom(1e4, size = 9, prob = samples)
hist(preds)
```

![](Chapter3_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

```r
length(preds[preds == 6])/1e4
```

```
## [1] 0.1695
```

### 3M5

Start over at 3M1, but now use a prior that is zero below p=0.5 and a constant above p=0.5. This corresponds to prior information that a majority of the Earth’s surface is water. Repeat each problem above and compare the inferences. What difference does the better prior make? If it helps, compare inferences (using both priors) to the true value p = 0.7.


```r
p_grid2 = seq(0, 1, length.out = 1000)
prior2 = c(rep(0, 500), rep(1, 500))
plot(prior2)
```

![](Chapter3_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

```r
likelihood2 = dbinom(8 , size = 15, prob = p_grid2)
posterior2 = likelihood2*prior2
posterior2 = posterior2/sum(posterior2)
plot(posterior2)
```

![](Chapter3_files/figure-html/unnamed-chunk-13-2.png)<!-- -->

```r
samples2 = sample(p_grid2, prob = posterior2, size = 1e4, replace = TRUE)
dens(samples2)
```

![](Chapter3_files/figure-html/unnamed-chunk-13-3.png)<!-- -->

```r
HPDI(samples2, 0.9)
```

```
##      |0.9      0.9| 
## 0.5005005 0.7097097
```

```r
preds2 = rbinom(1e4, size = 15, prob = samples2)
hist(preds2)
```

![](Chapter3_files/figure-html/unnamed-chunk-13-4.png)<!-- -->

```r
length(preds[preds == 8])/1e4
```

```
## [1] 0.0567
```


### 3H1

Using grid approximation, compute the posterior distribution for the probability of a birth being a boy. Assume a uniform prior probability. Which parameter value maximizes the posterior probability?


```r
data(homeworkch3)

p_grid = seq(0, 1, length.out = 1000)
prior = rep(1, 1000)
likelihood = dbinom(111 , size = 200, prob = p_grid)
posterior = likelihood*prior
posterior = posterior/sum(posterior)
plot(posterior)
```

![](Chapter3_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

```r
p_grid[which.max(posterior)]
```

```
## [1] 0.5545546
```

### 3H2

Using the sample function, draw 10,000 random parameter values from the posterior distribution you calculated above. Use these samples to estimate the 50%, 89%, and 97% highest posterior density intervals.


```r
samples = sample(p_grid, prob = posterior, size = 1e4, replace = TRUE)
HPDI(samples, 0.5)
```

```
##      |0.5      0.5| 
## 0.5275275 0.5735736
```

```r
HPDI(samples, 0.89)
```

```
##     |0.89     0.89| 
## 0.5005005 0.6116116
```

```r
HPDI(samples, 0.97)
```

```
##     |0.97     0.97| 
## 0.4774775 0.6276276
```

### 3H3

Use rbinom to simulate 10,000 replicates of 200 births. You should end up with 10,000 numbers, each one a count of boys out of 200 births. Compare the distribution of predicted numbers of boys to the actual count in the data (111 boys out of 200 births). There are many good ways to visualize the simulations, but the dens command (part of the rethinking package) is probably the easiest way in this case. Does it look like the model fits the data well? That is, does the distribution of predictions include the actual observation as a central, likely outcome?


```r
preds = rbinom(1e4, size = 200, prob = samples)
dens(preds)
```

![](Chapter3_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

```r
mean(preds)
```

```
## [1] 110.8746
```

```r
median(preds)
```

```
## [1] 111
```

The observation (111) is a reasonable outcome of the model.

### 3H4

Now compare 10,000 counts of boys from 100 simulated firstborns only to the number of boys in the first births, birth1. How does the model look in this light?


```r
preds_fb = rbinom(1e4, size = 100, prob = samples)
dens(preds_fb)
```

![](Chapter3_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

```r
mean(preds_fb)
```

```
## [1] 55.5437
```

```r
median(preds_fb)
```

```
## [1] 56
```

```r
sum(birth1)
```

```
## [1] 51
```

```r
HPDI(preds_fb, 0.95)
```

```
## |0.95 0.95| 
##    43    66
```

The observation isn't an implausible outcome of the model, but is not central.


### 3H5





