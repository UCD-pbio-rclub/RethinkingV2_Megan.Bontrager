# Chapter 11 book examples

library(rethinking)
data(chimpanzees)
d = chimpanzees

d$treatment = 1 + d$prosoc_left + 2*d$condition

table(d$treatment)
xtabs(~ treatment + prosoc_left + condition, d)

m11.1 = quap(
  alist(
    pulled_left ~ dbinom(1, p), 
    logit(p) <- a,
    a ~ dnorm(0, 1.5)      
  ), data = d)

set.seed(1999)
prior = extract.prior(m11.1, n = 1e4)

p = inv_logit(prior$a)
dens(p, adj = 0.1)

m11.2 = quap(
  alist(
    pulled_left ~ dbinom(1, p), 
    logit(p) <- a + b[treatment],
    a ~ dnorm(0, 1.5),
    b[treatment] ~ dnorm(0, 1)
  ), data = d)

set.seed(1999)
prior = extract.prior(m11.2, n = 1e4)

p = sapply(1:4, function(k) inv_logit(prior$a + prior$b[,k]))
dens(p[, 1] - p[,2], adj = 0.1)

dat_list = list(
  pulled_left = d$pulled_left,
  actor = d$actor,
  treatment = as.integer(d$treatment)
)

m11.4 = ulam(
  alist(
    pulled_left ~ dbinom(1, p), 
    logit(p) <- a[actor] + b[treatment],
    a[actor] ~ dnorm(0, 1.5),
    b[treatment] ~ dnorm(0, 0.5)
  ), data = dat_list, chains = 4, log_lik = TRUE)

precis(m11.4, depth = 2)

post = extract.samples(m11.4)
p_left = inv_logit(post$a)
plot(precis(as.data.frame(p_left)), xlim = c(0,1))

plot(precis(m11.4, depth = 2, pars = "b"))

diffs = list(db13 = post$b[,1] - post$b[,3], 
             db24 = post$b[,2] - post$b[,4])
plot(precis(diffs))

pl = by(d$pulled_left, list(d$actor, d$treatment), mean)
pl

stancode(m11.4)

# aggregate

d$side = d$prosoc_left + 1
d$cond = d$condition +1

library(tidyverse)
library(tidybayes)

d_aggregated = d %>% 
  group_by(treatment, actor, side, cond) %>% 
  summarize(left_pulls = sum(pulled_left))

d_aggregated

dat = compose_data(d_aggregated)
dat$n = NULL
dat

m11.6 = ulam(
  alist(
    left_pulls ~ dbinom(18, p),
    logit(p) <- a[actor] + b[treatment],
    a[actor] ~ dnorm(0, 1.5),
    b[treatment] ~ dnorm(0, 0.5)
  ), data = dat, chains = 4, log_lik = TRUE
  )
)
precis(m11.6, depth = 2)



data("UCBadmit")
d = UCBadmit

