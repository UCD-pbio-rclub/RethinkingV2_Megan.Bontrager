# Chapter 5 book code

# Working through chapter

# 5.1

# R code 5.1

library(rethinking)

# Load data
data(WaffleDivorce)
d = WaffleDivorce

# Standardize variables
summary(d$MedianAgeMarriage)
summary(d$Divorce)
d$A = scale(d$MedianAgeMarriage)
d$D = scale(d$Divorce)

## R code 5.2
sd(d$MedianAgeMarriage)

## R code 5.3
m5.1 = quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bA * A,
    a ~ dnorm(0, 0.2),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d)

## R code 5.4
set.seed(10)

# Extract samples from the prior
prior = extract.prior(m5.1)
precis(prior)

# Estimate mu = slope of relationship if sampling directly from prior
# Calculate from the model given the parameter samples in the prior
mu = link(m5.1, post = prior, data = list(A = c(-2,2)))

plot(NULL, xlim=c(-2,2), ylim=c(-2,2))

for (i in 1:50) lines(c(-2,2), mu[i,], col=col.alpha("black",0.4))

```

```{r}

## R code 5.5
# compute percentile interval of mean
A_seq = seq(from=-3, to=3.2, length.out=30)
mu = link(m5.1, data=list(A=A_seq))
mu.mean = apply(mu, 2, mean)
mu.PI = apply(mu, 2, PI)

# plot it all
plot(D ~ A, data=d, col=rangi2)
lines(A_seq, mu.mean, lwd=2)
shade(mu.PI, A_seq)

## R code 5.6
d$M = scale(d$Marriage)
m5.2 = quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu = a + bM * M,
    a ~ dnorm(0, 0.2),
    bM ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d)

```



```{r}
## R code 5.7
install.packages('dagitty')
library(dagitty)
dag5.1 = dagitty("dag {
    A -> D
    A -> M
    M -> D
}")
coordinates(dag5.1) = list(x=c(A=0,D=1,M=2), y=c(A=0,D=1,M=0))
plot(dag5.1)

## R code 5.8
m5.3 = quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu = a + bM*M + bA*A,
    a ~ dnorm(0, 0.2),
    bM ~ dnorm(0, 0.5),
    bA ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d)
precis(m5.3)

## R code 5.9
plot(coeftab(m5.1,m5.2,m5.3), par=c("bA","bM"))

## R code 5.10
N = 50 # number of simulated States
age = rnorm(N)        # sim A
mar = rnorm(N, age)  # sim A -> M
div = rnorm(N, age)  # sim A -> D

## R code 5.11
m5.4 = quap(
  alist(
    M ~ dnorm(mu, sigma),
    mu = a + bAM * A,
    a ~ dnorm(0, 0.2),
    bAM ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d)

## R code 5.12
mu = link(m5.4)
mu_mean = apply(mu, 2, mean)
mu_resid = d$M - mu_mean

## R code 5.13
# prepare new counterfactual data
M_seq = seq(from=-2, to=3, length.out=30)
pred_data = data.frame(M = M_seq, A = 0)

# compute counterfactual mean divorce (mu)
mu = link(m5.3, data=pred_data)
mu_mean = apply(mu, 2, mean)
mu_PI = apply(mu, 2, PI)

# simulate counterfactual divorce outcomes
D_sim = sim(m5.3, data=pred_data, n=1e4)
D_PI = apply(D_sim, 2, PI)

# display predictions, hiding raw data with type="n"
plot(D ~ M, data=d, type="n")
mtext("Median age marriage (std) = 0")
lines(M_seq, mu_mean)
shade(mu_PI, M_seq)
shade(D_PI, M_seq)

## R code 5.14
# call link without specifying new data
# so it uses original data
mu = link(m5.3)

# summarize samples across cases
mu_mean = apply(mu, 2, mean)
mu_PI = apply(mu, 2, PI)

# simulate observations
# again no new data, so uses original data
D_sim = sim(m5.3, n=1e4)
D_PI = apply(D_sim, 2, PI)

## R code 5.15
plot(mu_mean ~ d$D, col=rangi2, ylim=range(mu_PI),
     xlab="Observed divorce", ylab="Predicted divorce")
abline(a=0, b=1, lty=2)
for (i in 1:nrow(d)) lines(rep(d$D[i],2), mu_PI[,i], col=rangi2)

## R code 5.16
identify(x=d$D, y=mu_mean, labels=d$Loc)

## R code 5.17
N = 100                         # number of cases
x_real = rnorm(N)             # x_real as Gaussian with mean 0 and stddev 1
x_spur = rnorm(N, x_real)    # x_spur as Gaussian with mean=x_real
y = rnorm(N, x_real)         # y as Gaussian with mean=x_real
d = data.frame(y,x_real,x_spur) # bind all together in data frame

## R code 5.18
library(rethinking)
data(milk)
d = milk
str(d)

## R code 5.19
d$K = scale(d$kcal.per.g)
d$N = scale(d$neocortex.perc)
d$M = scale(log(d$mass))

## R code 5.20
m5.5_draft = quap(
  alist(
    K ~ dnorm(mu, sigma),
    mu = a + bN*N,
    a ~ dnorm(0, 1),
    bN ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data=d)

## R code 5.21
d$neocortex.perc

## R code 5.22
dcc = d[ complete.cases(d$K,d$N,d$M), ]

## R code 5.23
m5.5_draft = quap(
  alist(
    K ~ dnorm(mu, sigma),
    mu = a + bN*N,
    a ~ dnorm(0, 1),
    bN ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data=dcc)

## R code 5.24
prior = extract.prior(m5.5_draft)
xseq = c(-2,2)
mu = link(m5.5_draft, post=prior, data=list(N=xseq))
plot(NULL, xlim=xseq, ylim=xseq)
for (i in 1:50) lines(xseq, mu[i,], col=col.alpha("black",0.3))

## R code 5.25
m5.5 = quap(
  alist(
    K ~ dnorm(mu, sigma),
    mu = a + bN*N,
    a ~ dnorm(0, 0.2),
    bN ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data=dcc)

## R code 5.26
precis(m5.5)

## R code 5.27
xseq = seq(from=min(dcc$N)-0.15, to=max(dcc$N)+0.15, length.out=30)
mu = link(m5.5, data=list(N=xseq))
mu_mean = apply(mu,2,mean)
mu_PI = apply(mu,2,PI)
plot(K ~ N, data=dcc)
lines(xseq, mu_mean, lwd=2)
shade(mu_PI, xseq)

## R code 5.28
m5.6 = quap(
  alist(
    K ~ dnorm(mu, sigma),
    mu = a + bM*M,
    a ~ dnorm(0, 0.2),
    bM ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data=dcc)
precis(m5.6)

## R code 5.29
m5.7 = quap(
  alist(
    K ~ dnorm(mu, sigma),
    mu = a + bN*N + bM*M,
    a ~ dnorm(0, 0.2),
    bN ~ dnorm(0, 0.5),
    bM ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data=dcc)
precis(m5.7)

## R code 5.30
plot(coeftab(m5.5, m5.6, m5.7), pars=c("bM","bN"))

## R code 5.31
xseq = seq(from=min(dcc$M)-0.15, to=max(dcc$M)+0.15, length.out=30)
mu = link(m5.7, data=data.frame(M=xseq, N=0))
mu_mean = apply(mu,2,mean)
mu_PI = apply(mu,2,PI)
plot(NULL, xlim=range(dcc$M), ylim=range(dcc$K))
lines(xseq, mu_mean, lwd=2)
shade(mu_PI, xseq)

## R code 5.32
# M -> K = N
# M -> N
n = 100
M = rnorm(n)
N = rnorm(n, M)
K = rnorm(n, N - M)
d_sim = data.frame(K=K,N=N,M=M)

## R code 5.33
# M -> K = N
# N -> M
n = 100
N = rnorm(n)
M = rnorm(n, N)
K = rnorm(n, N - M)
d_sim2 = data.frame(K=K,N=N,M=M)

# M -> K = N
# M = U -> N
n = 100
U = rnorm(n)
N = rnorm(n, U)
M = rnorm(n, U)
K = rnorm(n, N - M)
d_sim3 = data.frame(K=K,N=N,M=M)

## R code 5.34
data(Howell1)
d = Howell1
str(d)

## R code 5.35
mu_female = rnorm(1e4,178,20)
mu_male = rnorm(1e4,178,20) + rnorm(1e4,0,10)
precis(data.frame(mu_female, mu_male))

## R code 5.36
d$sex = ifelse(d$male==1, 2, 1)
str(d$sex)

## R code 5.37
m5.8 = quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu = a[sex],
    a[sex] ~ dnorm(178, 20),
    sigma ~ dunif(0, 50)
  ), data=d)
precis(m5.8, depth=2)

## R code 5.38
post = extract.samples(m5.8)
post$diff_fm = post$a[,1] - post$a[,2]
precis(post, depth=2)

## R code 5.39
data(milk)
d = milk
unique(d$clade)

## R code 5.40
d$clade_id = as.integer(d$clade)

## R code 5.41
d$K = scale(d$kcal.per.g)
m5.9 = quap(
  alist(
    K ~ dnorm(mu, sigma),
    mu = a[clade_id],
    a[clade_id] ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data=d)
labels = paste("a[", 1:4, "]:", levels(d$clade), sep="")
plot(precis(m5.9, depth=2, pars="a"), labels=labels,
     xlab="expected kcal (std)")

## R code 5.42
set.seed(63)
d$house = sample(rep(1:4,each=8), size=nrow(d))

## R code 5.43
m5.10 = quap(
  alist(
    K ~ dnorm(mu, sigma),
    mu = a[clade_id] + h[house],
    a[clade_id] ~ dnorm(0, 0.5),
    h[house] ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data=d)


