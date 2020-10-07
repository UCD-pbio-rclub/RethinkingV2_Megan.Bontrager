library(tidyverse)
library(rethinking)

dat = read_csv("transplant_data.csv") %>% 
  filter(site == "AQ") %>% 
  filter(!is.na(nov_germ)) %>% 
  separate(blk, 2, into = c("drop", "blk")) %>% 
  mutate(blk = as.numeric(blk)) %>%
  filter(type == "WI") %>% 
  filter(blk %in% c(1, 2, 3, 4)) %>% 
  select(temperature_diff_fall = abs_tave_diff_sep_nov_scaled, blk, nov_germ, pop = dampop, dam, sire, nov_size, mar_surv, mar_size, fruit_count, total_est_seeds, temperature_diff_annual = abs_tave_diff_sep_jul_scaled, precip_diff_spring = abs_ppt_mm_diff_midparent_apr_jul, first_fl_julian, last_fl_julian)

write.csv(dat, "clarkia_transplant_data.csv", row.names = FALSE)

dat_nov_germ = list(
  nov_germ = dat$nov_germ,
  blk = dat$blk,
  temperature_diff = dat$temperature_diff_fall,
  pop = as.integer(as.factor(dat$pop))
)

summary(dat_nov_germ$temperature_diff)

# Model with temperature only
m0 = ulam(
  alist(
    nov_germ ~ dbinom(1, p),
    logit(p) <- a + bT*temperature_diff,
    a ~ dnorm(0, 1.5),
    bT ~ dnorm(0, 0.5)
  ), data = dat_nov_germ, cores = 2, chains = 2, iter = 2000, log_lik = TRUE)

prior = extract.prior(m0)
mu = link(m0, post = prior, data = list(temperature_diff = seq(-1.5, 2, by = 0.5)))

mu_plot = data.frame(mu) %>% 
  mutate(row = row_number()) %>% 
  pivot_longer(cols = X1:X8, names_to = "key", values_to = "value") %>% 
  mutate(temp_diff = (as.numeric(str_remove(key, "X")) - 4)/2)
  

ggplot(mu_plot, aes(x = temp_diff, y = value, group = row)) + 
  geom_line(alpha = 0.2)

precis(m0, depth = 2)

# Model with block and no pooling
m0.5 = ulam(
  alist(
    nov_germ ~ dbinom(1, p),
    logit(p) <- a + bT*temperature_diff + bB[blk],
    a ~ dnorm(0, 1.5),
    bT ~ dnorm(0, 0.5),
    bB[blk] ~ dnorm(0, 0.5)
  ), data = dat_nov_germ, cores = 2, chains = 2, iter = 2000, log_lik = TRUE)

precis(m0.5, depth = 2)


# Model with block and pooling
m1 = ulam(
  alist(
    nov_germ ~ dbinom(1, p),
    logit(p) <- a + bT*temperature_diff + bB[blk],
    a ~ dnorm(0, 1.5),
    bT ~ dnorm(0, 0.5),
    bB[blk] ~ dnorm(0, sigma),
    sigma ~ dexp(1)
  ), data = dat_nov_germ, cores = 2, chains = 2, iter = 2000, log_lik = TRUE)

precis(m1, depth = 2)

# Model with population and pooling
m2 = ulam(
  alist(
    nov_germ ~ dbinom(1, p),
    logit(p) <- a + bT*temperature_diff + bP[pop],
    a ~ dnorm(0, 1.5),
    bT ~ dnorm(0, 0.5),
    bP[pop] ~ dnorm(0, sigma),
    sigma ~ dexp(1)
  ), data = dat_nov_germ, cores = 2, chains = 2, iter = 2000, log_lik = TRUE)

precis(m2, depth = 2)


# Model with block and population
m3 = ulam(
  alist(
    nov_germ ~ dbinom(1, p),
    logit(p) <- a + bT*temperature_diff + bB[blk] + bP[pop],
    a ~ dnorm(0, 1.5),
    bT ~ dnorm(0, 0.5),
    bB[blk] ~ dnorm(0, sigma),
    bP[pop] ~ dnorm(0, sigma2),
    sigma ~ dexp(1),
    sigma2 ~ dexp(1)
  ), data = dat_nov_germ, cores = 2, chains = 2, iter = 2000, log_lik = TRUE)

precis(m3, depth = 2)

compare(m0, m0.5, m1, m2, m3)

