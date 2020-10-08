library(rstan)
library(tidyverse)
library(tidybayes)
library(broom)
library(plyr)

# Load data ----

germ = read_csv("light_round1_tall_well.csv") 
# %>% 
  # filter(wps == 0)

stdi = germ %>% 
  filter(pops == "STDI")

# Make df with maximum germination acheived during expt

totals = stdi %>% 
  group_by(pops, temps, wps, total_seeds, tray, well) %>% 
  dplyr::summarize(total_germ = max(cumulative_germ)) %>% 
  mutate(prop_germ = total_germ/total_seeds) %>% 
  # Center temperature 22.5 = middle temp (5+40)/2 
  mutate(temps = temps - 22.5)


# Look at data ----

summary(stdi)
summary(totals)

ggplot(totals, aes(x = temps, y = prop_germ, color = as.factor(wps))) +
  geom_jitter(alpha = 0.5, width = 0.5, height = 0.05) +
  geom_smooth()


# Fit gaussian curve to max germ ----

totals_stan = compose_data(totals)

# Stan code is in htg_gaussian.stan

mod_gaus = stan("htg_gaussian.stan",
                data = totals_stan, 
                chains = 4, iter = 2000, thin = 1)

# Model formula is:
# mu = exp(b0 + b_w * wps[i]) * exp(-(temps[i] - b1_t) ^ 2 / b2_t)

# b_w = effect of water potential, on data scale, exp(bw * wps)
# b0 = intercept when centered temp = 0
# b1_t = temperature optima (relative to centered 0)
# b2_t = temperature response breadth = variance^2

pars = c("b0", "b1_t", "b2_t", "b_w")

tidy(mod_gaus, pars = pars, conf.int = TRUE, rhat = TRUE, ess = TRUE)

# Germ at t = 0
# For wp = 0
exp(-0.306 + 2.97 *0)
# For wp = 0
exp(-0.306 + 2.97 *-0.3)
# For wp = 0
exp(-0.306 + 2.97 *-0.6)

sqrt(72.1)
# sd = 8.5, seems ok


# Plot parameters ----

plot_pars = mod_gaus %>% 
  rstan::extract(pars = pars)
niter <- length(plot_pars[[1]])

# Clunky, could be streamlined for sure
plot_dat = tibble(
  temps = rep(seq(-17.5, 17.5, length.out = 1e2), niter)) %>%
  mutate(b0 = rep(plot_pars$b0, each = nrow(.) / niter),
         b1_t = rep(plot_pars$b1_t, each = nrow(.) / niter),
         b2_t = rep(plot_pars$b2_t, each = nrow(.) / niter),
         b_w = rep(plot_pars$b_w, each = nrow(.) / niter),                 
         predicted_germ_0 = exp(b0 + b_w*0 -(((temps - b1_t) ^ 2) / b2_t)),                 
         predicted_germ_0.3 = exp(b0 + b_w*-0.3 -(((temps - b1_t) ^ 2) / b2_t)),                 
         predicted_germ_0.6 = exp(b0 + b_w*-0.6 -(((temps - b1_t) ^ 2) / b2_t))) %>% 
  ddply(.(temps), summarize,
        median_0 = median(predicted_germ_0),
        lower95_0 = HDInterval::hdi(predicted_germ_0, credMass = 0.95)[1],
        upper95_0 = HDInterval::hdi(predicted_germ_0, credMass = 0.95)[2],
        median_0.3 = median(predicted_germ_0.3),
        lower95_0.3 = HDInterval::hdi(predicted_germ_0.3, credMass = 0.95)[1],
        upper95_0.3 = HDInterval::hdi(predicted_germ_0.3, credMass = 0.95)[2],
        median_0.6 = median(predicted_germ_0.6),
        lower95_0.6 = HDInterval::hdi(predicted_germ_0.6, credMass = 0.95)[1],
        upper95_0.6 = HDInterval::hdi(predicted_germ_0.6, credMass = 0.95)[2])

opt_temp_int = mod_gaus %>% 
  tidy("b1_t", estimate.method = "median", conf.int = TRUE, conf.level = 0.95, conf.method = "HPDinterval")

plot_dat_tall = pivot_longer(plot_dat, cols = -temps, values_to = "estimate", names_to = "type") %>% 
  separate(type, into = c("type", "wps"), sep = "_") %>% 
  pivot_wider(id_cols = c(temps, wps), names_from = type, values_from = estimate) %>% 
  mutate(temps = temps,
         wps = as.factor(0-as.numeric(wps)))

ggplot() +
  geom_line(data = plot_dat_tall, aes(x = temps + 22.5, y = median, color = wps)) +
  geom_ribbon(data = plot_dat_tall, aes(x = temps + 22.5, ymin = lower95, ymax = upper95, fill = wps), alpha = 0.3) + 
  geom_pointrange(data = opt_temp_int, aes(y = 0.9, xmin = conf.low + 22.5, xmax = conf.high + 22.5, x = estimate + 22.5)) +
  geom_jitter(data = totals, aes(x = temps + 22.5, y = prop_germ, color = as.factor(wps)), alpha = 0.5, width = 0.5, height = 0.01) +
  labs(x = "Temperature", y = "Proportion germinated", color = "Water\npotential", fill = "Water\npotential")

# Seems like it might be overestimating germ at wp - 0.6, perhaps non-linear effects of water potential

# Next steps: 
# - estimate some sort of breadth where >50% of seeds germinate? but maybe b2_t acheives this.
# - we designed experiment to catch temperatures where germination would be near 0. So is this going to drive breadth estimates since they will be ~ anchored at 0 at 5 and 40?
# - asymmetry in curve
# - estimate parameters for each population


# Now with time: trying 3 parameter logistic ----

germ_avg = read_csv("light_round1_tall.csv") %>% 
  filter(wps == 0, pops == "STDI")

ggplot(germ_avg, aes(x = day, y = cumulative_prop_germ, color = as.factor(temps))) +
  geom_point() +
  geom_line()

# With replication across wells

germ_0 = stdi %>% 
  filter(wps == 0) %>% 
  # Yikes, some props way above 1. Need to look at raw data. Correct for now.
  mutate(cumulative_prop_germ = if_else(cumulative_prop_germ >1, 1, cumulative_prop_germ))
summary(germ_0)

ggplot(germ_0, aes(x = day, y = cumulative_prop_germ, color = as.factor(temps))) +
  geom_point(alpha = 0.2) +
  geom_smooth()

temp_fact = germ_0 %>%
  select(temps, cumulative_prop_germ, day) %>% 
  mutate(temps = as.factor(temps)) %>% compose_data()
  
mod_3pl = stan("htg_3pl.stan",
               data = temp_fact, 
               chains = 4, iter = 2000, thin = 1)

pars = c("b1_t", "b2_t", "b3_t")
tidy(mod_3pl, pars = pars, conf.int = TRUE, rhat = TRUE, ess = TRUE)


# Convert to one seed per row with Julin's code

one_per_row <- function(df) {
  total_seed <- max(df$total_seeds, sum(df$germ))
  newdata <- tibble(id=1:total_seed, germ=0, day=max(df$day))
  df <- df %>% filter(germ>0)
  count <- 1
  if (nrow(df) > 0) {
    for (i in 1:nrow(df)) { # we look at each row of the df where germination occured
      for (j in 1:df$germ[i]) { # now update the newdata to reflect the germiantion of each seed
        newdata$germ[count] <- 1
        newdata$day[count]=df$day[i]
        count <- count+1 # count keeps track of which individual we are at in the new data
      } # for j
    } # for i
  } # if 
  return(newdata)
}

germone <- stdi %>% group_by(temps) %>%
  dplyr::select(-cumulative_germ) %>% # not needed in this encoding (I think...in any case would need to be recalculated)
  nest() %>%
  mutate(newdata=map(data, one_per_row)) %>%
  select(-data) %>%
  unnest(newdata)

germone






# Incorporating asymmetry into curve ----
# NOT DONE BELOW HERE

# This doesn't seem super important for diversifolius, but probably is for other species

# First, skew normal

mod_emg = stan("htg_weibull.stan",
                data = totals_stan, 
                chains = 4, iter = 2000, thin = 1)


# b_w = effect of water potential, on data scale, exp(bw * wps)
# b0 = intercept when centered temp = 0
# b1_t = temperature optima (relative to centered 0)
# b2_t = temperature response breadth = variance^2

pars = c("b0", "b1_t", "b2_t", "b_w")

tidy(mod_gaus, pars = pars, conf.int = TRUE, rhat = TRUE, ess = TRUE)




# Also trying a weibull model
# Angilletta, estimating and comparing TPC

# Stan code is in htg_weibull.stan
# Work in progress, not ready to fit yet

mod_weib = stan("htg_weibull.stan",
                data = totals_stan, 
                chains = 4, iter = 2000, thin = 1, init)


