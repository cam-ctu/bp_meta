library(tidyverse)
library(nlme)
library(rstan)
library(bayesplot)

df <- read.csv(file="Central SBP in young ISH_v2.csv") %>% 
  cctu::clean_names() %>% 
  mutate(se=sd/sqrt(n),
         weights=se^2
         )
summary(df)

m0 <- gls(mean~group, weights=~weights, data=df,control=glsControl(sigma=1)) 
m0 %>%  summary

# This model is wrong
#m1 <- gls(mean~group, weights=~weights, data=df) 
#m1 %>% summary

df_stan <- list(
  N = nrow(df),
  y=df$mean,
  x=1*(df$group=="optimal"),
  se=df$se,
  sample=df$n
)


fit0 <- stan(
  file = "m0.stan",  # Stan program
  data = df_stan,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 2000,            # total number of iterations per chain
  cores = 2,              # number of cores (could use one per chain)
  refresh = 100             # no progress shown
)
  
fit1 <- stan(
  file = "m1.stan",  # Stan program
  data = df_stan,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 2000,            # total number of iterations per chain
  cores = 2,              # number of cores (could use one per chain)
  refresh = 100             # no progress shown
)
save.image("fits.Rdata")
load("fits.Rdata")

fit2 <- stan(
  file = "m2.stan",  # Stan program
  data = df_stan,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 2000,            # total number of iterations per chain
  cores = 2,              # number of cores (could use one per chain)
  refresh = 100             # no progress shown
)

summary(fit2, pars=c("mu","delta","sigma_within","sigma_between"))

renv::install("bayesplot")
library(bayesplot)
mcmc_areas(fit3,pars=my_pars)
mcmc_areas(fit3,pars="sigma_within")
mcmc_areas(fit3,pars="delta")
traceplot(fit3, pars=my_pars)
summary(fit3, c(my_pars,"sigma_within_se"))
summary(fit2, pars=c("mu","delta","sigma_within","sigma_between"))



fit4 <- stan(
  file = "m4.stan",  # Stan program
  data = df_stan,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 2000,          # number of warmup iterations per chain
  iter = 10000,            # total number of iterations per chain
  cores = 2,              # number of cores (could use one per chain)
  refresh = 500             # no progress shown
)

pairs(fit4, pars=c("mu","delta","nu","sigma_within","sigma_within_se"))
traceplot(fit4, pars=c("mu","delta","nu","sigma_within","sigma_within_se"))
traceplot(fit2, pars=c("mu","delta","sigma_within","sigma_between"))
mcmc_areas(fit4,pars="delta")

summary(fit0, pars=c("mu","delta"))$summary
summary(fit1, pars=c("mu","delta","sigma_within"))$summary
summary(fit2, pars=c("mu","delta","sigma_within", "sigma_between"))$summary
summary(fit4, pars=c("mu","delta","sigma_within","sigma_between","sigma_within_se","sigma_between" ))$summary

qgamma(c(0.025,0.975), shape=10*0.5, rate=0.5)
