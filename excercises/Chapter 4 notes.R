# chaper 4 code

# library ----
library(tidyverse)
library(brms)
library(rethinking)

# data ----

pos <- replicate( 1000 , sum( runif(16,-1,1) ) )
plot(density(pos))

data(Howell1)
d <- Howell1

rm(Howell1)
detach(package:rethinking, unload = T)

d %>%
  str()

d %>%
  pivot_longer(everything()) %>% 
  mutate(name = factor(name, levels = c("height", "weight", "age", "male"))) %>% 
  
  ggplot(aes(x = value)) +
  geom_histogram(bins = 10) +
  facet_wrap(~ name, scales = "free", ncol = 1)

d2 <- 
  d %>%
  filter(age >= 18)

# brms section 


b4.1 <- 
  brm(data = d2, 
      family = gaussian,
      height ~ 1,
      prior = c(prior(normal(178, 20), class = Intercept),
                prior(uniform(0, 50), class = sigma)),
      iter = 31000, warmup = 30000, chains = 4, cores = 4,
      seed = 4)

summary(b4.1)
plot(b4.1)

# McElreath’s uniform prior for  σ was rough on brms. It took an unusually-large
# number of warmup iterations before the chains sampled properly. As McElreath covered 
# in Chapter 9, Hamiltonian Monte Carlo (HMC) tends to work better when you default to an 
# exponential or half Cauchy for  σ. Here’s how to do so with the half Cauchy.1
b4.1_hc <- 
  brm(data = d2, 
      family = gaussian,
      height ~ 1,
      prior = c(prior(normal(178, 20), class = Intercept),
                # the magic lives here
                prior(cauchy(0, 1), class = sigma)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 4)

summary(b4.1_hc)
b4.1_hc$fit
plot(b4.1_hc)

# model with the very-narrow-μ-prior
b4.2 <- 
  brm(data = d2, 
      family = gaussian,
      height ~ 1,
      prior = c(prior(normal(178, 0.1), class = Intercept),
                prior(uniform(0, 50), class = sigma)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 4)

plot(b4.2)
summary(b4.2)


rbind(summary(b4.1_hc)$fixed,
      summary(b4.2)$fixed)

# sampling from the brms fit 

post <- posterior_samples(b4.1_hc)

head(post)

select(post, b_Intercept:sigma) %>% 
  cov()
