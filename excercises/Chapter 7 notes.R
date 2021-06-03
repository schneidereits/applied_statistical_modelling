# Chapter 5 notes 
# 26.5.2021

# data ----
library(tidyverse)
library(ggrepel)
library(rcartocolor)
carto_pal(7, "BurgYl")


(d <- tibble(species = c("afarensis", "africanus", "habilis", "boisei", "rudolfensis", "ergaster", "sapiens"), 
           brain   = c(438, 452, 612, 521, 752, 871, 1350), 
           mass    = c(37.0, 35.5, 34.5, 41.5, 55.5, 61.0, 53.5)))
  
d %>%
  ggplot(aes(x =  mass, y = brain, label = species)) +
  geom_point() +
  geom_text_repel(size = 3, family = "Courier", seed = 438) +
  labs(subtitle = "Average brain volume by body\nmass for six hominin species",
       x = "body mass (kg)",
       y = "brain volume (cc)") +
  xlim(30, 65)

# standardizing
d <- d %>% 
  mutate(mass_std  = (mass - mean(mass)) / sd(mass),
         brain_std = brain / max(brain))

library(brms)


b7.1 <-  brm(data = d, 
      family = gaussian,
      brain_std ~ 1 + mass_std,
      prior = c(prior(normal(0.5, 1), class = Intercept),
                prior(normal(0, 10), class = b),
                prior(lognormal(0, 1), class = sigma)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 42)

summary(b7.1)


R2_is_bad <- function(brm_fit, seed = 42, ...) {
  
  set.seed(seed)
  p <- predict(brm_fit, summary = F, ...)
  r <- apply(p, 2, mean) - d$brain_std
  1 - rethinking::var2(r) / rethinking::var2(d$brain_std)
  
}

R2_is_bad(b7.1)


# quadratic
b7.2 <- 
  update(b7.1,
         newdata = d, 
         formula = brain_std ~ 1 + mass_std + I(mass_std^2),
         iter = 2000, warmup = 1000, chains = 4, cores = 4,
         seed = 7)

# cubic
b7.3 <- 
  update(b7.1,
         newdata = d, 
         formula = brain_std ~ 1 + mass_std + I(mass_std^2) + I(mass_std^3),
         iter = 2000, warmup = 1000, chains = 4, cores = 4,
         seed = 7,
         control = list(adapt_delta = .9))


# fourth-order
b7.4 <- 
  update(b7.1,
         newdata = d, 
         formula = brain_std ~ 1 + mass_std + I(mass_std^2) + I(mass_std^3) + I(mass_std^4),
         iter = 2000, warmup = 1000, chains = 4, cores = 4,
         seed = 7,
         control = list(adapt_delta = .995))

# fifth-order
b7.5 <- 
  update(b7.1,
         newdata = d, 
         formula = brain_std ~ 1 + mass_std + I(mass_std^2) + I(mass_std^3) + I(mass_std^4) + I(mass_std^5),
         iter = 2000, warmup = 1000, chains = 4, cores = 4,
         seed = 7,
         control = list(adapt_delta = .99999))

# sixth order
custom_normal <- custom_family(
  "custom_normal", dpars = "mu",
  links = "identity",
  type = "real"
)

stan_funs  <- "real custom_normal_lpdf(real y, real mu) {
  return normal_lpdf(y | mu, 0.001);
}
real custom_normal_rng(real mu) {
  return normal_rng(mu, 0.001);
}
" 

stanvars <- stanvar(scode = stan_funs, block = "functions")

b7.6 <- 
  brm(data = d, 
      family = custom_normal,
      brain_std ~ 1 + mass_std + I(mass_std^2) + I(mass_std^3) + I(mass_std^4) + I(mass_std^5) + I(mass_std^6),
      prior = c(prior(normal(0.5, 1), class = Intercept),
                prior(normal(0, 10), class = b)),
      iter = 2000, warmup = 1000, chains = 4, cores = 4,
      seed = 7,
      stanvars = stanvars)

expose_functions(b7.6, vectorize = TRUE)

posterior_epred_custom_normal <- function(prep) {
  mu <- prep$dpars$mu
  mu 
}

posterior_predict_custom_normal <- function(i, prep, ...) {
  mu <- prep$dpars$mu
  mu 
  custom_normal_rng(mu)
}

log_lik_custom_normal <- function(i, prep) {
  mu <- prep$dpars$mu
  y <- prep$data$Y[i]
  custom_normal_lpdf(y, mu)
}

make_figure7.3 <- function(brms_fit, ylim = range(d$brain_std)) {
  
  # compute the R2
  r2 <- R2_is_bad(brms_fit)
  
  # define the new data 
  nd <- tibble(mass_std = seq(from = -2, to = 2, length.out = 200))
  
  # simulate and wrangle
  fitted(brms_fit, newdata = nd, probs = c(.055, .945)) %>% 
    data.frame() %>% 
    bind_cols(nd) %>% 
    
    # plot!  
    ggplot(aes(x = mass_std)) +
    geom_lineribbon(aes(y = Estimate, ymin = Q5.5, ymax = Q94.5),
                    color = carto_pal(7, "BurgYl")[7], size = 1/2, 
                    fill = alpha(carto_pal(7, "BurgYl")[6], 1/3)) +
    geom_point(data = d,
               aes(y = brain_std),
               color = carto_pal(7, "BurgYl")[7]) +
    labs(subtitle = bquote(italic(R)^2==.(round(r2, digits = 2))),
         x = "body mass (std)",
         y = "brain volume (std)") +
    coord_cartesian(xlim = c(-1.2, 1.5),
                    ylim = ylim)
  
}

# plotting model outputs ----
p1 <- make_figure7.3(b7.1)
p2 <- make_figure7.3(b7.2)
p3 <- make_figure7.3(b7.3)
p4 <- make_figure7.3(b7.4, ylim = c(.25, 1.1))
p5 <- make_figure7.3(b7.5, ylim = c(.1, 1.4))
p6 <- make_figure7.3(b7.6, ylim = c(-0.25, 1.5)) +
  geom_hline(yintercept = 0, color = carto_pal(7, "BurgYl")[2], linetype = 2) 


library(patchwork)

((p1 | p2) / (p3 | p4) / (p5 | p6)) +
  plot_annotation(title = "Figure7.3. Polynomial linear models of increasing\ndegree for the hominin data.")