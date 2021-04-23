dotR <- file.path(Sys.getenv("HOME"), ".R")
if (!file.exists(dotR)) dir.create(dotR)
M <- file.path(dotR, "Makevars")
if (!file.exists(M)) file.create(M)
cat("\nCXX14FLAGS += -O3 -mtune=native -arch x86_64 -ftemplate-depth-256",
    file = M, sep = "\n", append = FALSE)

remove.packages("rstan")
if (file.exists(".RData")) file.remove(".RData")

#install.packages("Rcpp", type = 'source')
#install.packages("StanHeaders", type="source")

install.packages(c("Rcpp", "RcppEigen", "RcppParallel", "StanHeaders"), type = "source")

install.packages("Rtools")

install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies = TRUE)
#install.packages("rstan")
library(rstan)

example(stan_model, package = "rstan", run.dontrun = TRUE)

# brms install ----

packages <- c("ape", "bayesplot", "brms", "broom", "dagitty", "devtools", "flextable", "GGally", "ggdag", "ggdark", "ggmcmc", "ggrepel", "ggthemes", "ggtree", "ghibli", "gtools", "loo", "patchwork", "psych", "rcartocolor", "Rcpp", "remotes", "rstan", "StanHeaders", "statebins", "tidybayes", "tidyverse", "viridis", "viridisLite", "wesanderson")

install.packages(packages, dependencies = T)

schools_dat <- list(J = 8, 
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))

fit <- stan(file = 'eight_schools.stan', data = schools_dat)

library(brms)

fit1 <- brm(count ~ zAge + zBase * Trt + (1|patient), 
            data = epilepsy, family = poisson())

b2.1 <-
  brm(data = list(w = 24), 
      family = binomial(link = "identity"),
      w | trials(36) ~ 0 + Intercept,
      prior(beta(1, 1), class = b, lb = 0, ub = 1),
      seed = 2)

print(b2.1)

posterior_summary(b2.1) %>% 
  round(digits = 2)
