# prior predictive check

# height data example
library(rethinking)
data(Howell1)
d <- Howell1[Howell1$age<18,]
# center weight
mean_weight <- mean(d$weight)
d$weight_c <- d$weight - mean_weight

# model 4H2 example

# sample intercept = average height at average weight (since centred)
a1 <- rnorm(100, 108, 64)
a2 <- rnorm(100, 100, 10)
hist(a1, 10, freq = F,
     main = "Normal(108, 64)",
     xlab = "a") # too broad
hist(a2, 10, freq = F,
     main = "Normal(100, 10)",
     xlab = "a")

# sample slope = cm increase in height for kg increase in weight
library(extraDistr)
b1 <- rnorm(100, 0, 7)
b2 <- rhnorm(100, 2) # half-normal distribution, can be implemented in brms via bounds
hist(b1, 10, freq = F,
     main = "Normal(0, 7)",
     xlab = "b") # unrealistic negative slopes
hist(b2, 10, freq = F,
     main = "Half-Normal(0, 2)",
     xlab = "b")

# Note, having the mass of the prior for slopes around zero counters over-fitting
#  as we need at least moderate evidence (data) to pull the posterior away from zero ("no effect")

# sample sigma = standard deviation of height around mean
sigma1 <- rexp(100, 0.04)
sigma2 <- rexp(100, 0.2)
hist(sigma1, 10, freq = F,
     main = "Exponential(0.04)",
     xlab = "sigma") # a bit broad
hist(sigma2, 10, freq = F,
     main = "Exponential(0.2)",
     xlab = "sigma")

# simulate average height with priors
# compare McElreath R code 5.4
w_new <- seq(0, 50, 1) # new weight data (un-centred)
# Note, since it's a line we would only need 2 points here, but I simulate a grid for later
w_c_new <- w_new-mean_weight # centred
mean_h <- cbind(rep(1, length(w_new)), w_c_new) %*% t(cbind(a2, b2)) # matrix formulation
# plot 100 simulations on top of each other
plot( NULL, xlim=c(0,50), ylim=c(0,200),
      xlab = "weight",
      ylab = "height",
      main = "a~Normal(100, 10);\n b~Half-Normal(0, 2)")
for (i in 1:100) lines(w_new, mean_h[,i], col=rgb(0,0,0,.2)) # on un-centred weight scale
# plot alternatively with abline()
plot( NULL, xlim=c(-20,30), ylim=c(0,200),
      xlab = "centred weight",
      ylab = "height",
      main = "a~Normal(100, 10);\n b~Half-Normal(0, 2)")
for (i in 1:100) abline(a2[i], b2[i], col=rgb(0,0,0,alpha=.2)) # centred weight scale

# simulate actual height ("prediction")
plot( NULL, xlim=c(0,50), ylim=c(0,200),
      xlab = "weight",
      ylab = "height",
      main = "a~Normal(100, 10);\n b~Half-Normal(0, 2);\n sigma~Exponential(0.2)")
for (i in 1:100) {
  h <- rnorm(length(w_new), mean_h[,i], sigma2[i])
  points(w_new, h, col=rgb(0,0,0,alpha=.1))
}