setwd(dirname(file.choose()))

# 2-region specific implementation
source('two_region_mcmc.R')

# multi-region, cost, utility, proposal distribution generalization
source('model.R')

# 2-region
u1 <- function(p) {
  return(-p[1]+8000)
}

u2 <- function(p) {
  return(-p[2]+14000)
}

c12 <- function(f) {
  return(2 * f[1])
}

c21 <- function(f) {
  return(2 * f[2])
}

p0 <- c(4000, 2000)

pdata <- data.frame(p1 = 4000,  p2 = 2000,  u1 = u1(p0),  u2 = u2(p0))

iterations <- 1:10000
for(i in iterations) {
  pdata <- pdatastep(pdata)
}

# do the population estimates converge?
plot(iterations, pdata$p1[-1], type = 'l', col = 'blue', ylim = c(0, 6000),
     main = '2-region', ylab = 'population', xlab = 'iteration')
lines(iterations, pdata$p2[-1], col = 'red')

# generalization
u1.new <- function(p) {
  return(-p + 8000)
}

u2.new <- function(p) {
  return(-p + 14000)
}

prop <- function(value, cond.value, mode) {
  if (mode == 'r') {
    return(10)
  } else if (mode == 'd') {
    return(1)
  }
}

c.new <- function(f) {
  return(2 * f)
}

# define parameter lists
u <- list(u1.new, u2.new)
c <- list(list(c.new, c.new), list(c.new, c.new))
p <- step.MCMC(p0, u, prop, c)

test <- matrix(NA, 10001, 2)
test[1, ] <- p
for(i in 2:10001) {
  p <- step.MCMC(p, u, prop, c)
  test[i,] <- p
}

plot(1:10001, test[, 2], 'l', col = 'red', ylim = c(0, 6000),
     main = 'generalization', ylab = 'population', xlab = 'iteration')
lines(1:10001, test[, 1], col = 'blue')
