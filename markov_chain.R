# First, how it runs towards equilibrium as a markov chain:

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

flowcalc <- function(p) {
  if (u2(p) > u1(p)) {
    f1 <- (p[1] - p[2] + 6000) / 4
    f <- c(f1, 0)
  } else if (u1(p) > u2(p)) {
    f2 <- (p[2] - p[1] - 6000) / 4
    f <- c(0, f2)
  } else {
    f <- c(0, 0)
  }

  return(f)
}

pdata <- data.frame(p1 = 4000,  p2 = 2000,  u1 = u1(p0),  u2 = u2(p0))

for(i in 1:10) {
  p <- c(tail(pdata$p1, 1), tail(pdata$p2, 1))
  f <- flowcalc(p)
  u <- c(u1(p), u2(p))
  totu <- sum(p * u)

  pdata <- rbind(pdata,  c(p[1]-f[1]+f[2],  p[2]+f[1]-f[2],  u))
}
