# MCMC implementation of Pan & Nagurney paper

step.MCMC <- function(pop, utility, proposal.dist, move.cost) {
  # Params:

  # pop: vector of populations
  # utility: list of utility functions
  # proposal.dist: a function that takes three parameters:
  #   value, value conditioned on, and mode;
  #   mode can take the value 'r' or 'd'. The former draws from the distribution while
  #   the latter returns the probability of a value according to the proposal.
  # move.cost: list of move cost functions b/w regions (assumes flow is uni-directional)

  # returns population counts for each region

  # pan-nagurney utility-cost model helper function
  pn.prob <- function(src.utility, dest.utility, cost) {
    (dest.utility - cost) / src.utility
  }

  # randomly select source and destination regions
  num.regions <- length(pop)
  rand.ints <- sample(1:num.regions, 2, FALSE)
  region.src <- rand.ints[1]
  region.dest <- rand.ints[2]

  # create pop, utility, move cost lists for src and dest regions
  p <- list(src=pop[region.src], dest=pop[region.dest])
  u <- list(src=utility[[region.src]], dest=utility[[region.dest]])
  m <- move.cost[[region.src]][[region.dest]]

  # generate flow from proposal distribution
  f <- proposal.dist(p$src, 0, 'r')

  # calculate whether to accept this step using M-H
  cost <- m(f)
  prob0 <- pn.prob(u$src(p$src), u$dest(p$dest), 0)  # prob before move
  prob1 <- pn.prob(u$src(p$src - f), u$dest(p$dest + f), cost)  # prob after
  prop.num <- proposal.dist(p$src, p$src - f, 'd')
  prop.denom <- proposal.dist(p$src - f, p$src, 'd')

  # a <- min(1, (prob1 * prop.num) / (prob0 * prop.denom))
  a <- min(1, prob1 * (prop.num / prop.denom))
  if ((a > 1 | runif(1) < a) & (p$src - f > 0)) {
    pop[region.src] <- p$src - f
    pop[region.dest] <- p$dest + f
  }

  return(pop)
}
