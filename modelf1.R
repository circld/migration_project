# MCMC implementation of Pan & Nagurney paper

step.MCMC <- function(pop, capacity, move.cost) {
  # Params:

  # pop: vector of populations
  # capacity: list of capacities for utility functions
  #   Assumes all utility functions are of the form u = k - p
  # move.cost: matrix of costs for 1 person to move

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
  u <- list(src=capacity[region.src] - p$src, dest=capacity[region.dest] - p$dest + 1)

  # calculate whether to accept this step using M-H
  cost <- move.cost[region.src,region.dest]

  # calculate a1

  a1 <- (u$dest - cost)/u$src

  # calculate a2

  lpop <- length(pop)
  lpop0 <- length(pop[pop != 0])
  lpop0m1 <- lpop0-1
  lpop0p1 <- lpop0+1

  if(p[1] == 1 && p[2] == 0){
    a2 <- 1
  } else if(p[1] == 1){
    qxxnew <- 1/(lpop0*(lpop-1))
    qxnewx <- 1/(lpop0m1*(lpop-1))
    a2 <- qxxnew/qxnewx
  } else if(p[2] == 0){
    qxxnew <- 1/(lpop0*(lpop-1))
    qxnewx <- 1/(lpop0p1*(lpop-1))
    a2 <- qxxnew/qxnewx
  } else{
    a2 <- 1
  }

  a <- min(1, a1*a2)

  if(runif(1) < a && p$src > 1){
    pop[region.src] <- p$src - 1
    pop[region.dest] <- p$dest + 1
  }

  return(pop)
}
