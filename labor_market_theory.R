# set up simulation parameters
totalPop <- 10000

capacity <- c(5000, 5000)

slack <- c(4000, 4000)

capjobs <- 0.98 * capacity

UE <- 1 - (capjobs / capacity)

avgUE <- sum((capacity / totalPop) * UE)


marginaljobs <- function(p,  capacity,  slack) {
  return((capacity - p) / (capacity - slack))
}

intmarginaljobs <- function(p,  capacity,  slack) {
  imj <- (2 * capacity * p - p^2) / (2 * capacity - 2 * slack)
  return(imj)
}

numjobsraw <- function(p1, p2, capacity, slack) {
  nj <- intmarginaljobs(p2, capacity, slack) -
    intmarginaljobs(p1, capacity, slack)
  return(nj)
}

numjobs <- function(p1, p2, capacity, slack) {
  if (p1 < slack) {
    if (p2 < slack) {
      return((p2-p1))
    } else if (p2 >= slack && p2 < capacity) {
      nj <- (slack-p1) + numjobsraw(slack, p2, capacity, slack)
      return(nj)
    } else {
      nj <- (slack-p1) + numjobsraw(slack, capacity, capacity, slack)
      return(nj)
    }

  } else if (p1 >= slack && p1 < capacity) {
    if (p2 < slack) {
      nj <- (p2-slack) + numjobsraw(p1, slack, capacity, slack)
      return(nj)
    } else if (p2 >= slack && p2 < capacity) {
      nj <- numjobsraw(p1, p2, capacity, slack)
      return(nj)
    } else {
      nj <- numjobsraw(p1, capacity, capacity, slack)
      return(nj)
    }

  } else {
    if (p2 < slack) {
      nj <- numjobsraw(capacity, slack, capacity, slack) + (p2 - slack)
      return(nj)
    } else if (p2 >= slack && p2 < capacity) {
      nj <- numjobsraw(capacity, p2, capacity, slack)
      return(nj)
    } else {
      return(0)
    }
  }
}

# what happens to the average unemployment rate if we move 500 people from one region to another?
deltaPop <- c(-500, 500)
deltaJobs <- c(numjobs(5000, 4500, 5000, 4000), numjobs(5000, 5500, 5000, 4000))

population <- capacity + deltaPop
jobs <- capjobs + deltaJobs
UE <- 1-(jobs / population)
avgUE <- sum((capacity / totalPop) * UE)

avgUE

# this shows what happens when people move from a productive configuration to a non-productive configuration. Overall unemployment increases. Given this new configuration,  500 people would be better off if they moved back to region A.
