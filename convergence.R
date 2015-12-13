library(coda)

source('modelf1.R')
source('sensitivity_analysis.R')

# convenience function for generating gelman shrinkage factors & plots
test.gr.convergence <- function(
  p, k, c, iter = 60000, num.chains = 9, num.regions = 4, plot = FALSE
) {

  pmatlist <- array(dim=c(iter, num.regions, num.chains))

  for(j in 1:num.chains) {
    pmat <- matrix(NA, iter, num.regions)
    pmat[1,] <- p

    for(i in 2:iter) {
      p <- step.MCMC(p, k, c)
      pmat[i,] <- p
    }

    pmatlist[,,j] <- pmat
  }

  # use gelman convergence utilities in coda packages
  mcmclist <- lapply(1:num.regions, function(reg) {
    mcmc.list(lapply(1:num.chains, function(i) mcmc(pmatlist[, reg, i])))
  })

  if (plot) {
    par(mfrow = c(2, 2))
    for (r in 1:num.regions) {
      gelman.plot(mcmclist[[r]], main = paste('Region', r), auto.layout = FALSE)
    }
    par(mfrow = c(1, 1))
  }

  results <- matrix(0, nrow = num.regions, ncol = 2)

  for (r in 1:num.regions) {
    psrf <- gelman.diag(mcmclist[[r]])
    results[r, ] <- psrf$psrf[1, ]  # extract only estimate & CI
  }

  colnames(results) <- c('Point est.', 'Upper C.I.')
  rownames(results) <- c('Region 1', 'Region 2', 'Region 3', 'Region 4')
  return(results)
}
