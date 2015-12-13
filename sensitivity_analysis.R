library(coda)
library(dplyr)
library(ggplot2)
library(reshape2)
library(ggthemes)

# multi-region, cost, utility, proposal distribution generalization
source('modelf1.R')

# experiment runner helper function
run.mcmc <- function(p, k, c, iter = 60000, burn.in = 0) {
  pmat <- matrix(NA, 60000, 4)

  pmat[1,] <- p

  for(i in 2:60000){
    p <- step.MCMC(p, k, c)
    pmat[i,] <- p
  }

  # organize results into data.frame
  pdata <- data.frame(pmat)
  colnames(pdata) <- c("p1", "p2", "p3", "p4")
  pdata$iter <- 1:length(pdata$p1)

  pdatamelt <- melt(pdata[(burn.in + 1):60000,], id.vars=c("iter"), measure.vars=c("p1","p2","p3", "p4"),
                    variable.name="region", value.name="population")

  return(pdatamelt)
}

# mean population
pop.mean <- function(df) {
  df %>% group_by(region) %>% summarize(pop.mean=mean(population))
}

# plotting helper functions
plot.by.iter <- function(df, title) {
  # plot population over iteration
  min.iter <- min(df$iter)
  total <- sum(df[df$iter == min.iter, 'population'])
  .e <- environment()

  ggplot(df, aes(x=iter, y=population, color=region), environment=.e) +
    geom_line() + ylim(0, total) + theme_bw() +
    ggtitle(title)
}

plot.distribution <- function(df) {
  # plot distributions by region
  min.iter <- min(df$iter)
  total <- sum(df[df$iter == min.iter, 'population'])
  .e <- environment()

  ggplot(df, aes(x=population, fill=region), environment=.e) + geom_histogram(binwidth = 10) +
    facet_grid(region ~ .) + xlim(0, total) + theme_bw()
}
