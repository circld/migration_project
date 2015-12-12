library(coda)
library(ggplot2)
library(reshape2)
library(ggthemes)

setwd(dirname(file.choose()))

# multi-region, cost, utility, proposal distribution generalization
source('modelf1.R')

pmat <- matrix(NA, 60000, 4)

p <- c(379, 547, 274, 135)
k <- c(2400, 1800, 1700, 1900)
c <- matrix(c(0,200,400,600,200,0,200,400,400,200,0,200,600,400,200,0),nrow=4)

# For testing out the zero cost case.
# c <- matrix(0, 4, 4)

pmat[1,] <- p

for(i in 2:60000){
  p <- step.MCMC(p, k, c)
  pmat[i,] <- p
}

pdata <- data.frame(pmat)
colnames(pdata) <- c("p1", "p2", "p3", "p4")
pdata$iter <- 1:length(pdata$p1)

pdatamelt <- melt(pdata[50001:60000,], id.vars=c("iter"), measure.vars=c("p1","p2","p3", "p4"),
                  variable.name="region", value.name="population")

ggplot(pdatamelt, aes(x=iter, y=population, color=region)) + geom_line()

ggplot(pdatamelt, aes(x=population, fill=region)) +
  geom_histogram(binwidth = 1) + theme_tufte()

# Creating a pmat list

pmatlist <- array(dim=c(60000,4,9))

for(j in 1:9){
  pmat <- matrix(NA, 60000, 4)

  p <- c(379, 547, 274, 135)
  k <- c(2400, 1800, 1700, 1900)
  c <- matrix(c(0,200,400,600,200,0,200,400,400,200,0,200,600,400,200,0),nrow=4)

  # For testing out the zero cost case.
  # c <- matrix(0, 4, 4)

  pmat[1,] <- p

  for(i in 2:60000){
    p <- step.MCMC(p, k, c)
    pmat[i,] <- p
  }

  pmatlist[,,j] <- pmat
}

#gelman.diag(mcmc.list(apply(pmatlist[,1,],2,mcmc)))

mcmclist <- mcmc.list(mcmc(pmatlist[,1,1]), mcmc(pmatlist[,1,2]),
                      mcmc(pmatlist[,1,3]), mcmc(pmatlist[,1,4]),
                      mcmc(pmatlist[,1,5]), mcmc(pmatlist[,1,6]),
                      mcmc(pmatlist[,1,7]), mcmc(pmatlist[,1,8]),
                      mcmc(pmatlist[,1,9]))



gelman.diag(mcmclist)
