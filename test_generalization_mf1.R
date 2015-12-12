setwd(dirname(file.choose()))

# multi-region, cost, utility, proposal distribution generalization
source('modelf1.R')

library(ggplot2)
library(reshape2)
library(ggthemes)

pmat <- matrix(NA, 60000, 4)

p <- c(379, 547, 274, 135)
k <- c(2400, 1800, 1700, 1900)
c <- matrix(c(0,200,400,600,200,0,200,400,400,200,0,200,600,400,200,0),nrow=4)
c[1:4, 1] <- c[1:4, 1] * 2

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

# Burn in
#pdatamelt <- melt(pdata, id.vars=c("iter"), measure.vars=c("p1","p2","p3", "p4"),
#                   variable.name="region", value.name="population")

pdatamelt <- melt(pdata, id.vars = c("iter"), measure.vars = c("p1", "p2", "p3", "p4"),
                variable.name = "region", value.name = "population")

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

# gelman.diag(mcmc.list(apply(pmatlist[,1,],2,mcmc)))

# pmatlist <- pmatlist[50001:60000,,]

mcmclist <- mcmc.list(mcmc(pmatlist[,1,1]), mcmc(pmatlist[,1,2]), 
                      mcmc(pmatlist[,1,3]), mcmc(pmatlist[,1,4]), 
                      mcmc(pmatlist[,1,5]), mcmc(pmatlist[,1,6]), 
                      mcmc(pmatlist[,1,7]), mcmc(pmatlist[,1,8]), 
                      mcmc(pmatlist[,1,9]))

# mcmclist2 <- as.mcmc.list(mcmc(pmatlist[,1,]))

# mcmclist <- as.mcmc.list(apply(pmatlist[,1,], 2, mcmc))

gelman.diag(mcmclist)

gelman.plot(mcmclist)

# Using Gelman Function from Statistical Computing

Gelman.Rubin <- function(psi){
  # psi[i,j] is the statistic psi(X[i,1:j])
  # for chain in i-th row of X
  
  # psi <- as.matrix(psi)
  psi <- t(psi)
  n <- ncol(psi)
  k <- nrow(psi)
  
  psi.means <- rowMeans(psi)
  B <- n * var(psi.means)
  psi.w <- apply(psi, 1, "var")
  W <- mean(psi.w)
  v.hat <- W*(n-1)/n + (B/n)
  r.hat <- v.hat / W
  return(r.hat)
  
}

Gelman.Rubin(t(pmatlist[30001:60000,1,]))^2

# Creating some facet graphs

costcomp <- array(dim=c(60000, 4, 4))
c <- matrix(c(0,200,400,600,200,0,200,400,400,200,0,200,600,400,200,0),nrow=4)

costiter <- array(dim=c(4,4,4))
costiter[,,1] <- c*5
costiter[,,2] <- c*7
costiter[,,3] <- c*9
costiter[,,4] <- c*11

for(cost in 1:dim(costiter)[3]){
  
  p <- c(379, 547, 274, 135)
  c <- costiter[,,cost]
  
  pmat <- matrix(NA, 60000, 4)
  
  pmat[1,] <- p
  
  for(i in 2:60000){
    p <- step.MCMC(p, k, c)
    pmat[i,] <- p
  }
  
  costcomp[,,cost] <- pmat
  
}

costcompmelt <- melt(costcomp)

colnames(costcompmelt) <- c("iter", "region", "costtype", "population")

costcompmelt$costtype <- factor(costcompmelt$costtype)

levels(costcompmelt$costtype) <- c("Cost*5", "Cost*7", "Cost*9", "Cost*11")

costcompmelt$region <- factor(costcompmelt$region)

levels(costcompmelt$region) <- c("p1", "p2", "p3", "p4")

ggplot(costcompmelt, aes(x = iter, y = population, color=region)) + geom_line() +
  facet_wrap( ~ costtype)
