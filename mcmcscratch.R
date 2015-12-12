library(dplyr)

ugen <- function(p,loc,cap){
  return(-p[loc]+cap)
}


flowcalc <- function(p,cap,c){
  flowtable <- data.frame(t(combn(1:length(p),2)))
  flowtable$flow <- cap[flowtable$X2]-cap[flowtable$X1]+p[flowtable$X1]-
    c[flowtable$X1,flowtable$X2]-p[flowtable$X2]
  return(flowtable)
  
}

flowcalcpan <- function(p,cap){
  u <- cap-p
  du <- data.frame(index=1:length(u), u=u)
  du <- arrange(du,u)
  
  flowtable <- data.frame(t(combn(du$index,2)))
  colnames(flowtable) <- c("src", "dest")
  flowtable$flow <- (cap[flowtable$dest]-cap[flowtable$src]+p[flowtable$src]-
                       p[flowtable$dest])/4
  
  fsame <- data.frame()
  
  for(i in 1:length(p)){
    fsame <- rbind(fsame, c(i, p[i] - filter(flowtable, src == i)$flow))
  }
  colnames(fsame) <- c("loc","stay")
  
  flowtable <- rbind(flowtable,data.frame(src=fsame$loc, dest=fsame$loc, flow=fsame$stay))
  
  fmat <- matrix(nrow=length(p), ncol=length(p))
  
  for(row in 1:length(flowtable$src)){
    fmat[flowtable[row,"src"],flowtable[row,"dest"]] <- flowtable[row,"flow"]
  }
  
  fmat[is.na(fmat)] <- 0
  
  pmat <- matrix(nrow=length(p), ncol=length(p))
  
  for(col in 1:length(p)){
    pmat[,col] <- p[col]
  }
  
  markovmat <- fmat/pmat
  
  return(list(flow = fmat, transition = markovmat))
}

flowcalcpan(c(4,2),c(8,14))

p <- c(4,2,2,2)
k <- c(8,14,14,14)
flowcalcpan(p,k)

p %*% flowcalcpan(p,k)$transition



