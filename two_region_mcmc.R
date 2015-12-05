# Charley's 2-region implementation

pdatastep <- function(pdata) {

  location <- sample(c(1, 2), 1)

  p <- c(tail(pdata$p1, 1), tail(pdata$p2, 1))

  if (location == 1) {
    if (p[1] >= 10 && p[2] <= 5990) {
      if (u1(p) < u2(p) - 200) {
        p[1] <- p[1]-10
        p[2] <- p[2]+10
        pdata <- rbind(pdata, c(p, u1(p), u2(p)))
      } else {
        probmove <- (u2(p) - 200) / u1(p)
        if (runif (1) < probmove) {
          p[1] <- p[1]-10
          p[2] <- p[2]+10
          pdata <- rbind(pdata, c(p, u1(p), u2(p)))
        } else {
          pdata <- rbind(pdata, c(p, u1(p), u2(p)))
        }
      }
    } else {
      pdata <- rbind(pdata, c(p, u1(p), u2(p)))
    }
  }

  if (location == 2) {
    if (p[2] >= 10 && p[1] <= 5990) {
      if (u2(p) < u1(p) - 200) {
        p[2] <- p[2]-10
        p[1] <- p[1]+10
        pdata <- rbind(pdata, c(p, u1(p), u2(p)))
      } else {
        probmove <- (u1(p) - 200) / u2(p)
        if (runif (1) < probmove) {
          p[2] <- p[2]-10
          p[1] <- p[1]+10
          pdata <- rbind(pdata, c(p, u1(p), u2(p)))
        } else {
          pdata <- rbind(pdata, c(p, u1(p), u2(p)))
        }
      }
    } else {
      pdata <- rbind(pdata, c(p, u1(p), u2(p)))
    }
  }

  return(pdata)

}
