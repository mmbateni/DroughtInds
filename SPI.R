library(fitdistrplus)  # Load the required library

SPI <- function(Data, scale, nseas) {
  erase_yr <- ceiling(scale / 12)
  
  n <- length(Data)
  Z <- numeric(n)
  
  for (is in 1:nseas) {
    tind <- seq(is, n, by = nseas)
    Xn <- rowSums(matrix(Data[tind:(tind[1] + scale - 1)], ncol = scale))
    
    zeroa <- which(Xn == 0)
    Xn_nozero <- Xn[-zeroa]
    q <- length(zeroa) / length(Xn)
    
    if (length(Xn_nozero) > 0) {
      parm <- fitdist(Xn_nozero, "gamma")$estimate
      Gam_xs <- q + (1 - q) * pgamma(Xn, parm[1], parm[2])
      Z[tind] <- qnorm(Gam_xs)
    }
  }
  
  return(Z)
}

# Example usage
set.seed(123)  # for reproducibility
Data <- rgamma(1000, shape = 1, rate = 1)
Z <- SPI(Data, scale = 3, nseas = 12)
