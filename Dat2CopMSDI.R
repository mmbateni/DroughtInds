# Example usage
# xd <- c(1, 2, 3, 4, 5)
# yd <- c(5, 4, 3, 2, 1)
# sc <- 3
# st <- "Frank"  # Change this to the appropriate copula family
# Dat2CopMSDI_result <- Dat2CopMSDI(xd, yd, sc, st)
# print(Dat2CopMSDI_result)
Dat2CopMSDI <- function(xd, yd, sc, st) {
  library(copula)
  n <- length(xd)
  A1 <- matrix(nrow = n - sc + 1, ncol = sc)
  B1 <- matrix(nrow = n - sc + 1, ncol = sc)
  
  for (i in 1:sc) {
    A1[, i] <- xd[i:(n - sc + i)]
    B1[, i] <- yd[i:(n - sc + i)]
  }
  
  X <- rowSums(A1)
  Y <- rowSums(B1)
  
  n <- length(X)
  SI <- matrix(0, n, 3)
  cp <- matrix(0, n, 3)
  
  for (k in 1:12) {
    copname <- st
    
    d1 <- X[k:12:n]
    d2 <- Y[k:12:n]
    
    cp[k:12:n, 1] <- empdis(d1)
    cp[k:12:n, 2] <- empdis(d2)
    
    subset_data <- cp[k:12:n, c(1, 2)]
    
    fitted_cop <- fitCopula(subset_data, family = copname)
    
    theta <- coef(fitted_cop)
    
    cp[k:12:n, 3] <- pCopula(subset_data, family = copname, parameters = theta)

  }
  
  SI[, 1] <- qnorm(cp[, 1])
  SI[, 2] <- qnorm(cp[, 2])
  SI[, 3] <- qnorm(cp[, 3])
  
  y <- SI[, 3]
  
  return(y)
}

