# Example usage
# xd <- c(1, 2, 3, 4, 5)
# yd <- c(5, 4, 3, 2, 1)
# sc <- 3
# Dat2EmpMSDI_result <- Dat2EmpMSDI(xd, yd, sc)
# print(Dat2EmpMSDI_result)

Dat2EmpMSDI <- function(xd, yd, sc) {
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
  p <- numeric(n)
  
  for (k in 1:12) {
    d1 <- X[k:12:n]
    d2 <- Y[k:12:n]
    p[k:12:n] <- emp_biv(cbind(d1, d2))
  }
  
  y <- norminv(p)
  
  return(y)
}


