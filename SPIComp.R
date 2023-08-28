# Example usage
# md <- c(1, 2, 3, 4, 5, 6, 7, 
SPIComp <- function(md, sc) {
  n <- length(md)
  A1 <- matrix(nrow = n - sc + 1, ncol = sc)
  
  for (i in 1:sc) {
    A1[, i] <- md[i:(n - sc + i)]
  }
  
  Y <- rowSums(A1)
  
  n <- length(Y)
  SI <- numeric(n)
  
  for (k in 1:12) {
    d <- Y[k:12:n]
    
    # Empirical method
    
    SI[k:12:n] <- empdis(d)
  }
  
  SI <- norminv(SI)
  
  return(SI)
}


        