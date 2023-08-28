# Example usage
# d <- matrix(c(1, 5, 2, 4, 3, 3, 4, 2, 5, 1), ncol = 2)
# emp_biv_result <- emp_biv(d)
# print(emp_biv_result)
emp_biv <- function(d) {
  n <- nrow(d)
  bp <- numeric(n)
  
  for (i in 1:n) {
    td <- matrix(0, n, 3)
    
    td[, 1] <- ifelse(d[, 1] <= d[i, 1], 1, 0)
    td[, 2] <- ifelse(d[, 2] <= d[i, 2], 1, 0)
    
    td[, 3] <- td[, 1] * td[, 2]
    bp[i] <- sum(td[, 3])
  }
  
  y <- (bp - 0.44) / (n + 0.12)
  
  return(y)
}

