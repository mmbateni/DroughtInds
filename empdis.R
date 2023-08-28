# Example usage
# d <- c(1, 2, 3, 4, 5)
# empdis_result <- empdis(d)
# print(empdis_result)

empdis <- function(d) {
  n <- length(d)
  bp <- numeric(n)
  
  for (i in 1:n) {
    bp[i] <- sum(d <= d[i])
  }
  
  y <- (bp - 0.44) / (n + 0.12)
  
  return(y)
}


