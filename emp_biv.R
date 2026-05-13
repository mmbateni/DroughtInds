# emp_biv.R
# Computes the empirical bivariate copula (joint CDF on the unit square),
# i.e., the quantity T = C_n(F_X(x), F_Y(y)) = F_XY(x, y) estimated
# non-parametrically from data.
#
# For each observation i, counts how many paired observations satisfy
# w(:,1) <= w(i,1) AND w(:,2) <= w(i,2), then divides by (n+1).
# This corresponds to the California / Weibull-type plotting position used
# in AghaKouchak's non-parametric framework.
#
# NOTE: The resulting values T_i are NOT Uniformly distributed on [0,1]
# in the multivariate case. Their distribution is the Kendall distribution
# K_C(t) >= t for all t in (0,1) (Genest & Rivest, 2001). Therefore,
# applying Phi^{-1} directly to T_i (as in MSDI-2013) does NOT yield a
# Gaussian index. Use empkend() to apply the Kendall correction before
# the Normal transform (De Michele et al., 2026, Eq. 10-11).
#
# Input:
#   w  : numeric matrix with n rows and 2 columns
#          col 1 = variable 1 values (e.g., accumulated precipitation)
#          col 2 = variable 2 values (e.g., accumulated soil moisture)
#
# Output:
#   cop_empri : numeric vector of length n with empirical copula values
#               in (0, 1), i.e., T_i = F_XY(w[i,1], w[i,2])
#
# References:
#   AghaKouchak A., Farahmand A., Melton F., Teixeira J., Anderson M.,
#   Wardlow B., Hain C., 2015, Remote sensing of drought: Progress,
#   challenges and opportunities, Reviews of Geophysics, 53(2), 452-480.
#
#   Hao Z., AghaKouchak A., 2013, Multivariate Standardized Drought Index:
#   A Parametric Multi-Index Model, Advances in Water Resources, 57, 12-18,
#   doi: 10.1016/j.advwatres.2013.03.009
#
#   De Michele C., Salvadori G., Durante F., AghaKouchak A., 2026, On the
#   Construction of Multivariate Drought Indices: Theoretical Foundations and
#   Practical Implications, Water Resources Research, 62, e2025WR041665,
#   doi: 10.1029/2025WR041665
#
# Example usage:
#   w <- matrix(c(1, 5, 2, 4, 3, 3, 4, 2, 5, 1), ncol = 2)
#   cop_empri <- emp_biv(w)
#   print(cop_empri)

emp_biv <- function(w) {
  nn <- nrow(w)
  bp <- numeric(nn)

  for (i in seq_len(nn)) {
    td        <- matrix(0, nrow = nn, ncol = 3)
    td[w[, 1] <= w[i, 1], 1] <- 1
    td[w[, 2] <= w[i, 2], 2] <- 1
    td[, 3]   <- td[, 1] * td[, 2]
    bp[i]     <- sum(td[, 3])
  }

  # California / Weibull plotting position (matches AghaKouchak MATLAB original)
  cop_empri <- bp / (nn + 1)

  return(cop_empri)
}
