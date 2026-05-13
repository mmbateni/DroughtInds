# empkend.R
# Computes the empirical Kendall distribution function K_C evaluated at
# specified quantile values tq, given a bivariate data matrix x.
#
# The Kendall distribution K_C is the CDF of the random variable
# T = F_XY(X, Y), i.e., K_C(t) = P(T <= t). In a multivariate setting
# T is NOT Uniform on [0,1]; it follows K_C(t) >= t for all t in (0,1)
# (Genest & Rivest, 2001; Nelsen et al., 2003). This function estimates
# K_C non-parametrically from data, following the algorithm:
#
#   Step 1. For each observation j, compute
#             w_j = #{i : x[i,1] < x_sorted[j,1]  AND  x[i,2] < x_sorted[j,2]} / (n+1)
#           where x_sorted has columns sorted independently. w_j is the
#           empirical joint CDF evaluated at observation j using strict
#           inequalities and the (n+1) denominator.
#
#   Step 2. For each query quantile tq[k]:
#             K_n(tq[k]) = #{j : w_j < tq[k]} / n
#
# This is the non-parametric Kendall CDF estimator used in MSDI-Kendall
# (De Michele et al., 2026, Eq. 10-11): U = K_C(T) ~ Uniform on [0,1],
# so Phi^{-1}(K_C(T)) ~ N(0,1) as required for proper standardization.
#
# Inputs:
#   x  : numeric matrix with n rows and 2 columns (bivariate observations)
#   tq : numeric vector of quantile values in [0,1] at which to evaluate K_C
#          In the MSDI context, tq = T_vals = emp_biv(cbind(X, Y)), i.e.,
#          the empirical joint CDF values of the accumulated data.
#
# Output:
#   Kn : numeric vector of length length(tq) with K_C(tq[k]) for each k
#
# References:
#   Genest C., Rivest L.-P., 2001, On the multivariate probability integral
#   transformation, Statistics and Probability Letters, 53(4), 391-399.
#   doi: 10.1016/S0167-7152(01)00047-5
#
#   Nelsen R.B., Quesada-Molina J.J., Rodriguez-Lallena J.A.,
#   Ubeda-Flores M., 2003, Kendall distribution functions,
#   Statistics and Probability Letters, 65(3), 263-268.
#   doi: 10.1016/j.spl.2003.08.002
#
#   De Michele C., Salvadori G., Durante F., AghaKouchak A., 2026, On the
#   Construction of Multivariate Drought Indices: Theoretical Foundations and
#   Practical Implications, Water Resources Research, 62, e2025WR041665,
#   doi: 10.1029/2025WR041665
#
# Example usage:
#   set.seed(1)
#   x  <- matrix(c(sort(runif(20)), sort(runif(20))), ncol = 2)
#   tq <- seq(0.1, 0.9, by = 0.1)
#   Kn <- empkend(x, tq)
#   print(Kn)

empkend <- function(x, tq) {

  n    <- nrow(x)
  n_tq <- length(tq)

  # w[j]: empirical joint CDF at sorted observation j (strict inequalities,
  # (n+1) denominator -- consistent with emp_biv's California position)
  w   <- numeric(n)
  wu  <- matrix(NA, nrow = n, ncol = 2)
  wu[, 1] <- sort(x[, 1])
  wu[, 2] <- sort(x[, 2])

  hi_ind <- logical(n)

  for (j in seq_len(n)) {
    for (i in seq_len(n)) {
      hi_ind[i] <- (x[i, 1] < wu[j, 1]) & (x[i, 2] < wu[j, 2])
    }
    w[j] <- sum(hi_ind) / (n + 1)
  }

  # K_n(tq[k]) = proportion of w values strictly below tq[k]
  Kn <- numeric(n_tq)
  for (k in seq_len(n_tq)) {
    Kn[k] <- sum(w < tq[k]) / n
  }

  return(Kn)
}
