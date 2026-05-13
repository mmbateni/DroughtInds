# Dat2EmpMSDI.R
# Computes the nonparametric (empirical) Multivariate Standardized Drought Index
# in two versions:
#
#   MSDI-2013  (original, Hao & AghaKouchak 2013):
#     MSDI_x,y = Phi^{-1}( F_XY(x, y) )
#     where F_XY is estimated via the empirical bivariate copula (emp_biv).
#     NOTE: this index is NOT correctly Gaussian-standardized because
#     T = F_XY(X,Y) follows the Kendall distribution K_C(t) >= t, not
#     Uniform on [0,1]. Applying Phi^{-1} directly to T overestimates drought
#     frequency (De Michele et al., 2026, Eq. 12 and Figures 2-3).
#
#   MSDI-Kendall  (corrected, De Michele et al. 2026):
#     MSDI_x,y = Phi^{-1}( K_C( F_XY(x, y) ) )
#     where K_C is the empirical Kendall distribution estimated by empkend().
#     Since U = K_C(T) ~ Uniform on [0,1] by the univariate PIT, the index
#     Phi^{-1}(U) ~ N(0,1) as required (De Michele et al., 2026, Eq. 10-11).
#     Yields approximately 15.9% of values below -1 as expected.
#
# Procedure (applied separately for each calendar month to preserve seasonality):
#   1. Accumulate xd and yd over sc months.
#   2. Compute T = emp_biv(cbind(X, Y))  -- empirical joint CDF (MSDI-2013 argument).
#   3. Compute K_C(T) = empkend(cbind(X, Y), T)  -- Kendall CDF evaluated at T.
#   4. MSDI-2013    = qnorm(T)
#      MSDI-Kendall = qnorm(K_C(T))
#
# Inputs:
#   xd : numeric vector of monthly precipitation data
#   yd : numeric vector of monthly soil moisture data
#   sc : integer accumulation time scale in months (e.g., 6)
#
# Output: list with two numeric vectors of length (length(xd) - sc + 1):
#   $msdi2013   : MSDI-2013 (original, not properly Gaussian)
#   $msdiKendall: MSDI-Kendall (Kendall-corrected, N(0,1))
#
# Required source files:
#   source("emp_biv.R")   -- empirical bivariate copula
#   source("empkend.R")   -- empirical Kendall distribution function
#
# References:
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
#   source("emp_biv.R")
#   source("empkend.R")
#   xd <- rgamma(360, shape = 2, rate = 1)
#   yd <- runif(360, 0.28, 0.40)
#   result      <- Dat2EmpMSDI(xd, yd, sc = 6)
#   msdi2013    <- result$msdi2013
#   msdiKendall <- result$msdiKendall

Dat2EmpMSDI <- function(xd, yd, sc) {
  n <- length(xd)

  # --- Step 1: build accumulation matrix over sc months ---
  A1 <- matrix(nrow = n - sc + 1, ncol = sc)
  B1 <- matrix(nrow = n - sc + 1, ncol = sc)
  for (i in seq_len(sc)) {
    A1[, i] <- xd[i:(n - sc + i)]
    B1[, i] <- yd[i:(n - sc + i)]
  }
  X  <- rowSums(A1)
  Y  <- rowSums(B1)
  n2 <- length(X)

  # Output vectors
  T_vals  <- numeric(n2)   # empirical joint CDF  = MSDI-2013 argument
  KC_vals <- numeric(n2)   # Kendall CDF of T     = MSDI-Kendall argument

  # --- Steps 2-3: compute per calendar month to account for seasonality ---
  for (k in seq_len(12)) {
    idx   <- seq(k, n2, by = 12)
    d_mat <- cbind(X[idx], Y[idx])

    # Step 2: T = F_XY(X,Y) via empirical bivariate copula (California position)
    T_k <- emp_biv(d_mat)
    T_vals[idx] <- T_k

    # Step 3: K_C(T) via empirical Kendall distribution
    # empkend() takes the raw bivariate data x and the query quantiles tq,
    # and returns K_n(tq[k]) = #{j : w_j < tq[k]} / n  for each element of tq
    KC_k <- empkend(x = d_mat, tq = T_k)
    KC_vals[idx] <- KC_k
  }

  # --- Step 4: Gaussian transform ---
  # Clamp strictly inside (0,1) to avoid infinite Normal quantiles
  T_clamped  <- pmin(pmax(T_vals,  1e-6), 1 - 1e-6)
  KC_clamped <- pmin(pmax(KC_vals, 1e-6), 1 - 1e-6)

  msdi2013    <- qnorm(T_clamped)   # MSDI-2013:    Phi^{-1}(F_XY)
  msdiKendall <- qnorm(KC_clamped)  # MSDI-Kendall: Phi^{-1}(K_C(F_XY))

  return(list(msdi2013    = msdi2013,
              msdiKendall = msdiKendall))
}
