# DroughtInds — Drought Indices in R

R implementation of univariate and multivariate standardized drought indices,
updated to include the **Kendall-distribution-based standardization** described
in De Michele et al. (2026).

---

## Background

Univariate drought indices (SPI, SSI, …) are constructed by applying the
**Probability Integral Transform (PIT)**: for a variable *X* with CDF *F_X*,
the transformed value *U = F_X(X)* is Uniform on [0, 1], so
*S = Φ⁻¹(F_X(X))* is standard Normal (McKee et al., 1993).

In a **multivariate setting**, the joint CDF *T = F_XY(X,Y)* is **not**
Uniform; its distribution is the **Kendall distribution** *K_C(t) ≥ t* for
all *t ∈ [0,1]*, where *C* is the copula of *(X,Y)* (Genest & Rivest, 2001).
Applying Φ⁻¹ directly to *T* — as in the original MSDI formulation
(Hao & AghaKouchak, 2013) — therefore yields an index that is **not**
Gaussian and **overestimates drought frequency** (e.g., ~34% of months below
−1 instead of the expected ~15.9%).

The correct multivariate standardized index is (De Michele et al., 2026, Eq. 11):

> **S_x = Φ⁻¹( K_C( F_XY(x, y) ) )**

where *K_C* is the Kendall distribution of the copula *C*. This ensures
*S_x ~ N(0, 1)* exactly.

---

## Files

| File | Description |
|------|-------------|
| `MSDI.R` | **Main script.** Loads data, computes SPI, SSI, and both MSDI versions (MSDI-2013 and MSDI-Kendall), prints drought-frequency summary, produces comparison plots. |
| `Dat2EmpMSDI.R` | Nonparametric MSDI. Returns both `msdi2013` and `msdiKendall` (empirical Kendall CDF correction). |
| `Dat2CopMSDI.R` | Parametric copula-based MSDI. Returns both `msdi2013` and `msdiKendall` (analytical/numerical Kendall CDF from the fitted copula). |
| `SPIComp.R` | Nonparametric (empirical CDF) univariate standardized index (SPI or SSI). |
| `SPI.R` | Parametric (Gamma-fitted) SPI. |
| `empkend.R` | **Empirical Kendall distribution** `K_C(t)`. Takes bivariate data `x` and query quantiles `tq`; returns `K_n(tq[k]) = #{j: w_j < tq[k]} / n`. Called by `Dat2EmpMSDI` for the MSDI-Kendall correction. |
| `emp_biv.R` | Empirical bivariate copula via California/Weibull plotting position `bp / (n+1)` (AghaKouchak non-parametric framework). Returns `T = F_XY(x,y)`, the MSDI-2013 argument. |
| `empdis.R` | Empirical univariate CDF via Cunnane plotting position `(rank - 0.44) / (n + 0.12)`. Used for marginals in SPI/SSI and the parametric copula fit. |
| `td.txt` | Sample data: monthly precipitation (col 1) and soil moisture (col 2). |

---

## MSDI versions

| Name | Formula | Distribution | Drought freq. (expected ~15.9%) |
|------|---------|-------------|-------------------------------|
| **MSDI-2013** | Φ⁻¹(F_XY) | Skewed (NOT Normal) | Inflated (~30–40% in practice) |
| **MSDI-Kendall** | Φ⁻¹(K_C(F_XY)) | **N(0,1)** ✓ | ~13–16% ✓ |

---

## Usage

```r
# Source all helper functions
source("empdis.R")
source("emp_biv.R")
source("empkend.R")
source("SPIComp.R")
source("Dat2EmpMSDI.R")
source("Dat2CopMSDI.R")

# Run main script
source("MSDI.R")
```

Or call functions directly:

```r
source("empdis.R"); source("emp_biv.R"); source("empkend.R"); source("Dat2EmpMSDI.R")

d   <- read.table("td.txt")
res <- Dat2EmpMSDI(d$V1, d$V2, sc = 6)

# Original (biased) MSDI
msdi_2013    <- res$msdi2013

# Kendall-corrected (properly N(0,1)) MSDI
msdi_kendall <- res$msdiKendall

# Percentage below drought threshold -1
cat(sprintf("MSDI-2013 below -1:    %.1f%%\n", 100*mean(msdi_2013 < -1)))
cat(sprintf("MSDI-Kendall below -1: %.1f%%\n", 100*mean(msdi_kendall < -1)))
```

---

## Dependencies

```r
install.packages(c("copula", "fitdistrplus"))
```

---

## References

- **De Michele C., Salvadori G., Durante F., AghaKouchak A., 2026**, On the
  Construction of Multivariate Drought Indices: Theoretical Foundations and
  Practical Implications, *Water Resources Research*, 62, e2025WR041665.
  doi: [10.1029/2025WR041665](https://doi.org/10.1029/2025WR041665)

- **Hao Z., AghaKouchak A., 2013**, Multivariate Standardized Drought Index:
  A Parametric Multi-Index Model, *Advances in Water Resources*, 57, 12–18.
  doi: [10.1016/j.advwatres.2013.03.009](https://doi.org/10.1016/j.advwatres.2013.03.009)

- **Hao Z., AghaKouchak A., Nakhjiri N., Farahmand A., 2014**, Global Integrated
  Drought Monitoring and Prediction System, *Scientific Data*, 1:140001.
  doi: [10.1038/sdata.2014.1](https://doi.org/10.1038/sdata.2014.1)

- **McKee T.B., Doesken N.J., Kleist J., 1993**, The relationship of drought
  frequency and duration to time scales, *Proc. 8th Conf. Applied Climatology*,
  American Meteorological Society, 179–184.

- **Farahmand A., AghaKouchak A., 2015**, A generalized framework for deriving
  nonparametric standardized drought indicators, *Advances in Water Resources*,
  76, 140–145. doi: [10.1016/j.advwatres.2014.11.012](https://doi.org/10.1016/j.advwatres.2014.11.012)

- **Nelsen R.B., 2006**, *An Introduction to Copulas*, 2nd ed., Springer-Verlag.

- **Genest C., Rivest L.-P., 2001**, On the multivariate probability integral
  transformation, *Statistics and Probability Letters*, 53(4), 391–399.
