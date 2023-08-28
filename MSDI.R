#  This code computes the the Standardized Precipitation Index (SPI), 
#  Standardized Soil Moisture Index (SSI), and Multivariate Standardized Drought Index (MSDI)
#  empirically (MSDIe) and parametrically (MSDIp). 
# 
#  "td.txt" includes sample monthly precipitation and soil moisture data (>=30 years,vector).
# 
#  Reference Publications:
#  MSDIp:  Hao Z., AghaKouchak A., 2013, Multivariate Standardized Drought Index: A Parametric Multi-Index Model, Advances in Water Resources, 57, 12-18, doi: 10.1016/j.advwatres.2013.03.009.
#  MSDIe   Hao Z., AghaKouchak A., 2014, A Nonparametric Multivariate Multi-Index Drought Monitoring Framework, Journal of Hydrometeorology, 15, 89-101, doi:10.1175/JHM-D-12-0160.1. 
#  Hao Z., AghaKouchak A., Nakhjiri N., Farahmand A., 2014, Global Integrated Drought Monitoring and Prediction System, Scientific Data, 1:140001, 1-10, doi: 10.1038/sdata.2014.1.
# 
# % Developed by: M. M. Bateni
# 
# 
# %% Compute SPI, SSI, MSDIe (nonparametric MSDI) and MSDIp (parametric MSDI) with monthly Precipitation and and Soil moisture data (ds.txt).
# Load the 'copula' package if not already loaded
if (!requireNamespace("copula", quietly = TRUE)) {
  install.packages("copula")
}
library(copula)  # Load the required library

# Load Precipitation and Soil Moisture Data
d <- read.table('td.txt')

# Monthly precipitation vector (mm/day, mm/month, in/day, in/month)
dp <- d$V1

# Monthly soil moisture vector
ds <- d$V2

nm <- length(dp)

# Specify time scale (e.g., 6-month SPI or MSDI)
sc <- 6

# Initialize matrices of drought indices
SPI <- rep(0, nm)
SSI <- rep(0, nm)
MSDIe <- rep(0, nm)
MSDIp <- rep(0, nm)

# Compute empirical SPI and SSI
SPI[(sc+1):nm] <- SPIComp(dp, sc)
SSI[(sc+1):nm] <- SPIComp(ds, sc)

# Compute empirical MSDI
MSDIe[(sc+1):nm] <- Dat2EmpMSDI(dp, ds, sc)

# Compute parametric (copula-based) MSDI
MSDIp[(sc+1):nm] <- Dat2CopMSDI(dp, ds, sc, 'Frank')

# Plot the four indices
par(mfrow=c(4,1))
plot(1:nm, SPI, col="green", type="l", ylab="Index", xlab="Year", main="SPI")
plot(1:nm, SSI, col="blue", type="l", ylab="Index", xlab="Year", main="SSI")
plot(1:nm, MSDIe, col="red", type="l", ylab="Index", xlab="Year", main="MSDIe")
plot(1:nm, MSDIp, col="black", type="l", ylab="Index", xlab="Year", main="MSDIp")

xk <- seq(1980, 2012, by=2)
n <- length(xk)
xt <- (xk - 1980) * 12 + 1
xl <- as.character(xk)

for (i in 1:n) {
  xt[i] <- (xk[i] - 1980) * 12 + 1
  xl[i] <- as.character(xk[i])
}

par(mfrow=c(1,1))
plot(1:nm, SPI, col="green", type="l", ylab="Index", xlab="Year", main="All Indices")
lines(1:nm, SSI, col="blue")
lines(1:nm, MSDIe, col="red")
lines(1:nm, MSDIp, col="black")
legend("topright", legend=c("SPI", "SSI", "MSDIe", "MSDIp"), col=c("green", "blue", "red", "black"), lty=1)

plot(1:nm, SPI, col="green", type="l", ylab="Index", xlab="Year", main="Threshold")
lines(1:nm, -0.8 * rep(1, nm), col="red")
legend("topright", legend="Threshold -0.8", col="red", lty=1)
