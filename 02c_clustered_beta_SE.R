# ----------------------------------------------
# Author: Raul
# Date: 2024-12-20
# Note: Clustered \hat{\beta}_k standard errors
# ----------------------------------------------

rm(list = ls())
set.seed(123)
n <- 1e2


sim_clustered_data <- function(n, k, rho, b1) {
  x1 <- runif(n, 1, 10)
  b0 <- 1
  # Variance of error increases with x1
  sigma <- 2*x1 # Standard deviation increases with x1
  u <- rnorm(n, mean = 0, sd = sigma) # Error term with heteroskedasticity
  y <- b0 + b1*x1 + u
  return(data.frame(y=y, x1=x1))
}
