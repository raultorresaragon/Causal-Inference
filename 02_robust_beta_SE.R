# ----------------------------------------------
# Author: Raul
# Date: 2024-12-20
# Note: Robust \hat{\beta}_k standard errors
# ----------------------------------------------
rm(list = ls())
set.seed(123)
n <- 1e2


sim_hetero_data <- function(n, b1) {
    x1 <- runif(n, 1, 10)
    b0 <- 1
    # Variance of error increases with x1
    sigma <- 2*x1 # Standard deviation increases with x1
    u <- rnorm(n, mean = 0, sd = sigma) # Error term with heteroskedasticity
    y <- b0 + b1*x1 + u
    return(data.frame(y=y, x1=x1))
}

get_beta_coverage <- function(data, b1) {

  y <- data$y
  x1 <- data$x1
  fit <- lm(y~x1)
  res <- y-predict(fit)
  
  X <- as.matrix(cbind(1,x1))
  XX_inv <- solve(t(X) %*% X)
  W <- diag(res^2)
  robust_varcovar <- XX_inv %*% t(X) %*% W %*% X %*% XX_inv 

  b1_hat <- coef(summary(fit))[2,1]
  b1_hat_se <- coef(summary(fit))[2,2]
  b1_hat_se_rob <- sqrt(diag(robust_varcovar))[2]

  coverage_se <- (b1_hat - 1.96*b1_hat_se) <= b1 & b1 <= (b1_hat + 1.96*b1_hat_se)
  coverage_se_rob <- (b1_hat - 1.96*b1_hat_se_rob) <= b1 & b1 <= (b1_hat + 1.96*b1_hat_se_rob)

  return <- list(b1_hat = b1_hat,
                 b1_hat_se = b1_hat_se,
                 b1_hat_se_rob = b1_hat_se_rob,
                 coverage_se = coverage_se,
                 coverage_se_rob = coverage_se_rob)
}

run_sims <- function(N, n, b1) {
  beta_hat <- beta_hat_se <- beta_hat_se_rob <- 
  coverage_se <- coverage_se_rob <- vector(length = N)
  for(i in 1:N){ 
    data <- sim_hetero_data(n, b1)
    sim <- get_beta_coverage(data, b1)
    beta_hat[i] <- sim$b1_hat
    beta_hat_se[i] <- sim$b1_hat_se
    beta_hat_se_rob[i] <- sim$b1_hat_se_rob
    coverage_se[i] <- sim$coverage_se
    coverage_se_rob[i] <- sim$coverage_se_rob
  }
  res <- data.frame(beta1 = b1,
                    beta_hat = mean(beta_hat),
                    beta_hat_se = mean(beta_hat_se),
                    beta_hat_se_rob = mean(beta_hat_se_rob),
                    beta_hat_se_boot = sd(beta_hat),
                    coverage_se = mean(coverage_se),
                    coverage_se_rob = mean(coverage_se_rob)
                    )
  res
}

run_sims(1e2, 1e3, 4)
