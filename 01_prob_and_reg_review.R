# ----------------------------------------------
# Author: Raul
# Date: 2024-12-20
# Note: This script showcases some results from
#       Causal Inference the Mixtape
# ----------------------------------------------
rm(list = ls())
set.seed(1234)


# ---------- #
# Summation  #
# ---------- #


# sum_{i=1}^n{x_i/y_i} != sum_{i=1}^n{x_i}/sum_{i=1}^n{y_i}
x <- runif(5)
y <- runif(5)
sum(x/y)
sum(x)/sum(y)


# sum_{i=0}^n{(x_i-mean(x))} = 0 but sum_{i=0}^n{(x_i-mean(x))^2} is usually not
(x-mean(x)) |> sum()
((x-mean(x))^2) |> sum()


# ---------- #
# Regression #
# ---------- #

# Regression forces E(u|x) = E(u)
x1 = ifelse(runif(n) <= 0.5, 1, 0)
b0 = 1
b1 = 2.5
u = rnorm(n)
y = b0 + b1*x1 + u
fit <- lm(y~x1)
e <- (y - predict(fit))
sum(e)
sum(e[x1==0])
sum(e[x1==1])

# If E(u)=0 and E(u|x)=0, then E(xu)=0 which means Cov(xu)=0
# proof: E(xu) = E[E(xu|x)] = E[xE(u|x)] = E[x*0] = E(0) = 0
cov(x1, e)
mean(x1*e)

# It still holds when the model is misspecified
# E(u|x) = E(u) = 0 holds by design of regression 
x2 <- rnorm(n) + 3*x1
y = b0 + b1*x1 + 5*x2 + u
fit <- lm(y~x1)
e <- (y - predict(fit))
sum(e)
sum(e[x1==0])
sum(e[x1==1])
cov(x1, e)
mean(x1*e)

# Take away: sum(e)=0, mean(x1*e)=0, and sum(e|x)=0 is by construction of 
# the \hat{beta}_k coefficients, and as such, regardless of model specification.


