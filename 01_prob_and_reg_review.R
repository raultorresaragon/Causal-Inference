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


