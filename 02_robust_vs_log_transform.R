# ----------------------------------------------
# Author: Raul
# Date: 2024-12-23
# Note: Transform Y or use sandwich SEs???
# ----------------------------------------------
library(sandwich)
library(lmtest)
rm(list = ls())
set.seed(123)
n <- 5e1

# Let's simulate Y such that it is continuous but non-negative
# and with homoscedasticity

sim_data <- function(n, b1) {
  x1 <- runif(n, 0, 1)
  b0 <- 1
  sigma <- 1
  u <- rnorm(n, mean = 0, sd = sigma) # <-no heteroscedasticity
  y <- exp(b0 + b1*x1 + u)
  return(data.frame(y=y, x1=x1))
}

data <- sim_data(n, 2) 


# In the real world,we don't know whether our data is baked 
# with heteroscedasticity. So we plot residuals to see if we
# have reason to believe there's heteroscedasticity.

plot_hetero <- function(data, fit=NULL) {
  
  if(is.null(fit)) { fit <- lm(y~x1,data) }
  yhat <- predict(fit)
  res <- data$y - yhat
  plot(y=res, x=yhat, main = "residual plot")
  
}

plot_hetero(data)

# Hence, even though we know there is no heteroscedasticity, the residual plot
# shows unequal variance because of the log(y) = x^TB relationship. 

# We use sandwich/robust standard errors because of the heteroscedasticity
# ------------------------------------------------------------------------
model1 <- lm(y ~ x1, data)
model1_sum <- coeftest(model1, vcov = vcovHC(model1, type = "HC1"))
model1_sum

model2 <- lm(log(y) ~ x1, data)
model2_sum <- summary(model2)
coef(model2_sum)



library(ggplot2)
library(dplyr)

plot_with_ci <- function(data, model, robust = FALSE) {
  
    x_grid <- seq(min(data$x1), max(data$x1), length.out = nrow(data))
    plot_data <- data
  
    if(robust == TRUE) {
      vcov <- vcovHC(model, type = "HC1")
   
      # Create a grid for predictions (when fitting robust linear model)
      y <- data$y
      X <- cbind(1, x_grid)
      fit <- X %*% coef(model)
      fit_se <- sqrt(rowSums((X %*% vcov) * X)) 
      alpha <- 0.05
      t_crit <- qt(1-alpha/2, df = model$df.residual)
      lower <- fit - t_crit * fit_se
      upper <- fit + t_crit * fit_se
      pred_data <- data.frame(x = x_grid, fit = fit, lower = lower, upper = upper)
      title <- "Linear Regression with Robust Standard Errors"
    
    } else {
      
      vcov <-vcov(model)
      y <- exp(data$y)
  
      # Create grid for predictions (when fitting log(Y) model)
      new_data <- data.frame(x1 = x_grid)
      pred_log <- predict(model, newdata = new_data, interval = "confidence")
    
      # Transform predictions back to the original scale
      pred_data <- data.frame(
        x = x_grid,
        fit = exp(pred_log[, "fit"]),              # Predicted values on y-scale
        lower = exp(pred_log[, "lwr"]),            # Lower bound on y-scale
        upper = exp(pred_log[, "upr"])             # Upper bound on y-scale
      )
      
      title <- "Log(Y) Model with Confidence Intervals"
   }


    ggplot(plot_data, aes(x = x1, y = y)) +
      geom_point(color = "black") +  # Scatter plot of data
      geom_line(data = pred_data, aes(x = x, y = fit), color = "darkred", linewidth = 1) +
      geom_ribbon(data = pred_data, 
                  aes(x = x, ymin = lower, ymax = upper), 
                  fill = "darkblue", alpha = 0.2) + 
      labs(title = title, x = "x", y = "y") + 
      theme_bw()
}

plot_with_ci(data, model1, TRUE)
plot_with_ci(data, model2, FALSE)


#### DEPRECATED
### p <- ggplot(data, aes(x = x1, y = y)) +
###      geom_point(color = "black") +  # Scatter plot
###      geom_smooth(method = "lm", color = "darkred", fill = "darkgray", level = 0.95) +
###      labs(title = "Linear Regression with Confidence Interval", x = "x", y = "y")
### p



