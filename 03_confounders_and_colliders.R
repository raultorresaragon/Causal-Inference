# ----------------------------------------------
# Author: Raul
# Date: 2024-12-23
# Note: Confouders and Colliders
# ----------------------------------------------

rm(list = ls())
set.seed(1234)
library(tidyverse)
library(stargazer)
library(dagitty)


# Simple counfounder
# ------------------

fig_confounder <- dagitty("dag{ A -> Y; A <- U -> Y}")
coordinates(fig_confounder) <- list(x=c(A=-1, U=0, Y=1),
                                    y=c(A=1, U=-1, Y=1))
plot(fig_confounder)

n <- 100
tb <- tibble(a = ifelse(runif(n)>0.5, 1, 0),
             u = 5*a + rnorm(n),
             y = 1 + 2.5*a + 1.5*u + rnorm(n))
mod1 <- lm(y ~ a, tb)
mod2 <- lm(y ~ a + u, tb)
stargazer(mod1, mod2,type = "text", column.labels = c("biased", "unbiased conditional"))



# Google collider
# ---------------

# Google was once accused of paying women less. They responded, and this has been common
# among many companies facing similar accusations, that once you account for location, 
# tenure, 'job role', and level, the pay gap goes away.
# Okay, but what if the discrimination affects job role. In other words, what if they only 
# consider men for certain job roles.

fig_google <- dagitty("dag{F -> Y; F -> JR; JR -> Y; JR <- U -> Y}")
coordinates(fig_google) <- list(x=c(JR=-1, F=-0.8, Y=0.5, U=1),
                                y=c(JR=0.6, F=-1,Y=1,U=-0.6))
plot(graphLayout(fig_google))

# Assume being female has an negative effect on earnings (Y) since women are discriminated 
# against). But also assume that being female has an effect on job role because only men
# are considered for higher-paying job roles.
# Now let's say some unobserved factor (such as ability) also affects both, job role and
# earnings (Y). 

tb <- tibble(female = ifelse(runif(n)>=0.5, 1, 0),
             ability = rnorm(n),
             jobrole = 1 + 2*ability - 2*female + rnorm(n),
             wage = 1 - 1.4*female + 1*jobrole + 2*ability + rnorm(n))

mod1 <- lm(wage ~ female, tb)
mod2 <- lm(wage ~ female + jobrole, tb)
mod3 <- lm(wage ~ female + jobrole + ability, tb)
stargazer(mod1, mod2, mod3, 
          type = "text", 
          column.labels = c("naive", "google", "true"))


# In model 1, we see the total effect of being female on earnings. This includes the 
# effect of female through job role. 
# Then Google says, "no, we have to account for job role." 
# If there is discrimination against women on term of job role, then this will open up
# a backdoor path F->JR<-U->Y, where JR is a collider that will bias F.
# When we account for U we close the backdoor path F->JR<-U->Y and we can isolate the 
# effect of F on Y that is not mediated through JR. 


