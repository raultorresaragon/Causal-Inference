# ----------------------------------------------
# Author: Raul
# Date: 2024-12-23
# Note: a form of simpson paradox
#       Why is there a negative correlation between
#       restuarant quality and view, or
#       between beauty and intelligence, or between 
#       rigor and revelance in scientific journals?
#       I first heard about this in Statistical Rethinking
#       written by a statistician 
# ----------------------------------------------

rm(list = ls())
set.seed(1234)
library(tidyverse)
library(ggplot2)

n <- 5e3
tb <- tibble(
  view = rnorm(n),
  taste = rnorm(n),
  score = view + taste,
  cutoff85 = quantile(score, .85),
  famous_restaurant = ifelse(score>=0.85, 1, 0)
)

myplot <- function(data) {
  lm(taste ~ view, data=data) |>
  ggplot(aes(x = view, y = taste)) + 
  geom_point(size = 0.5,shape = 23) + 
  xlim(-4,4) + 
  ylim(-4,4) +
  theme_bw() + 
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 
}


library(patchwork)
p1 <- myplot(tb)
p2 <- myplot(tb |> filter(famous_restaurant == 0))
p3 <- myplot(tb |> filter(famous_restaurant == 1))
p1 + p2 + p3
