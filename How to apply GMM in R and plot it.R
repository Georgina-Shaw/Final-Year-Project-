library(tidyverse)
library(mixtools)

#just a plotting theme
ggplot2::theme_set(
  theme_bw() +
    theme(
      plot.title = element_text(size = 14),
      axis.title = element_text(size = 14, colour = "grey30"),
      axis.text = element_text(size = 12, colour = "grey30"),
      panel.grid = element_blank()
    )
)

set.seed(12) #better make this reproducible
observations <- tibble(value = c(
  rnorm(n = 125, mean = 0.1, sd = 0.2), #the first normal distribution
  rnorm(n = 250, mean = 0.8, sd = 0.2) #this second distribution is double the size of the first
)
)

#and a quick gander...
ggplot(observations, aes(x = value)) + 
  geom_histogram(binwidth = 0.05)

my_mix <- normalmixEM(observations$value, k = 2)
plot(my_mix, which = 2)

my_mix[["mu"]]
my_mix[["sigma"]]
my_mix[["lambda"]]

ggplot(observations, aes(x = value)) +
  geom_histogram(binwidth = 0.05) +
  mapply(
    function(mean, sd, lambda, n, binwidth) {
      stat_function(
        fun = function(x) {
          (dnorm(x, mean = mean, sd = sd)) * n * binwidth * lambda
        }
      )
    },
    mean = my_mix[["mu"]], #mean
    sd = my_mix[["sigma"]], #standard deviation
    lambda = my_mix[["lambda"]], #amplitude
    n = length(observations$value), #sample size
    binwidth = 0.05 #binwidth used for histogram
  )


#for 4 observations 
observations <- tibble(value = c(
  rnorm(n = 125, mean = 0.1, sd = 0.2),
  rnorm(n = 175, mean = 0.4, sd = 0.05),
  rnorm(n = 250, mean = 0.6, sd = 0.05),
  rnorm(n = 250, mean = 0.8, sd = 0.2)
)
)

my_mix <- normalmixEM(observations$value, k = 4)

ggplot(observations, aes(x = value)) + 
  geom_histogram(binwidth = 0.05) +
  mapply(function(mean, sd, lambda, n, binwidth) {
    stat_function(
      fun = function(x) {
        (dnorm(x, mean = mean, sd = sd)) * n * binwidth * lambda
      }
    )
  },
  mean = my_mix[["mu"]], 
  sd = my_mix[["sigma"]],
  lambda = my_mix[["lambda"]],
  n = length(observations$value),
  binwidth = 0.05
  )


