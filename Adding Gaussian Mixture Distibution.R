load("data/stomach_dataset.Rdata")
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

df <- stom_df %>%
  transmute(Species = pred_species,
            wprey = prey_weight_g,
            wpredator = pred_weight_g,
            Nprey = prey_count / n_stomachs,
            l = log(wpredator / wprey))

stomach <- df %>%
  filter(Species == "Clupea harengus",
         wprey > 0)

stomach %>% 
  group_by(Species) %>% 
  summarise(wprey_min = min(wprey),
            wprey_max = max(wprey),
            lmin = min(l),
            lmax = max(l))

stomach %>% 
  group_by(Species) %>% 
  filter(wprey == max(wprey))

no_bins <- 30  # Number of bins
binsize <- (max(stomach$l) - min(stomach$l)) / (no_bins - 1)
breaks <- seq(min(stomach$l) - binsize/2,
              by = binsize, length.out = no_bins + 1)

binned_stomach <- stomach %>% 
  # bin data
  mutate(cut = cut(l, breaks = breaks, right = FALSE,
                   labels = FALSE)) %>% 
  group_by(Species, cut) %>% 
  summarise(Numbers = sum(Nprey), 
            Biomass = sum(Nprey * wprey)) %>% 
  # normalise
  mutate(Numbers = Numbers / sum(Numbers) / binsize,
         Biomass = Biomass / sum(Biomass) / binsize)  %>%
  # column for predator/prey size ratio
  mutate(l = map_dbl(cut, function(idx) breaks[idx] + binsize/2))
binned_stomach

binned_stomach <- binned_stomach %>%
  gather(key = "Type", value = "Density", Numbers, Biomass)

binned_stomach %>% 
  ggplot(aes(l, Density, fill = Type)) +
  geom_col(position = "dodge") +
  facet_grid(rows = vars(Species), scales = "free_y") +
  xlab("Log of Predator/Prey mass ratio") +
  expand_limits(x = c(0, 30))

adjust <- 1/2  # decrease bandwidth for kernel estimate
stomach <- stomach %>% 
  group_by(Species) %>% 
  mutate(weight_numbers = Nprey / sum(Nprey),
         weight_biomass = Nprey * wprey / sum(Nprey * wprey))
ggplot(stomach) +
  geom_density(aes(l, weight = weight_numbers,
                   fill = "Numbers"),
               adjust = adjust) +
  geom_density(aes(l, weight = weight_biomass,
                   fill = "Biomass"),
               adjust = adjust) +
  facet_grid(rows = vars(Species), scales = "free_y") +
  xlab("Log of predator/prey mass ratio") +
  expand_limits(x = c(0, 29))

weighted.sd <- function(x, w) {
  sqrt(sum(w * (x - weighted.mean(x, w))^2))
}
fit <- stomach %>% 
  summarise(mean = weighted.mean(l, weight_numbers),
            sd = weighted.sd(l, weight_numbers))
stomach %>% 
  ggplot() +
  geom_density(aes(l, weight = weight_numbers),
               fill = "#00BFC4", adjust = adjust) +
  xlab("Log of Predator/Prey mass ratio") +
  stat_function(fun = dnorm, 
                args = list(mean = fit$mean, 
                            sd = fit$sd), 
                colour = "blue") +
  expand_limits(x = c(6, 30))

stomach %>% 
  ggplot() +
  geom_density(aes(l, weight = weight_numbers,
                   fill = "Numbers"),
               adjust = adjust) +
  geom_density(aes(l, weight = weight_biomass,
                   fill = "Biomass"),
               adjust = adjust) +
  xlab("Log of Predator/Prey mass ratio") +
  stat_function(fun = dnorm, 
                args = list(mean = fit$mean, 
                            sd = fit$sd), 
                colour = "blue") +
  stat_function(fun = dnorm, 
                args = list(mean = fit$mean - fit$sd^2, 
                            sd = fit$sd), 
                colour = "red") +
  expand_limits(x = c(0, 30))



#trying to apply mixed model distribution 

library(MASS)
library(mclust, quietly = TRUE)

fit = Mclust(stomach$l, G = 2, model = "V")
summary(fit)
plot(fit, what = "density", main = "", 
     xlab = "PPMR")
rug(stomach$l)
library(sBIC)
gMix = GaussianMixtures(maxNumComponents = 2, 
                        phi = 1, restarts = 100)
set.seed(1234)
m = sBIC(stomach$l, gMix)
print(m)

matplot(
  cbind(m$BIC - m$BIC[1], m$sBIC - m$sBIC[1]),
  pch = c(1, 3),
  col = "black",
  xlab = "Number of components",
  ylab = expression(BIC - BIC(M[1])),
  las=1, xaxt="n"
)
axis(1, at = 1:10)
legend("topleft",
       c(expression(BIC), expression(bar(sBIC)[1])),
       pch = c(1, 3),
       y.intersp = 1.2)

stomach %>% 
  ggplot() +
  geom_density(aes(l, weight = weight_numbers,
                   fill = "Numbers"),
               adjust = adjust) +
  geom_density(aes(l, weight = weight_biomass,
                   fill = "Biomass"),
               adjust = adjust) +
  xlab("Log of Predator/Prey mass ratio") +
    expand_limits(x = c(0, 30)) 















library(mixtools)
wait <- stomach %>% 
  summarise(mean = weighted.mean(l, weight_numbers),
            sd = weighted.sd(l, weight_numbers))
mixmdl = normalmixEM(wait)
plot(mixmdl,which=2)
lines(density(wait), lty=2, lwd=2)

library(MGMM)
fit <- FitGMM(stomach[1:3516,1:7], k = 2)

wait1 <- normalmixEM(stomach, lambda = 0.5,
                    mu = c(1,5), sigma = 2)


estimatedDist = EstimatedDist

estimatedDist = EstimatedDistribution[dataSet,
                                      MixtureDistribution[{w1, w2}, {
                                        MultinormalDistribution[{m11, m12}, {{s111, s112}, {s112, s122}}],
                                        MultinormalDistribution[{m21, m22}, {{s211, s212}, {s212, s222}}]
                                      }]]

mvpdf <- function(x, mu, sigma) {
  if (det(sigma) == 0) {
    warning("Determinant is equal to 0.")
  }
  apply(x, 1, function(x) exp(-(1/2) * (t(x) - mu) %*% MASS::ginv(sigma) %*% 
                                t(t(x) - mu))/sqrt(det(2 * pi * sigma)))
}


# Mclust comes with a method of hierarchical clustering. We'll
# initialize 2 different classes.
library(mclust)
initialk <- mclust::hc(data = stomach, modelName = "EII")
initialk <- mclust::hclass(initialk, 2)
# First split by class and calculate column-means for each class.
mu <- split(stomach[,6:7], initialk)
mu <- t(sapply(mu, colMeans))
# Covariance Matrix for each initial class.
cov <- list(diag(4), diag(4), diag(4))
# Mixing Components
a <- runif(2)
a <- a/sum(a)

# Calculate PDF with class means and covariances.
z <- cbind(mvpdf(x = stomach[, 6:7], mu = mu[1, ], sigma = cov[[1]]), mvpdf(x = stomach[, 
                                                                                  6:7], mu = mu[2, ], sigma = cov[[2]]))                                    
# Expectation Step for each class.
r <- cbind((a[1] * z[, 1])/rowSums(t((t(z) * a))), (a[2] * z[, 2])/rowSums(t((t(z) * 
                                                                                a))), (a[3] * z[, 3])/rowSums(t((t(z) * a))))
# Choose the highest rowwise probability
eK <- factor(apply(r, 1, which.max))



library(mixtools)
my_mix <- normalmixEM(stomach$l, k = 2)
plot(my_mix, which = 2)

my_mix$mu
my_mix$sigma
my_mix$lambda

stomach %>% 
  ggplot() +
  geom_density(aes(l, weight = weight_numbers,
                   fill = "Numbers"),
               adjust = adjust) +
  geom_density(aes(l, weight = weight_biomass,
                   fill = "Biomass"),
               adjust = adjust) +
  xlab("Log of Predator/Prey mass ratio") +
  stat_function(fun = dnorm, 
                args = list(mean = my_mix[["mu"]][[1]], 
                            sd = my_mix[["sigma"]][[1]]), colour = "blue") +
  stat_function(fun = dnorm, 
                args = list(mean = my_mix[["mu"]][[2]], 
                            sd = my_mix[["sigma"]][[2]]), colour = "red") +
  expand_limits

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




