## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6
)

## ----setup--------------------------------------------------------------------
library(biogrowth)
library(tidyverse)

## -----------------------------------------------------------------------------
my_model <- "modGompertz"

## -----------------------------------------------------------------------------
pars <- tribble(
    ~par, ~mean, ~sd, ~scale,
    "logN0", 0, .2, "original",
    "mu", 2, .3, "sqrt",
    "lambda", .5, .1, "log",
    "C", 6, .5, "original"
    )

## -----------------------------------------------------------------------------
my_times <- seq(0, 15, length = 100)

## -----------------------------------------------------------------------------
n_sims <- 1000

## -----------------------------------------------------------------------------
unc_growth <- predict_growth_uncertainty(my_model, my_times, n_sims, pars)

## -----------------------------------------------------------------------------
print(unc_growth)

## -----------------------------------------------------------------------------
plot(unc_growth)

## -----------------------------------------------------------------------------
plot(unc_growth, ribbon80_fill = "purple", ribbon90_fill = "pink", alpha80 = .8)

## -----------------------------------------------------------------------------
my_cor <- matrix(c(1,   0,    0, 0,
                   0,   1, -0.7, 0,
                   0, -0.7,   1, 0,
                   0,    0,   0, 1),
                   nrow = 4)

## -----------------------------------------------------------------------------
unc_growth2 <- predict_growth_uncertainty(my_model, my_times, n_sims, pars, my_cor)

plot(unc_growth2)

## -----------------------------------------------------------------------------
## We will use the data included in the package

data("multiple_counts")
data("multiple_conditions")

## We need to assign a model equation for each environmental factor

sec_models <- list(temperature = "CPM", pH = "CPM")

## Any model parameter (of the primary or secondary models) can be fixed

known_pars <- list(Nmax = 1e8, N0 = 1e0, Q0 = 1e-3,
                   temperature_n = 2, temperature_xmin = 20, 
                   temperature_xmax = 35,
                   pH_n = 2, pH_xmin = 5.5, pH_xmax = 7.5, pH_xopt = 6.5)
                   
## The rest, need initial guesses

my_start <- list(mu_opt = .8, temperature_xopt = 30)


set.seed(12421)
global_MCMC <- fit_growth(multiple_counts, 
                         sec_models, 
                         my_start, 
                         known_pars,
                         environment = "dynamic",
                         algorithm = "MCMC",
                         approach = "global",
                         env_conditions = multiple_conditions,
                         niter = 100,
                         lower = c(.2, 29),  # lower limits of the model parameters
                         upper = c(.8, 34)  # upper limits of the model parameters
                         ) 

## -----------------------------------------------------------------------------
plot(global_MCMC$fit_results)

## -----------------------------------------------------------------------------
my_conditions <- tibble(time = c(0, 40),
                        temperature = c(30, 30),
                        pH = c(7, 7)
                        )

## -----------------------------------------------------------------------------
my_times <- seq(0, 40, length = 50)

## -----------------------------------------------------------------------------
niter <- 50

## -----------------------------------------------------------------------------
set.seed(124)
uncertain_prediction <- predictMCMC(global_MCMC,
                                    my_times,
                                    my_conditions, 
                                    niter = niter
                                    )

## -----------------------------------------------------------------------------
print(uncertain_prediction)

## -----------------------------------------------------------------------------
plot(uncertain_prediction)

## -----------------------------------------------------------------------------
uncertain_prediction2 <- predictMCMC(global_MCMC,
                                    my_times,
                                    my_conditions, 
                                    niter = 5,
                                    newpars = list(mu_opt = 0.5)
                                    )

## -----------------------------------------------------------------------------
uncertain_prediction2$sample

## -----------------------------------------------------------------------------
unc_distrib <- time_to_size(type = "distribution", unc_growth, 3)

## -----------------------------------------------------------------------------
print(unc_distrib)

## -----------------------------------------------------------------------------
summary(unc_distrib)

## -----------------------------------------------------------------------------
plot(unc_distrib)

