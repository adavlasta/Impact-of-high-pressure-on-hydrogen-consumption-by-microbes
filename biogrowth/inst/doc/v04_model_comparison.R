## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(biogrowth)
library(tidyverse)

## -----------------------------------------------------------------------------
data("growth_salmonella")
head(growth_salmonella)

## -----------------------------------------------------------------------------
fit1 <- fit_growth(growth_salmonella, 
                   list(primary = "Baranyi"), 
                   start = c(lambda = 0, logNmax = 8, mu = .1, logN0 = 2), 
                   known = c(),
                   environment = "constant"
                   )

## -----------------------------------------------------------------------------
fit2 <- fit_growth(growth_salmonella, 
                   list(primary = "Baranyi"), 
                   start = c(logNmax = 8, mu = .1, logN0 = 2), 
                   known = c(lambda = 0),
                   environment = "constant"
                   )

## -----------------------------------------------------------------------------
fit3 <- fit_growth(growth_salmonella, 
                   list(primary = "modGompertz"), 
                   start = c(C = 8, mu = .1, logN0 = 2, lambda = 0), 
                   known = c(),
                   environment = "constant"
                   )

## -----------------------------------------------------------------------------
my_models <- list(
  Baranyi = fit1,
  `Baranyi no-lag` = fit2,
  `mod Gompertz` = fit3
)

salmonella_models <- compare_growth_fits(my_models)

## -----------------------------------------------------------------------------
print(salmonella_models)

## ---- fig.width=6-------------------------------------------------------------
plot(salmonella_models)

## ---- fig.width=6-------------------------------------------------------------
plot(salmonella_models, type = 2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## ---- fig.width=6-------------------------------------------------------------
plot(salmonella_models, type = 3)

## -----------------------------------------------------------------------------
summary(salmonella_models)

## -----------------------------------------------------------------------------
coef(salmonella_models)

## -----------------------------------------------------------------------------
data("multiple_counts")
data("multiple_conditions")

sec_models <- list(temperature = "CPM", pH = "CPM")

known_pars <- list(Nmax = 1e8, N0 = 1e0, Q0 = 1e-3,
                   temperature_n = 2, temperature_xmin = 20, 
                   temperature_xmax = 35,
                   pH_n = 2, pH_xmin = 5.5, pH_xmax = 7.5, pH_xopt = 6.5,
                   temperature_xopt = 30)

my_start <- list(mu_opt = .8)

global_fit <- fit_growth(multiple_counts, 
                         sec_models, 
                         my_start, 
                         known_pars,
                         environment = "dynamic",
                         algorithm = "regression",
                         approach = "global",
                         env_conditions = multiple_conditions
) 

sec_models <- list(temperature = "CPM", pH = "CPM")

known_pars <- list(Nmax = 1e8, N0 = 1e0, Q0 = 1e-3,
                   temperature_n = 1, temperature_xmin = 20, 
                   temperature_xmax = 35,
                   pH_n = 2, pH_xmin = 5.5, pH_xmax = 7.5, pH_xopt = 6.5,
                   temperature_xopt = 30)

my_start <- list(mu_opt = .8)

global_fit2 <- fit_growth(multiple_counts, 
                         sec_models, 
                         my_start, 
                         known_pars,
                         environment = "dynamic",
                         algorithm = "regression",
                         approach = "global",
                         env_conditions = multiple_conditions
) 


## -----------------------------------------------------------------------------
global_comparison <- compare_growth_fits(list(`n=2` = global_fit, `n=1` = global_fit2))

## ---- fig.width=6-------------------------------------------------------------
plot(global_comparison)

## ---- fig.width=6-------------------------------------------------------------
plot(global_comparison, type = 3)

## ---- fig.width=6-------------------------------------------------------------
print(global_comparison)
plot(global_comparison, type = 2)
summary(global_comparison)
coef(global_comparison)


## -----------------------------------------------------------------------------
data("example_cardinal")
head(example_cardinal)

## -----------------------------------------------------------------------------
sec_model_names <- c(temperature = "Zwietering", pH = "CPM")

known_pars <- list(mu_opt = 1.2, temperature_n = 1,
                   pH_n = 2, pH_xmax = 6.8, pH_xmin = 5.2)
                   
my_start <- list(temperature_xmin = 5, temperature_xopt = 35,
                 pH_xopt = 6.5)
                 
fit1 <- fit_secondary_growth(example_cardinal, my_start, known_pars, sec_model_names)

known_pars <- list(mu_opt = 1.2, temperature_n = 2,
                   pH_n = 2, pH_xmax = 6.8, pH_xmin = 5.2)
                   
fit2 <- fit_secondary_growth(example_cardinal, my_start, known_pars, sec_model_names)


## -----------------------------------------------------------------------------
my_models <- list(`n=1` = fit1, `n=2` = fit2)

secondary_comparison <- compare_secondary_fits(my_models)

## ---- fig.width=6-------------------------------------------------------------
print(secondary_comparison)

plot(secondary_comparison)
plot(secondary_comparison, type = 2)

coef(secondary_comparison)
summary(secondary_comparison)

