## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(biogrowth)
library(tidyverse)

## -----------------------------------------------------------------------------
my_model <- "modGompertz"

## -----------------------------------------------------------------------------
my_pars <- list(logN0 = 2, C = 6, mu = .1, lambda = 50)

## -----------------------------------------------------------------------------
my_time <- seq(0, 200, length = 1000)

## -----------------------------------------------------------------------------
static_prediction <- predict_isothermal_growth(my_model, my_time, my_pars)


## -----------------------------------------------------------------------------
static_prediction$simulation

## -----------------------------------------------------------------------------
plot(static_prediction)

## -----------------------------------------------------------------------------
plot(static_prediction) +
  xlab("Storage time (h)") +
  ylab("Microbial count (log CFU/ml)") +
  theme_gray()

## -----------------------------------------------------------------------------
plot(static_prediction,
     line_col = "darkgreen", line_size = 2, line_type = 3) +
  xlab("Storage time (h)") +
  ylab("Microbial count (log CFU/ml)") 

## -----------------------------------------------------------------------------
my_conditions <- tibble(Time = c(0, 5, 40),
                         temperature = c(20, 30, 35),
                         pH = c(7, 6.5, 5)
                         )

## -----------------------------------------------------------------------------
ggplot(my_conditions) + 
  geom_line(aes(x = Time, y = temperature))

## -----------------------------------------------------------------------------
ggplot(my_conditions) + 
  geom_line(aes(x = Time, y = pH))

## -----------------------------------------------------------------------------
my_primary <- list(mu_opt = .9,
             Nmax = 1e8,
             N0 = 1e0,
             Q0 = 1e-3)

## -----------------------------------------------------------------------------
sec_temperature <- list(model = "Zwietering",
                        xmin = 25,
                        xopt = 35,
                        n = 1)

## -----------------------------------------------------------------------------
sec_pH <- list(model = "CPM",
               xmin = 5.5,
               xopt = 6.5,
               xmax = 7.5,
               n = 2)

## -----------------------------------------------------------------------------
my_secondary <- list(
    temperature = sec_temperature,
    pH = sec_pH
    )

## -----------------------------------------------------------------------------
my_times <- seq(0, 50, length = 1000)

## -----------------------------------------------------------------------------
dynamic_prediction <- predict_dynamic_growth(my_times,
                       my_conditions, my_primary,
                       my_secondary,
                       formula = . ~ Time)

## -----------------------------------------------------------------------------
dynamic_prediction$simulation

## -----------------------------------------------------------------------------
plot(dynamic_prediction)

## -----------------------------------------------------------------------------
plot(dynamic_prediction, add_factor = "temperature")

## -----------------------------------------------------------------------------
plot(dynamic_prediction, 
     add_factor = "temperature", 
     ylims= c(0, 7), 
     label_y1 = "Microbial count (log CFU/ml)", 
     label_y2 = "Storage temperature (ºC)",
     line_col = "lightgreen", 
     line_size = 2, line_type2 = 1
     ) +
  xlab("Storage time (h)")

## -----------------------------------------------------------------------------
time_to_logcount(static_prediction, 2.5)

## -----------------------------------------------------------------------------
time_to_logcount(dynamic_prediction, 5)

## -----------------------------------------------------------------------------
time_to_logcount(dynamic_prediction, 10)

## -----------------------------------------------------------------------------
my_data <- tibble(time = c(0, 25, 50, 75, 100),
                  log_size = c(2, 2.5, 7, 8, 8))

## -----------------------------------------------------------------------------
my_formula <- log_size ~ time

## -----------------------------------------------------------------------------
my_model <- "Baranyi"

## -----------------------------------------------------------------------------
known <- c(mu = .2)

## -----------------------------------------------------------------------------
start = c(logNmax = 8, lambda = 25, logN0 = 2)

## -----------------------------------------------------------------------------
static_fit <- fit_isothermal_growth(my_data, my_model,
                                    start, known,
                                    formula = my_formula
                                    )

## -----------------------------------------------------------------------------
summary(static_fit)

## -----------------------------------------------------------------------------
plot(static_fit)

## -----------------------------------------------------------------------------
plot(static_fit, 
     line_size = 2, point_col = "lightblue", point_size = 5)

## -----------------------------------------------------------------------------
data("example_dynamic_growth")
data("example_env_conditions")

## -----------------------------------------------------------------------------
head(example_env_conditions)

## -----------------------------------------------------------------------------
ggplot(example_env_conditions, aes(x = time, y = temperature)) + 
  geom_line() +
  geom_point()

## -----------------------------------------------------------------------------
head(example_dynamic_growth)

## -----------------------------------------------------------------------------
sec_model_names <- c(temperature = "CPM",
                     aw= "CPM")

## -----------------------------------------------------------------------------
known_pars <- list(Nmax = 1e4,  # Nmax for primary model
                   N0 = 1e0, Q0 = 1e-3,  # Initial values of the primary model
                   mu_opt = 4, # mu_opt of the gamma model
                   temperature_n = 1, temperature_xmax = 40,  # Secondary model for temperature
                   aw_xmax = 1, aw_xmin = .9, aw_n = 1  # Secondary model for water activity
                   )

## -----------------------------------------------------------------------------
my_start <- list(temperature_xmin = 25, temperature_xopt = 35,
               aw_xopt = .95)


## -----------------------------------------------------------------------------
my_dyna_fit <- fit_dynamic_growth(example_dynamic_growth, example_env_conditions, 
                         my_start,
                         known_pars, 
                         sec_model_names)

## -----------------------------------------------------------------------------
summary(my_dyna_fit)

## -----------------------------------------------------------------------------
plot(my_dyna_fit)

## -----------------------------------------------------------------------------
plot(my_dyna_fit, add_factor = "aw",
     label_y1 = "Log count (log CFU/ml)",
     label_y2 = "Water activity",
     line_col = "pink",
     line_col2 = "yellow",
     point_col = "lightgreen") +
  theme_dark()

## -----------------------------------------------------------------------------
data("multiple_experiments")

## -----------------------------------------------------------------------------
ggplot(multiple_experiments[[1]]$data) + 
  geom_point(aes(x = time, y = logN)) 

## ---- fig.width=7, fig.height=5-----------------------------------------------

multiple_experiments[[1]]$conditions %>%
  pivot_longer(-time, names_to = "condition", values_to = "value") %>%
  ggplot() +
  geom_line(aes(x = time, y = value)) +
  facet_wrap("condition", scales = "free")
  

## -----------------------------------------------------------------------------
sec_names <- c(temperature = "CPM", pH = "CPM")

## -----------------------------------------------------------------------------
known <- list(Nmax = 1e8, N0 = 1e0, Q0 = 1e-3,
    temperature_n = 2, temperature_xmin = 20, temperature_xmax = 35,
    pH_n = 2, pH_xmin = 5.5, pH_xmax = 7.5, pH_xopt = 6.5)

start <- list(mu_opt = .8, temperature_xopt = 30)


## -----------------------------------------------------------------------------
global_fit <- fit_multiple_growth(start, multiple_experiments, known, sec_names,
                                  lower = c(.5, 25),
                                  upper = c(1, 33))

## -----------------------------------------------------------------------------
summary(global_fit)

## ---- fig.width=7, fig.height=5-----------------------------------------------
plot(global_fit)

## ---- fig.width=7, fig.height=5-----------------------------------------------
plot(global_fit, add_factor = "temperature")

## ---- fig.width=7, fig.height=5-----------------------------------------------
plot(global_fit, add_factor = "temperature",
     label_x = "Storage time (h)",
     label_y1 = "Size of the population (log CFU/g)",
     label_y2 = "Temperature (ºC)",
     line_col = "maroon", line_size = 2,
     line_type2 = 1, line_col2 = "darkgrey"
     )

## -----------------------------------------------------------------------------

data("example_dynamic_growth")
data("example_env_conditions")

sec_model_names <- c(temperature = "CPM",
                     aw= "CPM")

known_pars <- list(Nmax = 1e4,  # Primary model
                   N0 = 1e0, Q0 = 1e-3,  # Initial values of the primary model
                   mu_opt = 4, # mu_opt of the gamma model
                   temperature_n = 1, temperature_xmax = 40, # Secondary model for temperature
                   aw_xmax = 1, aw_xmin = .9, aw_n = 1  # Secondary model for water activity
)

my_start <- list(temperature_xmin = 25, 
                 temperature_xopt = 35,
                 aw_xopt = .95)


## -----------------------------------------------------------------------------
my_MCMC_fit <- fit_MCMC_growth(example_dynamic_growth, example_env_conditions, 
                                my_start,
                                known_pars, 
                                sec_model_names, 
                                niter = 100) 


## -----------------------------------------------------------------------------
summary(my_MCMC_fit)

## -----------------------------------------------------------------------------
plot(my_MCMC_fit)

## -----------------------------------------------------------------------------
plot(my_MCMC_fit, add_factor = "temperature", 
     point_col = "steelblue", point_shape = 2, point_size = 6)

## -----------------------------------------------------------------------------
data("multiple_experiments")

## -----------------------------------------------------------------------------
## For each environmental factor, we need to defined a model

sec_names <- c(temperature = "CPM", pH = "CPM")

## Any model parameter can be fixed

known <- list(Nmax = 1e8, N0 = 1e0, Q0 = 1e-3,
    temperature_n = 2, temperature_xmin = 20, temperature_xmax = 35,
    pH_n = 2, pH_xmin = 5.5, pH_xmax = 7.5, pH_xopt = 6.5)

## The rest require starting values for model fitting

start <- list(mu_opt = .8, temperature_xopt = 30)


## -----------------------------------------------------------------------------
set.seed(12412)
global_MCMC <- fit_multiple_growth_MCMC(start, multiple_experiments, known, sec_names, 
                                        niter = 100,
                                        lower = c(.2, 29),  # lower limits of the model parameters
                                        upper = c(1.6, 34))  # upper limits of the model parameters


## -----------------------------------------------------------------------------
summary(global_MCMC)

## ---- fig.width = 7, fig.height=5---------------------------------------------
plot(global_MCMC)

## ---- fig.width = 7, fig.height=5---------------------------------------------
plot(global_MCMC, add_factor = "temperature",
     line_col = "grey",
     line_col2 = "blue", line_size2 = .5, line_type2 = 3)

## -----------------------------------------------------------------------------
example_env_conditions

## -----------------------------------------------------------------------------
my_times <- seq(0, 15, length = 5)

## -----------------------------------------------------------------------------
niter <- 100

## ---- warning=FALSE-----------------------------------------------------------

my_MCMC_prediction <- predict_MCMC_growth(my_MCMC_fit, 
                                          my_times,
                                          example_env_conditions, 
                                          niter)

## -----------------------------------------------------------------------------
my_MCMC_prediction$quantiles

## -----------------------------------------------------------------------------
plot(my_MCMC_prediction)

## -----------------------------------------------------------------------------
better_prediction <- predict_MCMC_growth(my_MCMC_fit, 
                                          seq(0, 15, length = 100),
                                          example_env_conditions, 
                                          niter)

## -----------------------------------------------------------------------------
plot(better_prediction)

## -----------------------------------------------------------------------------
other_prediction <- predict_MCMC_growth(my_MCMC_fit, 
                                        seq(0, 15, length = 100),
                                        example_env_conditions, 
                                        niter,
                                        newpars = list(aw_xopt = .96,
                                                        N0 = 10
                                                        )
                                         )
plot(other_prediction)

