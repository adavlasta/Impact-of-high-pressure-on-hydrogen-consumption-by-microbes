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

my_data <- data.frame(time = c(0, 10, 15, 20, 25, 30, 35, 40, 45, 50), 
                      logN = c(1.2, 1, 1.7, 1.9, 2.3, 3.1, 3.3, 3.8, 4.3, 4.1)
                      )

ggplot(my_data) + geom_point(aes(x = time, y = logN))

## -----------------------------------------------------------------------------
primary_model_data()

## -----------------------------------------------------------------------------
models <- list(primary = "Baranyi")

## -----------------------------------------------------------------------------
primary_model_data("Baranyi")$pars

## -----------------------------------------------------------------------------
start <- c(logNmax = 6, lambda = 25, logN0 = 2, mu = .2)  # Initial guess

check_growth_guess(
  my_data,
  models,
  start
)

## -----------------------------------------------------------------------------
auto_guess <- make_guess_primary(my_data, "Baranyi")
print(auto_guess)

## -----------------------------------------------------------------------------

check_growth_guess(
  my_data,
  models,
  auto_guess
)


## -----------------------------------------------------------------------------
known <- c()

## -----------------------------------------------------------------------------
primary_fit <- fit_growth(my_data, 
                          models, 
                          auto_guess, 
                          known,
                          environment = "constant",
                          )

## -----------------------------------------------------------------------------
print(primary_fit)

## -----------------------------------------------------------------------------
summary(primary_fit)

## -----------------------------------------------------------------------------
plot(primary_fit, line_col = "red", point_shape = 1)

## -----------------------------------------------------------------------------
known <- c(logNmax = 4)

## -----------------------------------------------------------------------------
new_guess <- auto_guess[c("logN0", "mu", "lambda")]

new_fit <-  fit_growth(my_data, 
                       models, 
                       new_guess, 
                       known,
                       environment = "constant",
                       )

## -----------------------------------------------------------------------------
new_fit

## -----------------------------------------------------------------------------
summary(new_fit)

## -----------------------------------------------------------------------------
time_to_size(primary_fit, 2)

## -----------------------------------------------------------------------------
time_to_size(primary_fit, 8)

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
secondary_model_data()

## -----------------------------------------------------------------------------
sec_model_names <- list(temperature = "Zwietering", aw = "CPM")

## -----------------------------------------------------------------------------
known_pars <- c(Nmax = 1e4,  # Primary model
                   N0 = 1e0, Q0 = 1e-3,  # Initial values of the primary model
                   mu_opt = 4, # mu_opt of the gamma model
                   temperature_n = 1,  # Secondary model for temperature
                   aw_xmax = 1, aw_xmin = .9, aw_n = 1  # Secondary model for water activity
                   )

## -----------------------------------------------------------------------------
my_start <-  c(temperature_xmin = 25, temperature_xopt = 35, aw_xopt = .95)


## -----------------------------------------------------------------------------
check_growth_guess(
  example_dynamic_growth,
  sec_model_names,
  c(my_start, known_pars),
  environment = "dynamic",
  env_conditions = example_env_conditions
)

## -----------------------------------------------------------------------------

my_dyna_fit <- fit_growth(example_dynamic_growth, 
                          sec_model_names, 
                          my_start, known_pars,
                          environment = "dynamic",
                          env_conditions = example_env_conditions
                          ) 

## -----------------------------------------------------------------------------
print(my_dyna_fit)

## -----------------------------------------------------------------------------
summary(my_dyna_fit)

## -----------------------------------------------------------------------------
plot(my_dyna_fit, add_factor = "temperature")

## -----------------------------------------------------------------------------
time_to_size(my_dyna_fit, 3)

## -----------------------------------------------------------------------------
data("multiple_counts")
names(multiple_counts)
head(multiple_counts[[1]])


## -----------------------------------------------------------------------------
data("multiple_conditions")
names(multiple_conditions)
head(multiple_conditions[[1]])

## -----------------------------------------------------------------------------
sec_models <- list(temperature = "CPM", pH = "CPM")

## Any model parameter (of the primary or secondary models) can be fixed

known_pars <- list(Nmax = 1e8, N0 = 1e0, Q0 = 1e-3,
                   temperature_n = 2, temperature_xmin = 20, 
                   temperature_xmax = 35,
                   pH_n = 2, pH_xmin = 5.5, pH_xmax = 7.5, pH_xopt = 6.5,
                   temperature_xopt = 30)
                   
## The rest, need initial guesses

my_start <- list(mu_opt = .8)

## -----------------------------------------------------------------------------
check_growth_guess(
  multiple_counts,
  sec_models,
  c(my_start, known_pars),
  environment = "dynamic",
  env_conditions = multiple_conditions,
  approach = "global"
)

## -----------------------------------------------------------------------------
global_fit <- fit_growth(multiple_counts, 
                         sec_models, 
                         my_start, 
                         known_pars,
                         environment = "dynamic",
                         algorithm = "regression",
                         approach = "global",
                         env_conditions = multiple_conditions
                         ) 

## -----------------------------------------------------------------------------
print(global_fit)

## -----------------------------------------------------------------------------
summary(global_fit)

## -----------------------------------------------------------------------------
plot(global_fit)

## -----------------------------------------------------------------------------
plot(global_fit, add_factor = "temperature",
     label_y2 = "Temperature (ºC)",
     line_col2 = "green",
     line_type2 = "dotted")

## -----------------------------------------------------------------------------
time_to_size(global_fit, 3)

## -----------------------------------------------------------------------------
time_to_size(global_fit, 5)

## -----------------------------------------------------------------------------
data("example_cardinal")
head(example_cardinal)

## -----------------------------------------------------------------------------
secondary_model_data()

## -----------------------------------------------------------------------------
sec_model_names <- c(temperature = "Zwietering",
                     pH = "CPM")

## -----------------------------------------------------------------------------
sec_guess <- make_guess_secondary(example_cardinal, sec_model_names)
sec_guess

## -----------------------------------------------------------------------------
known <- c()

## -----------------------------------------------------------------------------
fit_cardinal <- fit_secondary_growth(example_cardinal, 
                                     sec_guess,
                                     known, 
                                     sec_model_names)

## ---- error=TRUE--------------------------------------------------------------
summary(fit_cardinal)

## -----------------------------------------------------------------------------
known_pars <- list(mu_opt = 1.2,
                   temperature_n = 1,
                   pH_n = 2, pH_xmax = 6.8, pH_xmin = 5.2
                   )


## -----------------------------------------------------------------------------
my_start <- list(temperature_xmin = 5, temperature_xopt = 35,
               pH_xopt = 6.5)

## -----------------------------------------------------------------------------
fit_cardinal <- fit_secondary_growth(example_cardinal, 
                                     my_start,
                                     known_pars, 
                                     sec_model_names)

## -----------------------------------------------------------------------------
summary(fit_cardinal)

## -----------------------------------------------------------------------------
print(fit_cardinal)

## -----------------------------------------------------------------------------
plot(fit_cardinal)

## -----------------------------------------------------------------------------
plot(fit_cardinal, which = 2)

## -----------------------------------------------------------------------------
plot(fit_cardinal, which = 2, add_trend = TRUE)

