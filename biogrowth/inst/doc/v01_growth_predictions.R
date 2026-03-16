## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(biogrowth)
library(tidyverse)

## -----------------------------------------------------------------------------
primary_model_data()

## -----------------------------------------------------------------------------
my_model <- "Baranyi"

## -----------------------------------------------------------------------------
primary_model_data(my_model)$pars

## -----------------------------------------------------------------------------
primary_model <- list(model = my_model, logN0 = 1, logNmax = 7, mu = .2, lambda = 20)

## -----------------------------------------------------------------------------
my_times <- seq(0, 100, length = 1000)

## -----------------------------------------------------------------------------
my_prediction <- predict_growth(environment = "constant", my_times, primary_model)

## -----------------------------------------------------------------------------
my_prediction

## -----------------------------------------------------------------------------
coef(my_prediction)

## ---- fig.width=6-------------------------------------------------------------
plot(my_prediction)

## ---- fig.width=6-------------------------------------------------------------
plot(my_prediction,
     label_y1 = "log10 of the population size",
     label_x = "Time (years)",
     line_size = 2,
     line_col = "red",
     line_type = "dotted")

## ---- fig.width=6-------------------------------------------------------------
plot(my_prediction) + theme_gray() + xlab("Time (years)")

## -----------------------------------------------------------------------------
time_to_size(my_prediction, 3)

## -----------------------------------------------------------------------------
time_to_size(my_prediction, 9)

## ---- fig.width = 6-----------------------------------------------------------
primary_model <- list(model = my_model, logN0 = 1, logNmax = 7, mu = .2, lambda = 20)

Q0 <- lambda_to_Q0(lambda = 20, mu = .2)

equivalent_pars <- list(model = my_model, N0 = 10^1, Nmax = 10^7, mu_opt = .2, 
                        Q0 = Q0)

equivalent_prediction <- predict_growth(environment = "constant", my_times, 
                                        equivalent_pars)

plot(equivalent_prediction)


## ---- fig.width=6-------------------------------------------------------------
primary_model <- list(model = my_model, logN0 = 1, logNmax = 7, mu = .2, lambda = 20)
prediction_base_e <- predict_growth(environment = "constant", my_times, 
                                        primary_model,
                                        logbase_logN = exp(1))

prediction_base_e
plot(prediction_base_e)

## ---- fig.width=6-------------------------------------------------------------
primary_model <- list(model = my_model, N0 = 1, Nmax = 1e7, mu = .2, lambda = 20)
prediction_base_e <- predict_growth(environment = "constant", my_times, 
                                        primary_model,
                                        logbase_logN = exp(1))

plot(prediction_base_e)

## ---- fig.width=6-------------------------------------------------------------
primary_model <- list(model = my_model, logN0 = 1, logNmax = 7, mu = .2, lambda = 20)
prediction_mu_e <- predict_growth(environment = "constant", my_times, 
                                        primary_model,
                                        logbase_mu = exp(1))

prediction_mu_e
plot(prediction_mu_e)

## ---- fig.width=6-------------------------------------------------------------
primary_model <- list(model = my_model, logN0 = 1, logNmax = 7, 
                      mu = .2 * log(10), 
                      lambda = 20)
prediction_mu_e <- predict_growth(environment = "constant", my_times, 
                                        primary_model,
                                        logbase_mu = exp(1)
                                  )

plot(prediction_mu_e)

## -----------------------------------------------------------------------------
print(prediction_base_e)
time_to_size(prediction_base_e, size = 5)

## -----------------------------------------------------------------------------
time_to_size(prediction_base_e, log10(exp(5)), logbase_logN = 10)

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
secondary_model_data()

## -----------------------------------------------------------------------------
secondary_model_data("Zwietering")$pars

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
dynamic_prediction <- predict_growth(environment = "dynamic", 
                                     my_times, 
                                     my_primary, 
                                     my_secondary,
                                     my_conditions,
                                     formula = . ~ Time
                                     )

## -----------------------------------------------------------------------------
dynamic_prediction

## ---- fig.width=6-------------------------------------------------------------
plot(dynamic_prediction)

## ---- fig.width=6-------------------------------------------------------------
plot(dynamic_prediction,
     add_factor = "temperature",
     line_col2 = "steelblue",
     line_col = "magenta",
     label_y2 = "Temperature (ºC)")

## -----------------------------------------------------------------------------
time_to_size(dynamic_prediction, 3)

