## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(biogrowth)
library(tidyverse)
library(cowplot)

## ---- echo=FALSE, fig.width=6-------------------------------------------------

predict_growth(seq(0, 25, length = 100),
               list(model = "Baranyi", mu = .5, lambda = 5, logNmax = 8, logN0 = 2)) %>%
  plot() +
  geom_label(aes(x = x, y = y, label = label), 
             data = tibble(x = c(2, 11, 22),
                           y = 9,
                           label = c("Lag phase", "Exponential phase", "Stationary phase")
                           )
             ) +
  geom_vline(xintercept = c(5, 17),
             linetype = 2) +
  scale_x_continuous(name = "Elapsed time", breaks = NULL) +
  scale_y_continuous(name = "Logarithm of the population size", breaks = NULL)

## -----------------------------------------------------------------------------
predict_growth(seq(0, 5, length = 100),
               list(model = "Trilinear",
                    logN0 = 2, lambda = 1, logNmax = 9, mu = 1),
               environment = "constant") %>%
  plot() +
  theme_gray()

## -----------------------------------------------------------------------------
predict_growth(seq(0, 5, length = 100),
               list(model = "Trilinear",
                    logN0 = 2, lambda = 1, logNmax = 9, mu = 1),
               environment = "constant",
               logbase_mu = exp(1)) %>%
  plot() +
  theme_gray() +
  ylim(2, 6)

## ---- fig.width = 6-----------------------------------------------------------

my_time <- seq(0, 20, length = 1000)  # Vector of time points for the calculations

list(
    `base 2` = predict_growth(my_time,
                   list(model = "Baranyi",
                        logN0 = 0, logNmax = 6, mu = 1*log(10, base = 2), 
                        lambda = 5),
                   environment = "constant", 
                   logbase_mu = 2),
    `base 10` = predict_growth(my_time,
                   list(model = "Baranyi",
                        logN0 = 0, logNmax = 6, mu = 1, 
                        lambda = 5),
                   environment = "constant"),
    `base e` = predict_growth(my_time,
                   list(model = "Baranyi",
                        logN0 = 0, logNmax = 6, mu = 1*log(10), 
                        lambda = 5),
                   environment = "constant",
                   logbase_mu = exp(1))
    ) %>%
    map(., ~.$simulation) %>%
    imap_dfr(., ~ mutate(.x, model = as.character(.y))) %>%
    ggplot() +
    geom_line(aes(x = time, y = logN, colour = model, linetype = model, size = model)) +
  scale_size_manual(values = c(3, 2, 1))

## -----------------------------------------------------------------------------
my_data <- data.frame(time = c(0, 25, 50, 75, 100), 
                      logN = c(2, 2.5, 7, 8, 8))

models <- list(primary = "Baranyi")

known <- c(lambda = 25)

start <- c(logNmax = 8, logN0 = 2, mu = .3)

primary_fit10 <- fit_growth(my_data, models, start, known,
                          environment = "constant"
)



## -----------------------------------------------------------------------------
primary_fit10

## -----------------------------------------------------------------------------
primary_fit10$logbase_mu

## -----------------------------------------------------------------------------
primary_fit_e <- fit_growth(my_data, models, start, known,
                          environment = "constant",
                          logbase_mu = exp(1)
                          )

## -----------------------------------------------------------------------------
plot_grid(plot(primary_fit_e), plot(primary_fit10))

## -----------------------------------------------------------------------------
print(primary_fit_e)

## -----------------------------------------------------------------------------
print(primary_fit10)

## -----------------------------------------------------------------------------
coef(primary_fit10)["mu"] * log(10)

## -----------------------------------------------------------------------------
q0 <- 1e5
mu_opt <- 1  # in log10 scale

my_primary <- list(mu_opt = mu_opt,
                   Nmax = 1e8,N0 = 1e2,
                   Q0 = q0)

sec_temperature <- list(model = "CPM",
                        xmin = 5, xopt = 35, xmax = 40, n = 2)

my_secondary <- list(temperature = sec_temperature)

## -----------------------------------------------------------------------------
my_conditions <- data.frame(time = c(0, 5),
                            temperature = c(35, 35)
                            )

my_times <- seq(0, 5, length = 1000)

dynamic_prediction <- predict_growth(environment = "dynamic", 
                                     my_times,
                                     my_primary,
                                     my_secondary,
                                     my_conditions)


## -----------------------------------------------------------------------------
plot(dynamic_prediction) + theme_gray()

## -----------------------------------------------------------------------------
predict_growth(environment = "dynamic", 
               my_times,
               my_primary,
               my_secondary,
               my_conditions,
               logbase_mu = exp(1)) %>%
  plot() + theme_gray() + ylim(2, 7)

## -----------------------------------------------------------------------------

my_conditions <- data.frame(time = c(0, 5, 40),
                            temperature = c(20, 30, 35)
                            )

sec_temperature <- list(model = "Zwietering",
    xmin = 25, xopt = 35, n = 1)

my_secondary <- list(
    temperature = sec_temperature
    )

my_times <- seq(0, 50, length = 1000)


list(`base 10` = predict_growth(environment = "dynamic", 
                                     my_times, 
                                     list(mu = 2, Nmax = 1e7, N0 = 1, Q0 = 1e-3), 
                                     my_secondary,
                                     my_conditions
                                     ),
     `base e` = predict_growth(environment = "dynamic", 
                                     my_times, 
                                     list(mu = 2*log(10), Nmax = 1e7, N0 = 1, Q0 = 1e-3), 
                                     my_secondary,
                                     my_conditions,
                                     logbase_mu = exp(1)
                                     ),
     `base 2` = predict_growth(environment = "dynamic", 
                                     my_times, 
                                     list(mu = 2*log(10, base=2), Nmax = 1e7, N0 = 1, Q0 = 1e-3), 
                                     my_secondary,
                                     my_conditions,
                                     logbase_mu = 2
                                     )
     ) %>%
    map(., ~.$simulation) %>%
    imap_dfr(., ~ mutate(.x, model = as.character(.y))) %>%
    ggplot() +
    geom_line(aes(x = time, y = logN, colour = model, linetype = model, size = model)) +
  scale_size_manual(values = c(3, 2, 1))



## -----------------------------------------------------------------------------
q0 <- 1e-3
mu_opt <- 1  # in ln scale

my_primary <- list(mu_opt = mu_opt,
                   Nmax = 1e8,
                   N0 = 1e2,
                   Q0 = q0)

sec_temperature <- list(model = "CPM",
                        xmin = 5, xopt = 35, xmax = 40, n = 2)

my_secondary <- list(temperature = sec_temperature)

my_conditions <- data.frame(time = c(0, 30),
                            temperature = c(35, 35)
                            )

my_times <- seq(0, 30, length = 1000)

dynamic_prediction <- predict_growth(environment = "dynamic", 
                                     my_times,
                                     my_primary,
                                     my_secondary,
                                     my_conditions,
                                     logbase_mu = exp(1))

plot(dynamic_prediction)


## -----------------------------------------------------------------------------
bad_lambda <- Q0_to_lambda(q0, mu_opt)

bad_primary_model <- list(model = "Baranyi",
                          logN0 = 2, logNmax = 8, mu = mu_opt, 
                          lambda = bad_lambda)


bad_prediction <- predict_growth(my_times, bad_primary_model,
                                 logbase_mu = exp(1))

plot(bad_prediction, line_col = "red") +
    geom_line(aes(x = time, y = logN), linetype = 2, 
              data = dynamic_prediction$simulation)


## -----------------------------------------------------------------------------
good_lambda <- Q0_to_lambda(q0, mu_opt, logbase_mu = exp(1))

good_primary_model <- list(model = "Baranyi",
                           logN0 = 2, logNmax = 8, mu = mu_opt, 
                           lambda = good_lambda)


good_prediction <- predict_growth(my_times, good_primary_model,
                                  logbase_mu = exp(1))

plot(good_prediction, line_col = "green") +
    geom_line(aes(x = time, y = logN), linetype = 2,
              data = dynamic_prediction$simulation)

