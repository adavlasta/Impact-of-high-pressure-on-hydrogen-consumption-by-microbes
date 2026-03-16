## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(biogrowth)
library(tidyverse)
library(cowplot)

## -----------------------------------------------------------------------------
my_conditions <- data.frame(time = c(0, 50),
                            temperature = c(35, 35)
                            )

## -----------------------------------------------------------------------------
q0 <- 1e-4
mu_opt <- .5

my_primary <- list(mu_opt = mu_opt,
                   Nmax = 1e8,N0 = 1e2,
                   Q0 = q0)

sec_temperature <- list(model = "CPM",
                        xmin = 5, xopt = 35, xmax = 40, n = 2)

my_secondary <- list(temperature = sec_temperature)

## -----------------------------------------------------------------------------
my_times <- seq(0, 50, length = 1000)

## Do the simulation

dynamic_prediction <- predict_growth(environment = "dynamic", 
                                     my_times,
                                     my_primary,
                                     my_secondary,
                                     my_conditions)


## -----------------------------------------------------------------------------
lambda <- Q0_to_lambda(q0, mu_opt)

primary_model <- list(model = "Baranyi", 
                      logN0 = 2, logNmax = 8, mu = mu_opt, lambda = lambda)


static_prediction <- predict_growth(my_times, primary_model)

plot(static_prediction) +
    geom_line(aes(x = time, y = logN), linetype = 2, 
              data = dynamic_prediction$simulation,
              colour = "green")


## -----------------------------------------------------------------------------
max_time <- 100

c(15, 20, 25, 30, 35) %>%  # Temperatures for the calculation
  set_names(., .) %>%
  map(.,  # Definition of constant temperature profile
      ~ data.frame(time = c(0, max_time),
               temperature = c(., .))
      ) %>%
  map(.,  # Growth simulation for each temperature
      ~ predict_growth(environment = "dynamic", 
                                     my_times,
                                     my_primary,
                                     my_secondary,
                                     env_conditions = .,
                                     logbase_mu = 10)
      ) %>%
  imap_dfr(.,  # Extract the simulation
           ~  mutate(.x$simulation, temperature = .y)
           ) %>%
  ggplot() +
  geom_line(aes(x = time, y = logN, colour = temperature)) +
  theme_cowplot()

## -----------------------------------------------------------------------------
my_primary <- list(mu_opt = mu_opt,
                   Nmax = 1e8,N0 = 1e2,
                   Q0 = q0)

sec_temperature <- list(model = "CPM",
                        xmin = 5, xopt = 35, xmax = 40, n = 2)

sec_pH <- list(model = "CPM",
               xmin = 4, xopt = 7, xmax = 8, n = 2)

my_secondary_2 <- list(temperature = sec_temperature,
                     pH = sec_pH)

## -----------------------------------------------------------------------------
max_time <- 100

c(5, 5.5, 6, 6.5, 7, 7.5) %>%  # pH values for the calculation
  set_names(., .) %>%
  map(.,  # Definition of constant temperature profile
      ~ tibble(time = c(0, max_time),
               temperature = c(35, 35),
               pH = c(., .))
      ) %>%
  map(.,  # Growth simulation for each temperature
      ~ predict_growth(environment = "dynamic", 
                                     my_times,
                                     my_primary,
                                     my_secondary_2,
                                     env_conditions = .,
                                     logbase_mu = 10)
      ) %>%
  imap_dfr(.,  # Extract the simulation
           ~  mutate(.x$simulation, pH = .y)
           ) %>%
  ggplot() +
  geom_line(aes(x = time, y = logN, colour = pH)) +
  theme_cowplot()

