## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6
)

## ----setup--------------------------------------------------------------------
library(biogrowth)
library(tidyverse)
library(cowplot)

## -----------------------------------------------------------------------------
data("example_dynamic_growth")
data("example_env_conditions")

sec_models <- list(temperature = "CPM", aw = "CPM")

known_pars <- list(Nmax = 1e4,  # Primary model
                   N0 = 1e0, Q0 = 1e-3,  # Initial values of the primary model
                   mu_opt = 4, # mu_opt of the gamma model
                   temperature_n = 1,  # Secondary model for temperature
                   aw_xmax = 1, aw_xmin = .9, aw_n = 1  # Secondary model for water activity
                   )
my_start <- list(temperature_xmin = 25, temperature_xopt = 35,
                 temperature_xmax = 40, aw_xopt = .95)
                 
my_model <- fit_growth(example_dynamic_growth, 
                       sec_models, 
                       my_start, known_pars,
                       environment = "dynamic",
                       env_conditions = example_env_conditions
                       ) 

## -----------------------------------------------------------------------------
plot(my_model)

## -----------------------------------------------------------------------------
plot(my_model, 
     line_col = "red", 
     line_size = 1,
     line_type  = "dashed",
     label_y1 = "Population size (log-millions)",
     label_x = "Time (years)",
     point_size = 3,
     point_shape = 1,
     point_col = "darkgrey")


## -----------------------------------------------------------------------------
plot(my_model, 
     line_col = "red", 
     line_size = 1,
     line_type  = "dashed",
     label_y1 = "Population size (log-millions)",
     label_x = "Time (years)",
     point_size = 3,
     point_shape = 1,
     point_col = "darkgrey") +
  theme_gray() +
  theme(axis.title = element_text(colour = "green", size = 14))


## -----------------------------------------------------------------------------
plot(my_model) + 
  coord_cartesian(xlim = c(5, 10), ylim = c(0, 4)) # changing the axis limits

## -----------------------------------------------------------------------------
p1 <- plot(my_model, add_factor = "temperature")
p2 <- plot(my_model, add_factor = "aw")
plot_grid(p1, p2, labels = "AUTO")


## -----------------------------------------------------------------------------
# We save the plot as p1, notice that it does not get drawn now
p1 <- plot(my_model, line_col = "red")

# We save P1 as .pdf, as a 20x 20 cm square
# ggsave("static_prediction.pdf", p1, width = 20, height = 20, units = "cm")

## -----------------------------------------------------------------------------
my_model$best_prediction

## -----------------------------------------------------------------------------
head(my_model$best_prediction$simulation)

## -----------------------------------------------------------------------------
ggplot(my_model$best_prediction$simulation) +
  geom_line(aes(x = time, y = Q)) +
  scale_y_log10()

