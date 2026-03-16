## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(tidyverse)
library(biogrowth)

## -----------------------------------------------------------------------------
my_model <- "modGompertz" 
my_time <- seq(0, 100, length = 1000) 

## -----------------------------------------------------------------------------
set.seed(12412)
niter <- 500

par_sample <- tibble(logN0 = rnorm(niter, mean = 0, sd = 1),
                     C = 6,
                     mu = rgamma(niter, shape = 3, rate = 5),
                     lambda = rgamma(niter, shape = 2, rate = 2))

par_sample %>% 
    pivot_longer(everything()) %>%
    ggplot() + geom_histogram(aes(value)) + facet_wrap("name", scales = "free")

## -----------------------------------------------------------------------------
my_predictions <- par_sample %>%
    pmap(., function(logN0, mu, lambda, C) 
        list(model = my_model,
             logN0 = logN0, 
             mu = mu,
             lambda = lambda,
             C = C)
        ) %>%
    map(., 
        ~ predict_growth(my_time, .)
        )


## -----------------------------------------------------------------------------
summary_preds <- my_predictions %>%
    map(., ~.$simulation) %>%
    imap_dfr(., ~ mutate(.x, sim = .y)) %>%
    group_by(time) %>%
    summarize(m_logN = median(logN), 
              q10 = quantile(logN, .1), 
              q90 = quantile(logN, .9))

## -----------------------------------------------------------------------------
summary_preds %>%
    ggplot(aes(x = time)) +
    geom_ribbon(aes(ymin = q10, ymax = q90), alpha = .5) +
    geom_line(aes(y = m_logN))

