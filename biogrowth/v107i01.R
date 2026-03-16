
## ---- echo=FALSE------------------------------------------------------------
library("tidyverse")
library("cowplot")
library("biogrowth")
library("cowplot")
library("readxl")

## ----  fig.height = 4, fig.cap = "Illustration of a typical growth curve.", echo=FALSE, message=FALSE----
predict_growth(
  seq(0, 25, length = 100),
  list(model = "Baranyi", mu = .5, lambda = 5, logNmax = 8, logN0 = 2)
) %>%
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


## ----  fig.height = 4, echo = FALSE, fig.cap="Gamma factor according to the Cardinal Parameter Model (CPM) for an arbitrary environmental factor, $X$. The lines represent the effect of changing the shape factor ($n$), while the remaining parameters are fixed: $X_{min} = 10$ (minimum value for growth), $X_{opt}=35$ (optimum value for growth), $X_{max} = 40$ (maximum value for growth).", warning=FALSE----

tibble(
  x = seq(0, 50, length = 500)
) %>%
  mutate(`n = 2` = biogrowth:::CPM_model(x, xmin = 10, xopt = 35, xmax = 40, n = 2),
         `n = 3` = biogrowth:::CPM_model(x, xmin = 10, xopt = 35, xmax = 40, n = 3),
         `n = 1` = biogrowth:::CPM_model(x, xmin = 10, xopt = 35, xmax = 40, n = 1)) %>%
  pivot_longer(-x, names_to = "model", values_to = "mu") %>%
  ggplot() +
  geom_line(aes(x, mu, colour = model)) +
  geom_vline(xintercept = c(10, 35, 40), linetype = 2) +
  geom_label(aes(x = x, y = y, label = label), size = 3,
             data = tibble(x = c(10, 35, 40),
                           y = c(1.2, 1.2, 1.2),
                           label = c("Xmin", "Xopt", "Xmax"))
             ) +
  ylim(0, 1.3) +
  theme_cowplot() +
  theme(legend.title = element_blank(),
        legend.position = "top") +
  xlab("X") + ylab("Gamma factor (unitless)")



## ----  fig.height = 4, eval=FALSE------------------------------------------------------------
## install.packages("biogrowth")


## ----  fig.height = 4, eval=FALSE------------------------------------------------------------
## devtools::install_github("albgarre/biogrowth")


## ---------------------------------------------------------------------------
data("refrigeratorSpain", package = "biogrowth")
head(refrigeratorSpain)


## ---------------------------------------------------------------------------
secondary_model_data()


## ---------------------------------------------------------------------------
meta_info <- secondary_model_data("CPM")
meta_info$pars


## ---------------------------------------------------------------------------
sec_temperature <- list(model = "CPM", xmin = 1.4, 
                        xopt = 36.8, xmax = 41.0, n = 2)


## ---------------------------------------------------------------------------
my_secondary <- list(A1 = sec_temperature)


## ---------------------------------------------------------------------------
my_primary <- list(mu_opt = 2.61, Nmax = 1e7, N0 = 100, Q0 = 1e3)


## ---------------------------------------------------------------------------
my_times <- seq(0, max(refrigeratorSpain$time), length = 1000)

cereus_A1 <- predict_growth(environment = "dynamic", 
                            my_times, 
                            my_primary, my_secondary,
                            refrigeratorSpain,
                            logbase_mu = exp(1)
                            )


## ----  fig.height = 4, fig.cap="Prediction for the growth of \\emph{B. cereus} in a domestic refrigerator (black line). The grey line is the temperature profile."----
plot(cereus_A1, add_factor = "A1",
     label_y2 = "Temperature (ºC)",
     label_y1 = "Microbial count (log CFU/g)",
     label_x = "Storage time (h)",
     line_type2 = "solid",
     line_col2 = "grey")


## ---------------------------------------------------------------------------
my_secondary <- list(A2 = sec_temperature)

cereus_A2 <- predict_growth(environment = "dynamic", 
                            my_times,
                            my_primary, my_secondary,
                            refrigeratorSpain,
                            logbase_mu = exp(1)
                            )


## ----  fig.height = 5, fig.width = 10, fig.cap="Prediction for the growth of \\emph{B. cereus} in two domestic refrigerators (black line). The grey line is the temperature profile."----
plot_grid(labels = c("A1", "A2"),
  plot(cereus_A1, add_factor = "A1", ylims = c(2, 3), 
       label_y2 = "Temperature (ºC)", 
       label_y1 = "Microbial count (log CFU/g)",
       label_x = "Storage time (h)", 
       line_type2 = "solid", line_col2 = "grey"
       ),
  plot(cereus_A2, add_factor = "A2", ylims = c(2, 3), 
       label_y2 = "Temperature (ºC)", 
       label_y1 = "Microbial count (log CFU/g)",
       label_x = "Storage time (h)", 
       line_type2 = "solid", line_col2 = "grey")
  )


## ---------------------------------------------------------------------------
data("greek_tractors", package = "biogrowth")


## ---------------------------------------------------------------------------
greek_tractors <- greek_tractors %>%
  mutate(logtractors = log10(Value),
         t_model = Year - min(Year)
         )


## ---------------------------------------------------------------------------
primary_model_data()


## ---- error = TRUE----------------------------------------------------------
meta_info <- primary_model_data("Logistic")
meta_info$pars


## ---------------------------------------------------------------------------
my_guess <- make_guess_primary(greek_tractors,
                               primary_model = "Logistic",
                               formula = logtractors ~ t_model)

my_guess


## ----  fig.height = 4, fig.cap = "Visualization of the initial guess generated automatically for the data on the number of tractors."----
check_growth_guess(
  greek_tractors,
  model_keys =  list(primary = "Logistic"),
  guess = my_guess,
  formula = logtractors ~ t_model
  )


## ---------------------------------------------------------------------------
greek_logistics <- fit_growth(
  greek_tractors,
  list(primary = "Logistic"),
  start = my_guess,
  known = c(),
  formula = logtractors ~ t_model
)

greek_logistics


## ---- fig.cap = "Fit of the logistic model to the number of tractors in Greece between 1961 (year 0 in the plot) and 2006.", fig.height = 4----
plot(greek_logistics, 
     label_x = "Year", label_y1 = "log10 of the number of tractors")


## ---------------------------------------------------------------------------
summary(greek_logistics)


## ---------------------------------------------------------------------------
greek_logistics_noLag <- fit_growth(
  greek_tractors,
  list(primary = "Logistic"),
  start = c(mu = 0.03, logN0 = 4, C = 1),
  known = c(lambda = 0),
  formula = logtractors ~ t_model
  )


## ---------------------------------------------------------------------------
tractor_comparison <- compare_growth_fits(
  list(`Logistic`= greek_logistics,
       `Logistic - no lag` = greek_logistics_noLag
       )
  )

tractor_comparison


## ----  fig.height = 4, fig.cap="Comparison of the model fits of the two logistic models to the tractor data"----
plot(tractor_comparison)


## ----  fig.height = 4, fig.cap="Comparison of the residuals of the two logistic models fitted to the tractor data."----
plot(tractor_comparison, type = 3)


## ----  fig.height = 4, fig.cap="Comparison of the model parameters estimated with both logistic models from the tractor data. Dots represent estimated values and the error bars their standard errors."----
plot(tractor_comparison, type = 2)


## ----  fig.height = 4, fig.cap = "Comparison between the fit of both models (including extrapolation) to  the number of tractors in Greece between 1961 and 2006 when the lag phase is fixed to one."----
library("ggplot2")

tibble(
  year = 1950:2020,
  t_model = year - min(greek_tractors$Year)
  ) %>%
  mutate(Logistic = predict(greek_logistics, 
                            times = t_model),
         `Logistic no lag` = predict(greek_logistics_noLag, 
                                     times = t_model)
         ) %>%
  pivot_longer(-c(t_model, year),
               names_to = "model",
               values_to = "logtractors") %>%
  ggplot() +
  geom_line(aes(x = year, y = logtractors, colour = model), size = 1) +
  geom_point(aes(x = Year, y = logtractors),
             data = greek_tractors, size = 2) +
  ylab("log10 of the number of tractors") +
  xlab("Year") +
  theme_cowplot() +
  theme(legend.title = element_blank(),
        legend.position = "top")


## ---------------------------------------------------------------------------
pars <- coef(tractor_comparison) %>%
  filter(model == "Logistic") %>%
  select(-model, par = parameter, mean = estimate, sd = std.err) %>%
  mutate(scale = "original")

pars


## ---- fig.height = 4, fig.cap = "Prediction band for the number of tractors in Greece accounting for parameter uncertainty. The lightblue ribbon shows the interval for the 0.9 level and the darkblue the 0.8 interval."----
set.seed(12412)

greek_uncertainty <- predict_growth_uncertainty(
  greek_logistics$primary_model, 
  times = seq(0, 50, length = 100), 
  n_sims = 1500, 
  pars)

plot(greek_uncertainty, 
     ribbon80_fill = "darkblue", 
     ribbon90_fill = "lightblue") +
  xlab("Year") + 
  ylab("log10 of the number of tractors")



## ---------------------------------------------------------------------------
distrib_to_4p7 <- time_to_size(greek_uncertainty, 
                               size = 4.7, 
                               type = "distribution")

distrib_to_4p7


## ---- fig.height = 4, fig.cap="Distribution of the time to reach a population size of 4.7 log for the tractor data. The vertical, red line represents the median of the results. The vertical, gray lines the 10th and 90th percentiles."----
plot(distrib_to_4p7)


## ---------------------------------------------------------------------------
data("example_cardinal", package = "biogrowth")
head(example_cardinal)


## ---------------------------------------------------------------------------
sec_model_names <- c(temperature = "CPM", pH = "CPM")


## ---------------------------------------------------------------------------
my_guess <- make_guess_secondary(example_cardinal, sec_model_names)
print(my_guess)


## ---------------------------------------------------------------------------
fit_cardinal <- fit_secondary_growth(example_cardinal, 
                                     my_guess, 
                                     sec_model_names,
                                     known_pars = c()
                                     )


## ---- error=TRUE------------------------------------------------------------
## summary(fit_cardinal)


## ---------------------------------------------------------------------------
known_pars <- list(
  ## Secondary model for temperature
  temperature_n = 1,
  temperature_xmax = 40,
  ## Secondary model for pH
  pH_n = 2,
  pH_xmax = 7,
  pH_xmin = 5.2
  )

my_start <- my_guess[c("temperature_xmin", "temperature_xopt", 
                       "pH_xopt", "mu_opt")]

fixed_cardinal <- fit_secondary_growth(example_cardinal, 
                                       my_start,
                                       known_pars,
                                       sec_model_names)


## ---------------------------------------------------------------------------
summary(fixed_cardinal)


## ---- fig.height = 4, fig.cap="Comparison between the model fits and the predictions for the secondary model fitted to simulated data. The solid grey line is a linear regression model for fitted vs. observed and the dashed black line represents a perfect fit. "----
plot(fixed_cardinal)


## ---- fig.height = 4, fig.cap = "Comparison between the trend of the fitted model and the one of the observations for the secondary model fitted to simulated data. Observations are shown in gray and model fits in black."----
plot(fixed_cardinal, which = 2, add_trend = TRUE, add_segment = TRUE)


## ---- fig.height = 4, fig.cap = "Illustration of the data retrieved from ComBase on the growth of \\emph{L. monocytogenes} in pork."----
listeria_counts <- read_excel("listeria_combase.xlsx",
                              sheet = "Logcs") %>%
  mutate(Time = as.numeric(Time), Logc = as.numeric(Logc))

temperatures <- read_excel("listeria_combase.xlsx",
                           sheet = "Temperatures") %>%
  mutate(Time = as.numeric(Time), Temperature = as.numeric(Temperature))

pH_values <- read_excel("listeria_combase.xlsx", sheet = "pHs") %>%
  mutate(Time = as.numeric(Time), pH = as.numeric(pH))

p1 <- ggplot(listeria_counts, aes(x = Time, y = Logc)) +
  geom_point() +
  theme(legend.position = "top")

p2 <- ggplot(temperatures, aes(x = Time, y = Temperature)) +
  geom_line() +
  theme(legend.position = "top")


p3 <- ggplot(pH_values, aes(x = Time, y = pH)) +
  geom_line() +
  theme(legend.position = "top")

plot_grid(p1, p2, p3)


## ---------------------------------------------------------------------------
env_profile <- full_join(temperatures, pH_values,
                         by = c("Record ID", "Time")
                         )


## ---------------------------------------------------------------------------
sec_models <- list(Temperature = "Zwietering", pH = "Zwietering")


## ---------------------------------------------------------------------------
logN0 <- min(listeria_counts$Logc)

known <- c(Q0 = 1e10,
              Temperature_n = 2,
              pH_n = 2,
              Nmax = 1e8,
              N0 = 10^logN0
              )


## ---- fig.height = 4, fig.cap="Initial guess for fitting the growth model to the growth of \\emph{L. monocytogenes} in pork observed under dynamic conditions."----
start <- c(mu_opt = 1,
              Temperature_xopt = 35,
              Temperature_xmin = 5,
              pH_xmin = 4,
              pH_xopt = 7
              )


check_growth_guess(listeria_counts,
                   sec_models,
                   c(start, known),
                   env_profile,
                   environment = "dynamic",
                   formula = Logc ~ Time
                   )


## ---------------------------------------------------------------------------
listeria_model <- fit_growth(environment = "dynamic",
                             listeria_counts,
                             sec_models,
                             start,
                             known,
                             env_conditions = env_profile,
                             formula = Logc ~ Time
                             )


## ---- fig.height = 4, fig.cap="Fit of the Baranyi model with secondary model based on the gamma concept to the data on growth of \\emph{Listeria monocytogenes} under dynamic conditions (black). The temperature profile is shown as a gray line."----
plot(listeria_model, add_factor = "Temperature",
     line_type2 = 1, line_col2 = "grey",
     label_y1 = "Microbial count (log10 CFU/g)",
     label_y2 = "Temperature (ºC)",
     label_x = "Time (h)",
     point_shape = 1, point_size = 5)


## ---------------------------------------------------------------------------
summary(listeria_model)


## ---------------------------------------------------------------------------
known <- c(known, Temperature_xopt = 35, pH_xopt = 7)
start <- start[c("mu_opt", "Temperature_xmin", "pH_xmin")]

reduced_model <- fit_growth(environment = "dynamic",
                            listeria_counts,
                            as.list(sec_models),
                            start,
                            known,
                            env_conditions = env_profile,
                            formula = Logc ~ Time
                            )


summary(reduced_model)


## ---------------------------------------------------------------------------
comparison_listeria <- compare_growth_fits(
  list(Standard = listeria_model,
       Reduced = reduced_model)
  )



## ---- fig.height = 4, fig.cap="Comparison of the two model fits to the data on the growth of \\emph{L. monocytogenes} under dynamic conditions."----
plot(comparison_listeria)


## ---------------------------------------------------------------------------
print(comparison_listeria)


## ---- fig.height = 4, fig.cap="Comparison of the parameters estimates for the two models fitted to the data on the growth of \\emph{L. monocytogenes} under dynamic conditions. Note that the parameters that were fixed in the reduced model are not shown in their respective facets."----
plot(comparison_listeria, type = 2)


## ---------------------------------------------------------------------------
listeria_counts2 <- read_excel("listeria_combase2.xlsx",
                              sheet = "Logcs") %>%
  mutate(Time = as.numeric(Time), Logc = as.numeric(Logc))

temperatures2 <- read_excel("listeria_combase2.xlsx",
                           sheet = "Temperatures") %>%
  mutate(Time = as.numeric(Time), Temperature = as.numeric(Temperature))

pH_values2 <- read_excel("listeria_combase2.xlsx", sheet = "pHs") %>%
  mutate(Time = as.numeric(Time), pH = as.numeric(pH))


## ---------------------------------------------------------------------------
multiple_listeria <- list(R1 = listeria_counts,
                          R2 = listeria_counts2)


## ---------------------------------------------------------------------------
env_profile2 <- full_join(temperatures2, pH_values2,
                         by = c("Record ID", "Time")
                         )

multiple_profile <- list(R1 = env_profile,
                         R2 = env_profile2)


## ---- cache=TRUE------------------------------------------------------------
set.seed(12421)

global_fit <- fit_growth(multiple_listeria,
                         sec_models,
                         start,
                         known,
                         environment = "dynamic",
                         algorithm = "MCMC",
                         approach = "global",
                         env_conditions = multiple_profile,
                         formula = Logc ~ Time,
                         niter = 6000,
                         updatecov = 500,
                         burninlength = 1000
                         )


## ---- fig.height = 5, fig.width = 10, fig.cap="Global fit of the growth model for \\emph{L. monocytogenes} in pork meat from two independent experiments (black line). The pH profile is shown in maroon."----
plot(global_fit, add_factor = "pH", label_y2 = "pH value",
     label_y1 = "Microbial concentration (log CFU/g)",
     label_x = "Time (h)", line_col2 = "maroon", line_type2 = 1)


## ---- cache=TRUE------------------------------------------------------------
pred_cond <- tibble(time = c(0, 1000),
                    Temperature = c(5),
                    pH = c(7, 5))

set.seed(14241)

pred_uncertainty <- predictMCMC(global_fit,
            seq(0, 1000, length = 100),
            pred_cond,
            niter = 1000,
            formula = . ~ time,
            newpars = list(N0 = 1)
            )


## ---- fig.height = 4, fig.cap="Prediction band for the growth of \\emph{L. monocytogenes} under dynamic environmental conditions including parameter uncertainty. The grey and blue ribbons represent, respectively, the credible intervals at the 0.9 and 0.8 levels. The dashed, black line represents the pH profile."----
plot(pred_uncertainty, add_factor = "pH",
     label_y2 = "pH value", 
     label_y1 = "Microbial concentration (log CFU/g)",
     label_x = "Time (h)", 
     fill_80 ="lightblue", 
     line_col = "darkblue")


## ---------------------------------------------------------------------------
time_to_100 <- time_to_size(pred_uncertainty, 2, type = "distribution")
time_to_100


## ---- fig.height = 4, fig.cap="Estimated distribution for the time to reach 100 CFU/g for \\emph{L. monocytogenes}. The vertical, red line represents the median of the simulations, whereas the vertical, gery lines show the 10th and 90th percentiles."----
plot(time_to_100)

