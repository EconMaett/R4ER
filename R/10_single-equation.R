# 10 - Single equation models ----

# A fundamental part of economic research is estimate the 
# relationship between variables.

# A good starting point is to use reduced forms of well-established
# models from the literature.

# For example, to analyze the effect of a certain variable on
# economic activity, we can use a specification derived 
# from the IS curve.


# Similarly, we can use (a variation of) the Phillips Curve to
# measure the impact of a variable on inflation.


# We will estimate the parameters of a simple reduced form
# Phillips Curve.


# We use the seasonally-adjusted quarterly data for the
# Brazilian economy ranging from 2004-Q1 to 2022-Q4

# The basic Phillips Curve is assumed to follow the form
# \pi_{t} \equals \beta_{1} \pi_{t-1} + \beta_{2} \pi_{t,t+4|t}^{3}
# + \beta_{3} \delta e_{t-1} + \beta_{4} \tilde{y_{t-1}} + \epsilon_{t}

# Where `\pi_{t}` is a measure of inflation

# `\pi_{t,t+4|t}^e` is the expected inflation at time `t` for time `t+4`

# and `e` is a measure for the exchange rate or imported inflation

# `\tilde{y}` is a measure of the output gap.


# In this exercise, `\pi_{t}` is a measure of core inflation which
# excludes food-at-home and regulated prices (`CPI_CORE`)

# `\pi^{e}` are the market expectations compiled by the Brazilian
# Central Bank (`CPI_EXP`)

# `e` is an index of commodity prices in USD (`CI_USD`)

# `\tilde{y}` is the cycle component obtained from the HP Filter
# on the GDP series (`YGAP`).


# We start by importing the data set, which is provided
# on the book's GitHub page, and visualizing the variables
# of interest.

library(tidyverse)

cp_data <- readRDS(file = "data/ch12_cp_data.rds")

head(cp_data)
# date CPI_CORE YGAP CPI_EXP CI_USD

cp_data |> 
  pivot_longer(cols = -date, names_to = "var", values_to = "value") |> 
  ggplot(mapping = aes(x = date, y = value)) +
  geom_line(lwd = 1) +
  theme_bw() +
  facet_wrap(facets = ~ var, scales = "free_y") +
  labs(
    title = "Philipps Curve variables",
    x = "", y = "",
    caption = "Data source: Brazilian Central Bank"
  )

ggsave(filename = "figures/10_PC-variables.png", height = 8, width = 12)
graphics.off()


# Next, we create the approproiate variables for
# lagged `CPI` and `YGAP` and the percentage change of `CI_USD`.

# Then we fit the model to the data.

# Note that we are not imposing any restrictions on the coefficients
# at this point, although the structural version of the Philipps Curve
# does.


# Additionally, we use OLS to estimate the coefficients, 
# although an endogeneity-robust method such as the 
# Generalized Method of Moments (GMM) is more suitable.


# Remember the formula. 
# We regress today's inflation pi_t on
# - inflation in the last period (pi_t-1)
# - the output gap in the last period y
# - the change in the exchange rate in the last period e_t-1


cp_reg_data <- cp_data |> 
  relocate(date, CPI_CORE, CPI_EXP, CI_USD, YGAP) |> 
  mutate(
    CPI_CORE_lag = dplyr::lag(CPI_CORE, n = 1),
    YGAP_lag     = dplyr::lag(YGAP, n = 1),
    dlog_CI_USD  = log(CI_USD / dplyr::lag(CI_USD, n = 1)) * 100
  )

head(cp_reg_data)

# date CPI_CORE CPI_EXP CI_USD YGAP CPI_CORE_LAG YGAP_lag

cp_fit <- lm(
  formula = CPI_CORE ~ CPI_CORE_lag + CPI_EXP + YGAP_lag + dlog_CI_USD - 1, 
  data = cp_reg_data
)

summary(cp_fit)
# All coefficients are highly significant and show the
# expected signs.

# To check the model's validity, plot the residuals to see
# if they have a mean close to zero and do not exhibit
# a clear trend.

# The `forecast::checkresiduals()` function provides
# useful summaries of the model residuals
forecast::checkresiduals(cp_fit)
ggsave(filename = "figures/10_PC-residuals.png", height = 8, width = 12)
graphics.off()

# The residuals are well-behaved.
# The mean is around zero, there are no clear outliers and no clear trend.

# There is an autocorrelation signal in the third lag
# evidenced by the ACF plot, but given that it is small and far enough
# away we overlook it.


# We can use the estimated coefficients to provide rules
# of thumb for everyday use.

# For example, the `dlog_CI_USD` coefficient measures the
# pass-through from imported prices on domestic inflation.

# According to the model, a 10% increase in imported prices
# adds 0.17pp to domestic inflation in the current quarter.

summary(cp_fit)$coefficients["dlog_CI_USD", "Estimate"] * 10

# Plotting the model fit is a good way to check deviations
# of the target variable from its fundamentals (at least those
# taken into account in our model).

# We can use the `broom::augment()` function to return
# a data frame with the fitted values, residuals, etc.

# The `broom` R package from the `tidymodels` family of packages
# provides other handy functions to manipulate regression results.
library(broom)

cp_fit |> 
  broom::augment()
# .rownames CPI_CORE CPI_CORE_lag CPI_EXP YGAP_lag
# dlog_CI_USD .fitted .resid .hat .sigma .cooksd .std.resid


cp_reg_data |> colnames()
# date CPI_CORE CPI_EXP CI_USD YGAP YPI_CORE_lag YGAP_lag dlog_CI_USD

# We have lost the "date" variable in `cp_fit |> broom::augment()`
# We therefore add it again from `cp_reg_data`.

# The function `tibble::rowid_to_column()` allows us to add
# a new variable `.rownames` whose values are the row numbers.
# We can use this variable to match the two data frames.

cp_fit_plot <- cp_fit |> 
  augment() |> 
  mutate(
    .rownames = as.numeric(.rownames),
    deviation = CPI_CORE - .fitted
    ) |> 
  left_join(
    y = cp_reg_data |> 
      select(date) |> 
      rowid_to_column(var = ".rownames"),
    by = join_by(.rownames)
    )

head(cp_fit_plot)
cp_fit_plot |> colnames()
# .rownames CPI_CORE CPI_CORE CPI_CORE_lag CPI_EXP 
# YGAP_lag dlog_CI_USD .fitted .resid
# .hat .sigma .cooksd .std.resid deviation date


# Plot the data
cp_fit_plot |> 
  ggplot(mapping = aes(x = date)) +
  geom_line(mapping = aes(y = CPI_CORE, color = "Actual"), lwd = 1) +
  geom_line(mapping = aes(y = .fitted, color = "Model"), lwd = 1) +
  geom_col(mapping = aes(y = deviation, fill = "Deviation (Actual - Fitted)")) +
  theme_light() +
  theme(legend.position = "top") +
  scale_fill_manual(values = "darkgrey") +
  labs(
    title = "CPI Core: Actual vs. Fitted (%QoQ SA)",
    x = "", y = "%", color = "", fill = "",
    caption = "Data source: Brazilian Central Bank"
  )

ggsave(filename = "figures/10_PC-actual-fitted.png", height = 8, width = 12)
graphics.off()


# It would be interesting to see which of the explanatory variables
# hat what impact on inflation over time.

# We can multiply the value of each variable in a certain time period
# with the model coefficient we have obtained from our regression.

# The `broom::tidy()` function returns the model coefficients in
# a tidy format.
cp_fit |> 
  broom::tidy()
# term estimate std.error statistic p.value
# CPI_CORE_lag
# CPI_EXP
# YGAP_lag
# dlog_CI_USD

cp_fit |> 
  broom::tidy() |> 
  select(term, estimate)

# Chose the "date" variable and the coefficients
cp_fit_plot |> 
  select(date, names(cp_fit$coefficients))

# Pivot into long format, save the coefficients as "term"
cp_fit_plot |> 
  select(date, names(cp_fit$coefficients)) |> 
  pivot_longer(cols = -date, names_to = "term", values_to = "value")

# Add the "estimate" column and join by the "term" column
cp_fit_plot |> 
  select(date, names(cp_fit$coefficients)) |> 
  pivot_longer(cols = -date, names_to = "term", values_to = "value") |> 
  left_join(
    y = cp_fit |> 
      broom::tidy() |> 
      select(term, estimate), 
    by = join_by(term)
    ) |> 
  mutate(contribution = value * estimate)
# date term value estimate contribution

# Create a new data frame in long format, with the contribution
# of the residual
cp_fit_plot |> 
  select(date, contribution = .resid) |> 
  mutate(term = "residual")

# bind the data frame by row to the previous one

cp_decomp <- cp_fit_plot |> 
  select(date, names(cp_fit$coefficients)) |> 
  pivot_longer(cols = -date, names_to = "term", values_to = "value") |> 
  left_join(
    y = cp_fit |> 
      broom::tidy() |> 
      select(term, estimate),
    by = join_by(term)
  ) |> 
  mutate(contribution = value * estimate) |> 
  bind_rows(
    cp_fit_plot |> 
      select(date, contribution = .resid) |> 
      mutate(term = "residual")
  )

head(cp_decomp)
# date term value estimate contribution
cp_decomp$term |> unique()
# CPI_CORE_lag
# CPI_EXP
# YGAP_lag
# dlog_CI_USD
# residual

# Plot the contribution of each explanatory variable in the model
cp_decomp |> 
  ggplot(mapping = aes(x = date, y = contribution, fill = term)) +
  geom_col() +
  theme_light() +
  scale_fill_brewer(type = "qual", palette = 6) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "Contribution of each variable ot Core CPI (pp)",
    x = "", y = "", fill = "Variable",
    caption = "Data source: Brazilian Central Bank"
  )

ggsave(filename = "figures/10_PC-decomposition.png", height = 8, width = 12)
graphics.off()


# We can clearly see that inertia (CPI_CORE_lag) and
# expectations (CPI_EXP) are the main drivers of inflation
# throughout the sample, although for specific periods
# economic activity and imported inflation play a larger role
# than usual.

# The residual term is included in the plot because it is
# important to know when factors outside of those included
# in the model are relevant to the outcome.


# We can use this model to produce forecasts.

# We provide values for the exogenous variables
# including the lagged CPI.

# For now, we take the last value of each variable and
# add a small random variation.

set.seed(123)

new_values <- tibble(
  CPI_CORE_lag = last(cp_reg_data$CPI_CORE),
  CPI_EXP      = last(cp_reg_data$CPI_EXP) + rnorm(1),
  YGAP_lag     = last(cp_reg_data$YGAP) + rnorm(1),
  dlog_CI_USD  = last(cp_reg_data$dlog_CI_USD) + rnorm(1)
)

predict(object = cp_fit, newdata = new_values)
# 1.341488

# END