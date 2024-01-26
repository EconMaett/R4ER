# 06 - Hodrick-Prescott Filter ----

# Once our economic time series data has been seasonally-
# adjusted and converted from nominal into real values,
# we might be interested in decomposing the time series
# into an underlying trend and cycle component.

# For seasonally-adjusted real GDP, the trend component
# may represent *potential GDP* and the cycle component
# may be seen as the *output gap*.

# The trend should be a smooth series, with the
# cycle representing movements around (i.e. temporarily
# above or below) the trend.


# Historically, the Hodrick-Prescott filter, or HP-filter
# for short, was used for this task.

# It is convenient in that it requires only a single
# parameter called lambda, which controls the sensitivity
# of the trend-component to short-term fluctuations.

# Empirically, rules-of-thumb have proven effective:
# - lambda =  1600 for quarterly data
# - lambda = 14400 for monthly data
# - lambda =   100 for yearly data

# While the HP-filter can easily be built like this:
HP_Filter <- function(x, lambda = 1600) {
  
  eye <- diag(length(x))
  
  return(solve(eye + lambda * crossprod(diff(eye, lag = 1, differences = 2)), x))
}

# the `mFilter` R package implements an efficient version
# of the HP-filter with default values for lambda
# defined by the frequency of the `ts` time series object
# used as input for the `mFilter::hpfilter()` function.

# The `mFilter` package is available on 
# - CRAN: https://cran.r-project.org/package=mFilter
# - GitHub: https://github.com/mbalcilar/mFilter


# We import data on Brazilian GDP using the `sidrar`
# R package which is an interface to the IBGE's API.

# Again, this package is available on
# - CRAN: https://cran.r-project.org/package=sidrar
# - GitHub: https://github.com/rpradosiqueira/sidrar/
# - Vignette: https://cran.r-project.org/web/packages/sidrar/vignettes/Introduction_to_sidrar.html

# We choose to define the "date" variable with the
# `as.yearqrtr()` function from the `zoo` R package
# since it is compatible with `ts` objects - 
# the default input for the `mFilter::hpfilter()` function.
library(tidyverse)
library(sidrar)

# Accordign to the `sidrar` vignette, we go to the
# IBGE website: https://sidra.ibge.gov.br/home/pmc/brasil
# and click on the tab *CNT* - Contas Nacionais Trimestrais
# - the Quarterly National Accounts
# We open the table tab: https://sidra.ibge.gov.br/pesquisa/cnt/tabelas
# and chose table 
# 1620 - Série encadeada do índice de volume trimestral (Base: média 1995 = 100)
# the Chained series of the quarterly volume index (Base: 1995 average = 100)

# We can also call
# sidrar::info_sidra("1620", wb = TRUE)
# to see https://apisidra.ibge.gov.br/desctabapi.aspx?c=1620
# which provides a description of the variables.

# We can then either add the relevant parameters
# to the `sidrar::get_sidra()` function
# or click on the *link* symbol on the website
# https://sidra.ibge.gov.br/tabela/1621
# to get the API address directly.

# We need to select all quarters and receive the link:
# https://apisidra.ibge.gov.br/values/t/1620/n1/all/v/all/p/all/c11255/90707/d/v583%202
# We only use the part after "values":
# /t/1620/n1/all/v/all/p/all/c11255/90707/d/v583%202
# This is the same link as is used in the book.
# We try the link from the book first
gdp_br <- sidrar::get_sidra(
  api = "/t/1620/n1/all/v/all/p/all/c11255/90707/d/v583%202"
)

gdp_br |> 
  as_tibble() |> 
  colnames()
# We are interested in the variables
# "Trimestre (Código)" and "Valor".

gdp_br <- gdp_br |> 
  as_tibble() |> 
  select(quarter = `Trimestre (Código)`, gdp = Valor) |> 
  mutate(quarter = zoo::as.yearqtr(quarter, format = "%Y%q"))

print(gdp_br)
# A tibble: 111 x 2
# quarter     gdp
# <yearqtr>   <dbl>
# 1996 Q1     96.8

# We convert the `tibble::tibble()` object into
# a `stats::ts()` object.
gdp_br_ts <- ts(
  data  = gdp_br$gdp,
  start = first(gdp_br$quarter),
  frequency = 4
)

# We want to use the `forecast::autoplot()` function.
library(forecast)

gdp_br_ts |> 
  forecast::autoplot() +
  labs(
    title = "Brazilian Quarterly GDP (Index: 1995 = 100)",
    x = "", 
    y = "Brazilian Quarterly GDP (Index: 1995 = 100)",
    caption = "Data source: IBGE"
  ) +
  theme_light()

ggsave(filename = "06_br-gdp-quarterly.png", path = "figures/", height = 4, width = 8)
graphics.off()


# Note that we have chosen table "1620" which provides
# non-seasonally adjusted GDP instead of the seasonally-
# adjusted table "1621".

# We therefore need to use `seasonal::seas()` to remove 
# the seasonal component using the automatic selection model.
library(seasonal)
library(mFilter)

gdp_br_sa <- final(seas(gdp_br_ts))
gdp_br_hp <- hpfilter(gdp_br_sa)

hp_out <- tibble(
  "quarter" = gdp_br$quarter,
  "cycle" = gdp_br_hp$cycle |> c(),
  "trend" = gdp_br_hp$trend |> c(),
)

# The `c()` functions extract the raw vectors.

print(hp_out)
# A tibble: 111 x 3
# quarter   cycle trend
# <yearqtr> <dbl> <dbl>

# Plot the trend and cycle as reported by the HP-Filter
hp_out |> 
  pivot_longer(cols = -quarter, names_to = "var", values_to = "value") |> 
  ggplot(mapping = aes(x = quarter, y = value)) +
  geom_line(lwd = 2) +
  facet_wrap(facets = ~ var, scales = "free_y", ncol = 1) +
  labs(
    title = "HP-Filter decomposition of Brazilian GDP",
    x = "",
    y = "",
    caption = "Data source: IBGE"
  ) +
  theme_bw()

ggsave(filename = "06_br-gdp-hpfilter.png", path = "figures/", height = 5, width = 8)
graphics.off()

# The HP filter is both widely used and widely criticized.

# A common argument concerns the **end-point-bias*, whose most 
# common workaround is to add projections at the end of the 
# time series before applying the HP-filter.

# That is, we use back-casts and forecasts to artificially
# elongate the time series before we apply the HP-filter.

# This method is implemented inside the X13 software.
m <- seas(gdp_br_ts)

gdp_br_hp <- hpfilter(gdp_br_sa)

hp_out_x13 <- tibble(
  "quarter" = gdp_br$quarter,
  "cycle" = gdp_br_hp$cycle |> c(),
  "gdp_sa" = final(m) |> c(),
  "trend_sa" = trend(m) |> c(),
  "trend_hp" = gdp_br_hp$trend |> c()
)

print(hp_out_x13)

hp_out_x13 |> 
  pivot_longer(cols = -quarter, names_to = "var", values_to = "value") |> 
  filter(var %in% c("trend_hp", "trend_sa")) |> 
  ggplot(mapping = aes(x = quarter, y = value, color = var)) +
  geom_line(lwd = 2) +
  labs(
    title = "Trends of Brazilian GDP",
    x = "",
    y = "",
    color = "",
    caption = "Data source: IBGE"
  ) +
  theme_bw() +
  theme(legend.position = "top")

ggsave(filename = "06_br-gdp-hptrend-x13trend.png", path = "figures/", height = 5, width = 8)
graphics.off()

# We see that the trend returned by the HP-filter applied
# to the seasonally-adjusted series is much smoother
# than the trend returned by the X-13 software.


# Hamilton (2017) formalized several of the common criticisms
# of the HP filter and proposed a new filter that was supposed
# to overcome the HP-filter's shortcomings.

#   "A regression of the variable at date `t + h` on the four most recent
#    values as of date `t` offers a robust approach to de-trending that 
#    achieves all the objectives sought by users of the HP filter with
#    none of its drawbacks."

# You can check out the R package `neverhpfilter` by economist
# Justin M. Shea available online:
# - GitHub: https://github.com/JustinMShea/neverhpfilter/
# - Website: https://justinmshea.github.io/neverhpfilter/


# The Hamilton-filter works as follows.
# The fitted values and the residuals from the regression equation
# provide the trend and cycle components.

# Hamilton suggests `h = 8`, so `t + 8` for quarterly data.
# The regression equation is
# y_{t+h} = \alpha + \sum_{p=1}^{4}\beta_{p}y_{t+1-p}
# It is possible to use longer periods `h` and more lags `k`.

# We want to perform Hamilton's proposed filter by estimating the
# above equation and then arranging the corresponding output into
# a data frame as we did with the HP filter.

# The `augment()` function from the `broom` R package does a great job
# of converting the output of linear regression objects of class `lm`
# into data frames.

# # Apply the Hamilton filter to the seasonally-adjusted real GDP
gdp_br_hamilton <- tibble(
  quarter = gdp_br$quarter,
  gdp_sa  = gdp_br_sa |> c() 
  ) |> 
  mutate(
    y  = gdp_sa,
    y1 = dplyr::lag(gdp_sa, n = 8),
    y2 = dplyr::lag(gdp_sa, n = 9),
    y3 = dplyr::lag(gdp_sa, n = 10),
    y4 = dplyr::lag(gdp_sa, n = 11)
  )

print(gdp_br_hamilton)

# Fit the linear regression model proposed by Hamilton
hamilton_filter <- lm(formula = y ~ y1 + y2 + y3 + y4, data = gdp_br_hamilton)

summary(hamilton_filter)

# Use the `broom::augment()` function to convert the regression
# output into a data frame with columns
# # .rownames y y1 y2 y3 y4 .fitted .resid .hat .sigma .cooksd .std.resid
hamilton_out <- hamilton_filter |> 
  broom::augment() |> 
  mutate(
  ) |> 
  select(quarter, trend = .fitted, cycle = .resid)

print(hamilton_out)
# A tibble: 100 x 3
# quarter   trend cycle
# <yearqtr> <dbl> <dbl>

# Print the trend and the cycle component returned by the 
# Hamilton Filter
hamilton_out |> 
  pivot_longer(cols = -quarter, names_to = "var", values_to = "value") |> 
  ggplot(mapping = aes(x = quarter, y = value)) +
  geom_line(lwd = 2) +
  facet_wrap(facets = ~ var, scales = "free_y", ncol = 1) +
  labs(
    title = "Hamilton-filter decomposition of Brazilian GDP",
    subtitle = "h = 8",
    x = "", y = "",
    caption = "Data source: IBGE"
  ) +
  theme_bw()

ggsave(filename = "06_br-gdp-hamiltonfilter-8.png", path = "figures/", height = 5, width = 8)
graphics.off()

# We see a sharp drop and rise in the final part of the output
# series that is at odds with what we would expect from a trend
# component.

# We can solve this problem by setting `h = 12`.
# `h` should be a multiple of the time series frequency,
# which is 4 for quarterly data.

# The new plot is shown below.
gdp_br_hamilton2 <- tibble(
  quarter = gdp_br$quarter,
  gdp_sa  = gdp_br_sa |> c()
  ) |> 
  mutate(
    y  = gdp_sa,
    y1 = dplyr::lag(gdp_sa, n = 12),
    y2 = dplyr::lag(gdp_sa, n = 13),
    y3 = dplyr::lag(gdp_sa, n = 14),
    y4 = dplyr::lag(gdp_sa, n = 15)
  )

print(gdp_br_hamilton2)

# Fit the model proposed by Hamilton with `h = 12` instead of `h = 8`
hamilton_filter2 <- lm(formula = y ~ y1 + y2 + y3 + y4, data = gdp_br_hamilton2)

summary(hamilton_filter2)

hamilton_out2 <- hamilton_filter2 |> 
  broom::augment() |> 
  mutate(quarter = gdp_br_hamilton2$quarter[as.numeric(.rownames)]) |> 
  select(quarter, trend = .fitted, cycle = .resid)

print(hamilton_out2)

# Plot the Hamilton-filter with `h = 12`
hamilton_out2 |> 
  pivot_longer(cols = -quarter, names_to = "var", values_to = "value") |> 
  ggplot(mapping = aes(x = quarter, y = value)) +
  geom_line(lwd = 2) +
  facet_wrap(facets = ~ var, scales = "free_y", ncol = 1) +
  labs(
    title = "Hamilton-filter decomposition of Brazilian GDP",
    subtitle = "h = 12",
    x = "", y = "",
    caption = "Data source: IBGE"
  ) +
  theme_bw()

ggsave(filename = "06_br-gdp-hamiltonfilter-12.png", path = "figures/", height = 5, width = 8)
graphics.off()


# Combine the Hamilton filters with `h = 12`
# with the Hodrick-Prescott filter results
final_out <- hamilton_out2 |> 
  mutate(type = "Hamilton") |> 
  bind_rows(hp_out |> mutate(type = "HP"))

print(final_out)
# quarter trend cycle type gdp_sa trend_sa trend_hp


# Plot the Hamilton filter with `h = 12` and the Hodrick-Prescott filter
final_out |> 
  pivot_longer(cols = -c(quarter, type), names_to = "var", values_to = "value") |> 
  ggplot(mapping = aes(x = quarter, y = value, color = type)) +
  geom_line(lwd = 2) +
  facet_wrap(facets = ~ var, scales = "free_y", ncol = 1) +
  labs(
    title = "Trend-Cycle Decomposition of Brazilian GDP",
    x = "", y = "", color = "",
    caption = "Data source: IBGE"
  ) +
  theme_bw() +
  theme(legend.position = "top")

ggsave(filename = "06_br-gdp-hp-hamiltonfilter-12.png", path = "figures/", height = 5, width = 8)
graphics.off()


# Which one should we chose in this case?
# The HP-filter yields a smoother path for the trend component
# and the desired stationary behavior for the cycle component.

# Nevertheless, theory suggests that there is a link between
# the cycle component of the GDP and inflation.

# Thus, a common strategy is to evaluate what measure would have
# explained (core) inflation better in that period.

# Alternatively, other developments in the economy could favor
# the choice of either measure.

# It is up to the analyst to choose which one would best 
# represent his view of the evolution of the economy in that period.

# END
