# 04 - Seasonal adjustment ----
# URL: https://book.rleripio.com/ds_seasonality
library(tidyverse)
library(sidrar)
library(seasonal)
library(forecast)
fig_path <- "figures/"
Sys.setlocale("LC_TIME", "English")
# Seasonality means expected (deterministic) fluctuations in time series 
# that occur within a regular frequency, usually not exceeding a one-year period.

# Fluctuations that are larger than one year are sometimes refereed to as 
# a *cycle* and combined  in the trend-cycle component of a time series.

# Seasonal fluctuations make it difficult to identify trends in time series.
# Before analyzing a time series, it is recommended to remove seasonal components.

# There are multiple methods available for seasonal adjustment,
# such as the U.S. Census Bureau's X-13-ARIMA program.

# It makes use of a standard ARIMA model with external regressors,
# accounting for outliers, permanent or transitory shifts, holiday effects, etc.

# The Census Bureau's Q&A section of its website on Seasonal Adjustment
# provides helpful information: https://www.census.gov/data/software/x13as/seasonal-adjustment-questions-answers.html

# Identify and remove the seasonal pattern from the Brazilian Retail Sales 
# data (PMC provided by IBGE, the Brazilian official bureau of statistics) using X-13-ARIMA.

### 4.0.1 Spotting a seasonal pattern ----
# It is common for time series to exhibit a seasonal pattern
# that is strong enough to be visible by simple visual inspection.
pmc_ts_nsa <- read_csv(file = "data/pmc_ts_nsa.csv")

pmc_ts_nsa |> 
  ggplot(mapping = aes(x = date, y = value)) +
  geom_line(lwd = 1.2) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "Retail sales in Brazil - Volume index (2022 = 100)",
    x = NULL, y = NULL
  ) +
  theme_light()

ggsave(filename = "04_br-retail-sales-raw.png", path = fig_path, height = 6, width = 10, bg = "white")
graphics.off()

# A mix of trends and random noise may hinder our ability to spot the seasonal pattern.

# Use`forecast::ggmonthplot()` function to build a plot
# where the data are grouped by period so we get a sense
# of which values are typical for which period.

# Note that the `forecast` package works with the "ts" class
# of time series objects.
pmc_ts_nsa <- ts(data = pmc_ts_nsa$value, start = c(2000, 1, 1), frequency = 12)

pmc_ts_nsa |> 
  ggmonthplot(lwd = 1.2) +
  labs(
    title = "Retail sales in Brazil - Volume Index (2022 = 100)",
    x = NULL, y = NULL
  ) +
  theme_light()

ggsave(filename = "04_br-retail-sales-monthplot.png", path = fig_path, height = 6, width = 10, bg = "white")
graphics.off()

# The graph conveys that retail sales are usually higher
# in December than in other months. This is related to year-end sales.

### 4.0.2 Removing the seasonal pattern ----
# We use the `seasonal` R package to access the X-13-ARIMA
# software through the `seas()` function.

# The function will automatically select the model that fits the data best.

# The `seas()` function returns the model selected for seasonal adjustment.
pmc_sa_autox13 <- seas(pmc_ts_nsa)

# We access the seasonally adjusted series with the `final()` function.
pmc_sa_autox13 |> 
  final() |> 
  autoplot(lwd = 1.2) +
  autolayer(object = pmc_ts_nsa, series = "Retail NSA", lwd = 1.2) +
  labs(
    title = "Retail sales in Brazil - Volume index (2022 = 100)",
    subtitle = "Seasonally-adjusted",
    x = NULL, y = NULL
  ) +
  theme_light() +
  theme(
    legend.position = "top",
    legend.title = element_blank()
  )

ggsave(filename = "04_br-retail-sales-adjusted.png", path = fig_path, height = 6, width = 10, bg = "white")
graphics.off()

# Use `forecast::ggmonthplot()` to see if the adjustment removed all seasonal variation:
pmc_sa_autox13 |> 
  final() |> 
  ggmonthplot() +
  labs(
    title = "Retail sales in Brazil - Volume index (2022 = 100)",
    subtitle = "Seasonally-adjusted",
    x = "",
    y = "Index (2022 = 100)"
  ) +
  theme_light()

ggsave(filename = "04_br-retail-sales-adjusted-monthplot.png", path = fig_path, height = 6, width = 10, bg = "white")
graphics.off()

# We can assess relevant information about the model selected
# by `seasonal::seas()` using linear model objects (`lm`).

# For example, information on the estimated parameters are
# available through the `summary()` function,
# while the `checkresiduals()` function from the `forecast`
# package can be used to check the properties of the residuals
# (or direclty perform any test based on model residuals) 
# using the `residuals()` function.
pmc_sa_autox13 |> 
  summary()
# We see significant effects for Saturday,
# Easter, and spring months.

# Check the residuals
pmc_sa_autox13 |> 
  forecast::checkresiduals()

ggsave(filename = "04_br-retail-sales-adjusted-residuals.png", path = fig_path, height = 6, width = 10, bg = "white")
graphics.off()

### 4.0.3 Moving to a custom specification ----
# Sometimes we don't want to rely on the automatic selection
# models, because we would like to include special
# moving holidays, such as Black Friday, or Super Bowl Sunday,
# or because we want to replicate the official seasonal adjustment
# process used by a third party.
pmc_ts_sa <- read_csv(file = "data/pmc_ts_sa.csv")

print(pmc_ts_sa)

# Plot the time series
pmc_ts_sa |> 
  rename(off_value = value) |> 
  bind_cols(tsbox::ts_df(final(pmc_sa_autox13)) |> rename(adj_value = value)) |> 
  select(date, off_value, adj_value) |> 
  pivot_longer(cols = -c("date"), names_to = "index", values_to = "value") |> 
  mutate(
    index = case_when(
      index == "off_value" ~ "unadjusted",
      index == "adj_value" ~ "adjusted"
    ),
    index = factor(index, levels = c("adjusted", "unadjusted"))
  ) |> 
  ggplot(mapping = aes(x = date, y = value, color = index)) +
  geom_line(lwd = 1.2) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(values = c("firebrick", "black")) +
  labs(
    title = "Retail sales in Brazil - Volume index (2022 = 100)",
    x = NULL, y = NULL
  ) +
  theme_light() +
  theme(
    legend.position = "top",
    legend.title = element_blank()
  )

ggsave(filename = "04_br-retail-sales-official-unofficial.png", path = fig_path, height = 6, width = 10, bg = "white")
graphics.off()

# We see that the automatically adjusted series follows the official one
# closely, until the COVID shock in the early 2020s.

# IBGE describes the model specification it uses for the
# seasonal adjustment of its series in a technical note.

# 1. - SARIMA(0,1,1)(0,1,1)

# 2. - Trading day adjustment
#    - Moving holidays: - Carnival
#                       - Corpus Christi
#                       - Easter

# 3. - Two level shifts:     - April 2020
#                            - December 2020
#    - One temporary change: - April 2020.

# The SARIMA model SARIMA(p=0,d=1,q=1)(P=0,D=1,Q=1)
# is the classic *Airline Model*.
# It can be specified inside `seasonal::seas()`.
# See more: http://www.seasonal.website/
# This model is usually used to de-seasonalize retail sales.

# The Easter regressor is also included inside X13.
# However, for the holidays that are specific to Brazil,
# we need to create dummy variables.

# Corpus Christi is celebrated in many catholic countries.
# Carnival, however, is an especially big celebration in Brazil.

# It always happens on a Tuesday, but the celebration 
# starts on Monday and ends on Wednesday.
# We want to include these two extra days in the input vector.

# The `seasonal::genhol()` function generates holiday regressors,
# hence the name.

# It can be used to extend a date vector by a number of 
# earlier and later dates, defined by the offsetting parameters
# `start` and `end`.

# Note that: 
# - Carnival occurs 47 days before Easter.
# - Corpus Christi occurs 60 days after Easter.

# The `seasonal` package has a built-in data set
# for Easter dates called "easter".

# Level shifts and temporary changes can be incorporated
# inside X13 with the `regression.variables` parameter.

# For the the level shifts in April & December 2020 we
# use "lsYEAR.MONTH".

# For the transitory change in April 2020 we use
# "tcYEAR.MONTH".

# See more in the Census Bureau's X-13-ARIMA-SEATS
# Rererence manual: https://www2.census.gov/software/x-13arima-seats/x-13-data/documentation/docx13as.pdf
# or on the seasonal.website

# We specify the seasonal adjustment for the monthly survey
# or Brazilian retail sales volume according to IBGE's specification:

# We use the built-in data set of Easter dates
print(seasonal::easter)

# The `lubridate` shortcuts to add dates `%m+%` and `%m-%` come in handy.

# Brazilian carnival is celebrated 47 days BEFORE Easter.
carnival <- seasonal::easter %m-% days(47)

print(carnival)

# Corpus Christi is celebrated 60 days AFTER Easter.
corpus_christi <- seasonal::easter %m+% days(60)

print(corpus_christi)

# Use `seasonal::genhol()` to generate holiday regressors
# by specifying the offset arguments `start` and `end`.

# Carnival celebrations start a day before and end a day after the actual holiday.
carnival_holiday <- genhol(
  x = carnival,
  start = -1,
  end = 1,
  frequency = 12,
  center = "calendar"
)

print(carnival_holiday)

# For Corpus Christi we specify only one holiday.
corpus_christi_holiday <- genhol(
  x = corpus_christi,
  frequency = 12,
  center = "calendar"
)

print(corpus_christi_holiday)

# Centering avoids bias in the resulting series.
# Use "calendar" for Easter and Chinese New Year "mean" for Ramadan.
# See references: Notes on centering holiday.

# Then we fit the model to the un-adjusted data "pmc_ts_nsa"
# as an object of class "ts" (time series).

# The model spcification is the following:

# Regression for pre-adjustment
# Trading day adjustment: "td"
# Level shift for April 2020: "ls2020.apr"
# Temporary change for April 2020: "tc2020.apr"
# Level shift for December 2020: "ls2020.dec"

# External regressors: 
# Carnival and Corpus Christi holiday dummy variables.

# The SARIMA(0,1,1)(1,1,1)[12] model follows
# the classical Airline Model (with log-adjustment).

# We use X-13 and not SEATS.
pmc_sa_customx13 <- seas(
  x = ts(data = pmc_ts_sa$value, start = c(2000, 1, 1), frequency = 12),
  regression.variables = c(
    "td",
    "easter[1]",
    "ls2020.apr",
    "tc2020.apr",
    "ls2020.dec"
  ),
  xreg = ts.union(carnival_holiday, corpus_christi_holiday),
  regression.usertype = "holiday",
  arima.model = "(0 1 1)(0 1 1)",
  regression.aictest = NULL,
  outlier = NULL,
  transform.function = "log",
  x11 = ""
)

# Note the use of `stats::ts.union()` 
head(ts.union(carnival_holiday, corpus_christi_holiday))

# Now we can plot the official seasonal adjustment together
# with our implementation of the same procedure.
pmc_sa_customx13 |> 
  final() |> 
  autoplot(lwd = 1.2) +
  autolayer(
    object = ts(data = pmc_ts_sa$value, start = c(2000, 1, 1), frequency = 12), 
    series = "Retail SA (official)",
    lwd = 1.2
    ) +
  labs(
    title = "Retail sales in Brazil - Volume index (2022 = 100)",
    subtitle = "Seasonally-adjusted",
    x = NULL, y = NULL
  ) +
  theme_light() +
  theme(
    legend.position = "top",
    legend.title = element_blank()
  )

ggsave(filename = "04_br-retail-sales-official-emulation.png", path = fig_path, height = 6, width = 10, bg = "white")
graphics.off()

# The new specification produces an almost perfect match with he official 
# seasonally-adjusted data, especially for the post-COVID period.

# Some deviations are arguably due to slight differences
# in the holiday vector, but for now we consider our goal achieved.

# END