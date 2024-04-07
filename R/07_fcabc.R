# 7 Forecasting - the abc ----
# URL: https://book.rleripio.com/fc_abc
library(tidyverse)
library(forecast)
library(rbcb)
library(rsample)
library(timetk)
library(ggtext)
fig_path <- "figures/"
Sys.setlocale("LC_TIME", "English")
# In recent years, forecasting has been widely associated
# with Machine Learning methods such as XGBoost,
# LSTM, Random Forests, and Neural Networks.

# These algorithms are effective in improving forecast
# accuracy in classification problems.

# However, for economic time series, a range of other
# methods exist that should at least serve as the 
# benchmark for more complex methods.

# In this chapter we cover time series forecasting
# with the `forecast` R package to predict the
# Brazilian CPI ex-regulated prices.

# The choice of the appropriate method is only part
# of the forecasting process.

# The `forecasting` R package is discussed at length
# in the book "Forecasting: Principles and Practice (2nd ed)"
# by Rob J. Hyndman and George Athanasopolus
# available at: https://otexts.com/fpp2/

# The third edition makes use of the new time series class
# `tsibble` and the `fable` R casting and is
# available here: https://otexts.com/fpp3/

# The third edition tries to combine `tidyverse` approaches
# with time series forecasting methods.


## 7.1 Step 1 - Observe the time series features ----
# Time series forecasting is about extrapolating patterns.

# Note that any forecasting method assumes either the underlying
# pattern in the data to continue in the future or 
# the association between two time series to hold in the future.

# For univariate time series forecasting, we need to investigate
# the features of the time series first.

# We import the monthly CPI ex-regulated prices (Not Seasonally Adjusted)
# from the Brazilian Central Bank API using the `rbcb` R package.

# The `rcbc` R package is available on:
# - CRAN: https://cran.r-project.org/package=rbcb
# - GitHub: https://github.com/wilsonfreitas/rbcb
# - Website: https://wilsonfreitas.github.io/rbcb/
# - Banco Central do Brasil: https://opendata.bcb.gov.br/ 

# Then we will plot the series with the `autoplot()` function
# from the `forecast` R package.
# Note that it is based on the `ggplot2::ggplot()` function.

# Use `rbcb::get_series()` to access the CPI series
cpi_br <- rbcb::get_series(
  code = list("cpi" = 11428),
  start_date = "2004-01-01",
  end_date = "2022-08-01",
  as = "ts"
)

autoplot(object = cpi_br) +
  labs(
    title = "Brazilian CPI ex regulated prices - %MoM NSA",
    x = "", y = "",
    caption = "Data source: Banco Central do Brasil"
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme_light()

ggsave(filename = "07_cpi-br.png", path = "figures/", height = 4, width = 8)
graphics.off()

# The figure above gives a general picture of the Month-on-Month
# growth rates non-regulated prices in Brasil between 2004 
# and mid 2022.

# We see that on average CPI grew about 0.5% month-on-month
# from 2004 until 2016, then dropped to about half that
# from mid-2016 to early 2020 before the Covid-19 pandemic
# hit the economy, when inflation started to increase to
# values close to around 1% month-on-month.

# The function `forecast::mstl()` will decompose a time
# series into trend, seasonal and remainder terms using
# loess, with multiple seasonal periods being allowed.
head(mstl(cpi_br))
# MM YYYY Data Trend Seasonal12 Remainder

mstl(cpi_br) |> 
  autoplot() +
  labs(
    title = "Brazilian CPI: time series decomposition",
    caption = "Data source: Banco Central do Brasil"
  ) +
  theme_light()

ggsave(filename = "07_cpi-br-decomp.png", path = "figures/", height = 4, width = 8)
graphics.off()

# We can extract two important pieces of information from this graph.

# The first is that the upward trend started in the aftermath
# of the Covid-19 pandemic seems to be flattening.

# The second is a noticeable change in the seasonal pattern
# as of 2016, with higher peaks and a different shape.

# This means that when selecting a forecasting method,
# we should opt for one with a flexible approach to
# trend and seasonality, to take the changing patterns into accoutn.

## 7.2 Step 2 - Split the sample ----
# We need to split the sample into training and testing sets
# to evaluate our forecasting model.

# There are several splitting schemes whose choice depends on
# the nature of the data and the sample size.

# For time-series data, the most robust scheme is called
# *block cross-validation*, where many contiguous sections of
# the time series are selected at random to train the model
# and the the performance on the adjacent observations.

# In practice, leave-one-out (LOO), where we fit the model
# to up to and including `t-1` observations to predict
# the `t`-th observation.

# This procedure can be iterated over up to `t` observations
# to provide a representative set of pseudo out-of-sample forecasts
# that allow us to assess the accuracy of our model.

# Note that the structural changes we saw in the previous plot
# have important implications for the choice of the split scheme.

# The new seasonal pattern after 2016 comprises about
# 40% of the sample, and the post-Covid upwards trend
# is present in about 15% of the data.

# We make the choice of taking the sample as of 2016
# and using the *leave-one-out* approach starting
# in January 2019 to predict the 12 months ahead.

# We use the `rsample` R package from the `tidymodels`
# ecosystem of R packages.
# See more: https://rsample.tidymodels.org/

# To learn more, consult this article on the `tidymodels`
# homepage: https://www.tidymodels.org/learn/models/time-series/

# The `rsample::rolling_origin()` function provides
# a convenient object where in each *slice* we have two
# components, the training set and the test set,
# both of which can be accessed by special accessor functions.

# The `initial` argument defines the size of the initial
# sample used to train the model.

# The `assess` argument defines the number of observations
# used to evaluate the performance in each step.

# The `cumulative = TRUE` option means we will not drop
# the previous training data as we incorporate new data
# (the training data will increase over time).
# We have an *expanding-window* sample.

# Exclude data before January 2016 to only incorporate the new CPI pattern.

# Coerce a time series `ts` class object to a `tibble` object
# with `timetk::tk_tbl()`

# Note that for the date class "yearmon" it is important
# to change your language settings to English!
cpi_df <- tk_tbl(cpi_br, rename_index = "date") |> 
  filter(date >= "Jan 2016")

print(cpi_df)
# Use `rsample::rolling_origin()` to create subsamples
# with an expanding window.

# The first sample that we fit model on will
# cover the monthly CPI observations from
# January 2016 to December 2018 (so three years).
# After that we will evaluate the model on the 
# next month (leave-one-out), `assess = 1`
# and add this month to the sample data for the next step.
cpi_split <- rolling_origin(
  data = cpi_df,
  initial = which(cpi_df$date == "Dec 2018"),
  assess = 1,
  cumulative = TRUE
)

tail(cpi_split)
# Rolling origin forecast resampling
# A tibble: 44 x 2
# splits          id
# <list>          <chr>
# <split [36/1]>  Slice01
# ...
# <split [79/1]>  Slice44

# As is typical for objects returned by functions form the 
# `tidymodels` R family of packages,
# we have a list-tibble, where the entries in the 
# column "splits" are other objects

# The first split contains 3 years of training data,
# or 36 months, and the last month contains 79 months
# of training data.
# In every split, just 1 month will be used to evaluate
# the model.
# All in all, we will fit our model 44 times on increasing
# training data sets and evaluate them each time on one
# observation.

# The object "cpi_split" has an "id" variable that is named
# "Slice01" to "Slice44".

# The first split contains the data from January 2016 through
# December 2018 and will evaluate on January 2019.

class(cpi_split)
# "rolling_origin" "rset" "tbl_df" "tbl" "data.frame"


## 7.3 Step 3 - Choose the model ----
# For now we restrict ourselves to uni-variate models, i.e.,
# we will not employ any explanatory variables.

# The `forecast` package contains a large set of uni-variate
# models. The ETS is generally a good choice 
# when there is no obvious candidate since it does not
# impose strong assumptions about the data (not even stationarity)
# and has proven to perform well on a variety of data sets at the
# **M Competition**
# See more at: https://en.wikipedia.org/wiki/Makridakis_Competitions

# The TBATS model is usually the first approach whenever
# we have to predict data with high frequencies (daily or higher)
# since it can handle multiple seasonal patterns.

# The ARIMA model is useful for stationary data,
# especially when the ACF/PACF plots show a well-defined
# auto-correlation structure.

# Note that in practice, statistical assumptions are often overlooked
# as the goal is to crate accurate predictions and not to make inference.

# As a matter of fact, producing accurate forecasts is inevitably
# a trial and error process.

# We saw that the Brazilian CPI time series exhibits a changing
# trend, which theoretically should favor a flexible model
# such as ETS, where the trend is allowed to change over time.

# On the other hand, for most of the periods this trend
# evolves at a constant pace, which makes ARIMA a good candidate
# model as well.

# Furthermore, we have downloaded the non-seasonally-adjusted
# CPI time series from the Brazilian Central Bank.

# As we have seen in our preliminary decomposition, 
# the data exhibits clear seasonal patterns.

# We need to successfully capture the seasonality in the data
# to create successful forecasts.

# One reason why forecast combinations often create superior
# results is that often one model effectively captures the trend
# in the data, while another successfully captures the
# seasonality.

# For all these reasons, we apply three models to our pseudo-
# leave-one-out forecasting exercise:
# - ETS
# - ARIMA
# - The average between the two

# We use `purrr::map_*()` functions to fit and forecast
# our models on the subsets inside "cpi_split".

# We use `dplyr::mutate()` to create four new variables
# - `ets_fc`
# - `arima_fc`
# - `avg_fc`
# - `date`

# The function `rsample::analysis()` converts objects
# of class `rsample` to a data frame 
rsample::analysis(cpi_split$splits[[1]])
# Then we use `timetk::tk_ts()` to coerce the data frame
# with the sub-sample into a `ts` object.
# This will discard the "date" column.
slice_01 <- timetk::tk_ts(
  data = rsample::analysis(cpi_split$splits[[1]]), 
  select = "value",
  start = c(2016, 1), 
  frequency = 12
  )

print(slice_01)
# These time series can then be used as input to
# `forecast::ets()` and `forecast::forecast()`
ets_01 <- forecast::ets(slice_01)
ets_01

ets_fc_01 <- forecast(ets_01, h = 1)
ets_fc_01

# We take the `mean` as our point forecast
ets_fc_01$mean

# The function `rsample::assessment()` converts an object
# type `rsplit` into a data frame
rsample::assessment(cpi_split$splits[[1]])$date |> 
  as.character() |> 
  zoo::as.yearmon()
# "Jan 2019"

cpi_fc <- cpi_split |> 
  mutate(
    
    ets_fc = map_dbl(
      .x = splits,
      .f = ~ (.x |> 
                analysis() |> 
                tk_ts(select = "value", start = c(2016, 1), frequency = 12) |> 
                ets() |> 
                forecast(h = 1)
              )$mean
    ),
    
    arima_fc = map_dbl(
      .x = splits,
      .f = ~ (.x |> 
                analysis() |> 
                tk_ts(select = "value", start = c(2016, 1), frequency = 12) |> 
                auto.arima() |> 
                forecast(h = 1)
              )$mean
    ),
    
    avg_fc = (arima_fc + ets_fc) / 2,
    
    date = map_chr(
      .x = splits,
      .f = ~ (.x |> 
                assessment()
              )$date |> 
        as.character()
    ) |> 
      zoo::as.yearmon()
  ) |> 
  select(date, contains("fc")) |> 
  right_join(y = cpi_df, by = "date")

# This will take a minute to run.

arrange(cpi_fc, date)
# A tibble: 80 x 5
# date        ets_fc  arima_fc  avg_fc  value
# <yearmon>   <dbl>   <dbl>     <dbl>   <dbl>
# Aug 2022    x.xxx   x.xxx     x.xxx   x.xx
# ...
# Jan 2016    NA      NA        NA      x.xx

# We have used right join so that we have the 
# full data set.

# When have the dates, the predictions of our three
# models, and the actual values for the last 34 months,
# - including periods pre- and during - and post Covid.

# Note that such tibbles have to be ordered
# according to the date variable, unlike the
# `tsibble` class!

# Before numerically calculating measures of forecast accuracy,
# we want to visually explore our forecasts.
cpi_fc |> 
  pivot_longer(cols = -date, names_to = "model", values_to = "forecast") |> 
  ggplot(mapping = aes(x = date, y = forecast, color = model)) +
  geom_line(lwd = 2) +
  theme_light() +
  scale_color_brewer(type = "qual", palette = 6) +
  theme(legend.position = "top") +
  labs(
    title = "Brazilian CPI Forecasts - %MoM NSA",
    y = "CPI (%MoM NSA)", color = "",
    caption = "Data source: Banco Central do Brasil"
  )

ggsave(filename = "07_cpi-br-fc.png", path = "figures/", height = 4, width = 8)
graphics.off()


## 7.4 Step 4 - Evaluate the model ----

# We use the mean absolute error (MAE) and the
# root mean square error (RMSE) as our measures
# of forecast accuracy.

# Note that both the MAE and the RMSE are 
# **symmetric functions** and use the **simple average**
# to summarize over the forecast errors created at each
# sub-sample.
# We do not take into account that as we evaluated our models
# on growing subsamples, the predictions should tend to get
# more accurate over time.

# Furthermore, the symmetry means we don't care if we over-
# or under-predict CPI.

# If you forecast electricity demand you may not want to
# be surprised by larger demand than predicted.

# We propose two accuracy metrics that are slight modifications
# of the well known mean absolute error (MAE).

# The first, `accuracy_1` assigns twice the weight to upside
# errors (predictions below actual values), whereas 
# the second, `accuracy_2`, assigns (linearly) decreasing
# weights to errors that are further in the past.
accuracy_1 <- function(e) {
  .abs_weighted_errors <- ifelse(test = e > 0, yes = 2 * e, no = abs(e))
  .mean_abs_weighted_errors <- mean(.abs_weighted_errors)
  return(.mean_abs_weighted_errors)
}

accuracy_2 <- function(e) {
  .abs_errors <- abs(e)
  .weights <- seq(from = 1, to = length(.abs_errors), by = 1)
  .weights <- .weights / sum(.weights)
  .mean_abs_weighted_errors <- weighted.mean(x = .abs_errors, .w = weights)
  return(.mean_abs_weighted_errors)
}

# We plot the `accuracy_1` function along with the original 
# MAE function to give you a sense of what is happening behind the scenes.

# For negative errors (realized value below the prediction)
# the weights are the same as in the original MAE,
# while it is somewhat higher for positive errors
# (realized value above the prediction).
acc_demo <- tibble(
  x = seq(from = -2, to = 2, by = 0.01)
  ) |> 
  mutate(
    t      = 1:n(),
    Loss_1 = ifelse(test = x > 0, yes = 2 * x, no = abs(x)),
    mae    = abs(x)
  )

head(acc_demo)
# x t Loss_1 mae

acc_demo |> 
  ggplot(mapping = aes(x = x)) +
  geom_line(mapping = aes(y = Loss_1), color = "darkblue", lwd = 2) +
  geom_line(mapping = aes(y = mae), color = "red", lwd = 1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_light() +
  theme(
    plot.title = element_markdown(lineheight = 1.1),
    axis.title = element_text(size = 13),
    legend.position = "none"
  ) +
  labs(
    title = "<span style='color:#002266;'><b>Custom loss function</b></span> vs <span style='color:#ff1a1a;'><b>MAE</b></span>",
    x = "Error", y = "Loss"
  )

ggsave(filename = "07_error-functions.png", path = "figures/", height = 4, width = 8)
graphics.off()

# We are ready to apply our custom functions plus the
# MAE to the errors we computed from the three models
# in order to decide which model we want to apply to the real data.
cpi_errors <- cpi_fc |> 
  filter(date >= "Jan 2019") |> 
  mutate(across(
    .cols = contains("fc"), 
    .fns = ~ value - .x, 
    .names = "error_{.col}"
    )) |> 
  summarise(across(
    .cols = contains("error"), 
    .fns = list(
      "acc1" = ~ accuracy_1(.x),
      "acc2" = ~ accuracy_2(.x),
      "mae"  = ~ mean(abs(.x))
    ), 
    .names = "{.col}-{.fn}")) |> 
  pivot_longer(cols = everything(), names_to = "model_metric", values_to = "value") |> 
  separate(col = "model_metric", into = c("model", "metric"), sep = "-") |> 
  pivot_wider(names_from = "metric", values_from = "value") |> 
  mutate(model = str_remove_all(string = model, pattern = "error_|_fc"))

print(cpi_errors)

arrange(cpi_errors, acc1)
# ARIMA has the lowest value for accuracy_1
arrange(cpi_errors, acc2)
# arima has also the lowest value for accuracy_2
arrange(cpi_errors, mae)
# arima has the lowest value for the MAE

# We have taken a simple average of the two model predictions
# for our third forecast implementation.

# We could also have assigned weights to the two models.

# Furthermore, we have compared our models by means of their
# point forecasts.

# This does not take into account the fact that each point
# forecast is a single realization of a random process
# and literature suggests the use of density forecasts
# and distributional accuracy measures.

# END