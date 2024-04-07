# 03 - Rolling, cumulative and lagged/leading values ----
# URL: https://book.rleripio.com/ds_windowing
library(tidyverse)
library(glue)
library(RcppRoll)
library(httr)
library(jsonlite)
fig_path <- "figures/"
Sys.setlocale("LC_TIME", "English")
# Windowing operations are calculations over a sliding portion 
# of an array such as rolling means, products, or sums.

# Useful operations include accumulating values in a sequence,
# computing leading or lagged values, taking (first) differences,
# integrating time series, filtering data and taking moving averages.
# We call these *indexing operations*.

## 3.1 Rolling means ----
# Rolling operations are employed to smooth volatile time series, 
# which may help identify patterns such as trends and turning points, 
# and remove random noise or seasonal patterns.

# For the Google Mobility data for Brazil in the year 2021,
# we have observations on a daily frequency.

# Since mobility in workplaces is usually higher on weekdays,
# we face a predictable intra-weekly seasonal pattern.

# Taking a 7-days rolling mean of the data removes this intra-weekly seasonality.

# The `RcppRoll` package provides rolling functions for
# the minimum, maximum, median, standard deviations, products etc.

# Note that we can also use the suffixesl, c, and r for left, right, and center
# instead of the `align` parameter inside of the function call.
gmob_data_br <- read_csv(
  unz(
    description = "data/Region_Mobility_Report_CSVs.zip",
    filename = "2021_BR_Region_Mobility_Report.csv"
    )
  ) |> 
  filter(is.na(sub_region_1)) |> 
  select(date, mobility_workplaces = contains("workplaces"))

# We enamed a variable within `dplyr::select()`.

# Apply a 7-day right-aligned rolling mean
gmob_data_br_7dma <- gmob_data_br |> 
  arrange(date) |> 
  mutate(
    mobility_workplaces_7dma = roll_meanr(
      mobility_workplaces,
      n = 7,
      na.rm = TRUE
    )
  )

print(gmob_data_br_7dma)

# Plot the data
gmob_data_br_7dma |> 
  pivot_longer(cols = mobility_workplaces:mobility_workplaces_7dma, names_to = "index", values_to = "value") |>
  mutate(
    index = case_when(
      index == "mobility_workplaces" ~ "Raw",
      index == "mobility_workplaces_7dma" ~ "7-day moving-average"
    ),
    index = factor(index, levels = c("Raw", "7-day moving-average"))
  ) |> 
  ggplot(mapping = aes(x = date, y = value, color = index)) +
  geom_line(lwd = 1.2) +
  labs(
    title = "Brazil: Mobility in workplaces (% change from baseline)",
    caption = "Data source: Google",
    x = NULL, y = NULL
  ) +
  theme_light() +
  theme(
    legend.position = "top",
    legend.title = element_blank()
    )

ggsave(filename = "03_brasil-mobility-smoothed.png", path = fig_path, height = 6, width = 10)
graphics.off()

## 3.2 Accumulated in n-periods ----
# Taking a rolling mean to smooth out volatile time series
# to mitigate seasonal patterns is a natural choices when
# we care about the *level* of a series.

# However, some seasonality is better removed by taking  growth rates over a certain period.
# For CPI data, the percentage changes over a twelve-moth period are the norm.
# For quarterly data such as GDP, percentage changes over 
# the last four quarters are usually reported.

# Let's take the monthly US CPI data we used in the first chapter.
api_url       <- "https://api.stlouisfed.org/fred/series/observations?"
api_fred_key  <- Sys.getenv("FRED_API_KEY")
api_series_id <- "CPALTT01USM657N"
obs_start     <- "2010-01-01"
obs_end       <- today()
api_filetype  <- "json"
api_request   <- glue(
  "{api_url}series_id={
  api_series_id
  }&observation_start={
  obs_start
  }&observation_end={
  obs_end
  }&api_key={
  api_fred_key
  }&file_type={
  api_filetype
  }"
)

cpi_request <- GET(url = api_request)
cpi_content <- content(cpi_request, as = "text")
cpi_list    <- fromJSON(cpi_content, flatten = FALSE)
cpi_tbl     <- cpi_list[["observations"]] |> as_tibble()

# Change the types of the columns
cpi_tbl <- cpi_tbl |> 
  mutate(
    date  = as_date(date),
    value = as.numeric(value),
    .keep = "used"
  )

print(cpi_tbl)

# We have downloaded month-on-month growth rates of the CPI, and want to get 12-month growth rates instead.
# Calculate the percentage growth rate over a 12 month window with `RollCpp::roll_prodr()`.
# Use the right-aligned 12-month rolling product of 1 plus the monthly growth rates divided by 100,
# subtract 1 and multiply by 100
cpi_12m <- cpi_tbl |>
  arrange(date) |> 
  mutate(value_12m = (roll_prodr(1 + value/100, n = 12) - 1) * 100)

tail(cpi_12m)

# Plot the CPI accumulated over 12 months
cpi_12m |> 
  select(date, value_12m) |> 
  ggplot(mapping = aes(x = date, y = value_12m)) +
  geom_line(lwd = 1.2) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "US: CPI accumulated in 12-months (%)",
    x = NULL, y = NULL
  ) +
  scale_y_continuous(breaks = seq(from = 0.0, to = 10.0, by = 2.5), limits = c(0.0, 10.0)) +
  theme_light()

ggsave(filename = "03_us-cpi-12-months.png", path = fig_path, height = 6, width = 10)
graphics.off()

## 3.3 From changes to levels ----
# Sometimes we want to see the level of a time series instead of its rate of change.
# This is useful when you expect the data to follow a specific trend.
# Accumulating rates of change over time, or integrating
# a time series, will return the level of the series.

# Use the cumulative product to recover the level from the growth rates,
# and calculate the percentage change compared to the first available level
cpi_level <- cpi_tbl |> 
  arrange(date) |> 
  mutate(
    value_level = cumprod(1 + value / 100),
    value_level = (value_level / first(value_level) * 100)
  )

tail(cpi_level)

# Plot the US CPI in levels
cpi_level |> 
  select(date, value_level) |> 
  ggplot(mapping = aes(x = date, y = value_level)) +
  geom_line(lwd = 1.2) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "US: CPI in level (Jan 2010 = 100)",
    x = NULL, y = NULL
  ) +
  scale_y_continuous(limits = c(100, 140)) +
  theme_light()

ggsave(filename = "03_us-cpi-level.png", path = fig_path, height = 6, width = 10)
graphics.off()

# Looking at the level of prices makes it easier for the
# analyst to conjecture possible scenarios for inflation.

# We can either extrapolate the last value or assume the
# series will return to its pre-COVID path.

## 3.4 Lagged and leading values ----

# Lags and leads of time series are often used in regressions,
# (sometimes called auto-regressive models).

# However, they also appear in graphs that seek to compare
# two or more series that have a non-contemporary relationship.

# Knowing how to refer to past or future values of a time series
# is useful when computing changes from a baseline for example.

# The `dplyr::lead()` and `dplyr::lag()` functions make this task very easy.
# Note that there is also a `stats::lag()` function!
cpi_lag_lead <- cpi_tbl |> 
  mutate(
    value_lag01  = dplyr::lag(value, n = 1),
    value_lag03  = dplyr::lag(value, n = 3),
    value_lag06  = dplyr::lag(value, n = 6),
    value_lag12  = dplyr::lag(value, n = 12),
    value_lead_2 = dplyr::lead(value, n = 2)
  )

print(cpi_lag_lead)

# END