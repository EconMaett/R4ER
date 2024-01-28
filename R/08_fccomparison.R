# 08 - Forecasting by comparison ----

# Forecasting is the task of extrapolating historical patterns
# of a given process in order to infer a plausible range
# where future values should lie.

# Sometimes we do not have a historical pattern to rely on.

# In some situations, we do not have historical data
# to base our forecasts on.

# In this case we will have to rely on similar events
# elsewhere.


## 8.1 The early days of COVID-19 in Brazil ----

# At the beginning of March 2020, a novel corona virus 
# began rapidly spreading across continents.

# Brazil was a few weeks behind the rest of the world.

# China and South Korea had imposed tight restrictions on the
# movement of people, and it was unlikely that Brazil would folllow
# their example.

# Better comparisons were countries like Italy and Iran.

# We chose to model these two countries separately.

# We let r_{t} be the daily increase in total cases at
# period t and i is either Italy or Iran.

# \log\left(r_{it}\right) = \beta_{0} + \beta_{1} t_{i}

# The OLS estimation is calculated below


# Get the data for the regression
library(tidyverse)
library(ggtext)

# Note: The data set is on the book's GitHub repository
covid_data <- readRDS(file = "data/ch07_covid_data.rds")

covid_data_aux <- covid_data |> 
  as_tibble() |> 
  filter(type == "confirmed") |> 
  group_by(Country.Region) |> 
  mutate(
    acum_cases = cumsum(cases),
    r = ((acum_cases / lag(acum_cases)) - 1) * 100
  ) |> 
  filter(acum_cases >= 100) |> 
  mutate(t = row_number()) |> 
  ungroup()

head(covid_data_aux)


# Plot the data
library(gghighlight)
library(ggrepel)

# We need to remove some countreis
countries_out <- c("Qatar", "Pakistan", "Dominican_Republic")

covid_data_aux |> 
  filter(
    !(Country.Region %in% countries_out), 
    t <= 50, 
    date <= "2020-03-17"
    ) |> 
  ggplot(mapping = aes(x = t, y = r, color = Country.Region)) +
  geom_line(lwd = 1) +
  gghighlight(Country.Region %in% c("Brazil", "Italy", "Iran")) +
  theme_light() +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  theme(legend.position = "none") +
  labs(
    title = "Daily increase in total cases for available countries (%)",
    subtitle = "Data up to 2020-03-17",
    x = "Days after 100th confirmed case", y = "", color = "",
    caption = "Data: European Centre for Disease Prevention and Control."
  )

ggsave(filename = "figures/08_daily-increase-in-total-cases-vs-days-since-100th-case.png", height = 8, width = 12)

graphics.off()

# We see that the growth rate is an exponentially decaying function
# of time, so we assume that after taking the logarithm,
# we could well approximate this behavior with a linear model.

# `\log\left(r_{it}\right)` = \beta_{0} + \beta_{1} t_{i}

# We prepare the data for the regression
data_reg <- covid_data_aux |> 
  filter(Country.Region %in% c("Italy", "Iran")) |> 
  plyr::dlply(.variables = "Country.Region")

# Model equation
mod_eq <- "log(r) ~ t" |> as.formula()

# Fit the linear model
fit_reg <- map(.x = data_reg, .f = ~ lm(formula = mod_eq, data = .x))

# We use the `jtools` and `huxtable` R packages to create pretty tables
library(jtools)
library(huxtable)

# Plot the model results
export_summs(
  fit_reg$Italy,
  fit_reg$Iran,
  model.names = rev(names(fit_reg))
)

# Next we consider the total number of accumulated cases at
# time `t`, denoted `C_{t}`.

# The total number of cases in Brazil for time `t + 1` is given by
# C_{t+1} \equals C_{t} \times \left(1 + \frac{\hat{r_{it}}}{100}\right)
# where we approximate the growth rate for Brazil with that of
# Italy or Iran, using our linear model
# \hat{r_{it}} \equals e^{\hat{\beta_{0}} + \hat{\beta_{1}} t_{1}}

# If we want to compute the cumulative cases `t + k` time periods in the future,
# we need to calculate the cumulative product
# C_{t + k} \equals C_{t} \times \prod_{t}^{k}\left(1 + \left{\hat{r_{it}}}{100}\right)

# We want to start on March 16, the third day after accumulated
# cases in Brazil exceeded 100 cases.

# To recreate an *Italy-like* path on March 16 we should take the
# exponential of the fitted values form the Italy model.
1 + exp(fitted(fit_reg$Italy)) / 100

# We create a function that takes the Italy model as input
# as well as the already accumulated cases and the
# current time k
covid_fc <- function(model, C, t) {
  mod_fitted <- fitted(model)
  r_t <- 1 + exp(mod_fitted) / 100
  r_t_cum <- r_t[t:length(r_t)] |> cumprod()
  out <- round(C * r_t_cum, digits = 0)
  return(out)
}

covid_fc(model = fit_reg$Italy, C = 235, t = 4)


# Use `purrr::map()` to fit the models and get the forecasts
covid_br_fc <- purrr::map(
  .x = fit_reg,
  .f = ~ covid_fc(model = .x, C = 235, t = 4) |> 
    as.data.frame() |> 
    tibble::rownames_to_column() |> 
    magrittr::set_names(value = c("t", "forecast")) |> 
    mutate(t = as.numeric(t))
  ) |> 
  plyr::ldply(.id = "country") |> 
  pivot_wider(names_from = "country", values_from = "forecast")


print(covid_br_fc)


# We want to explore the results graphically
data_plot <- covid_data_aux |> 
  filter(Country.Region == "Brazil") |> 
  select(date, t, observed = acum_cases) |> 
  left_join(covid_br_fc) |> 
  select(-t)

data_plot |> 
  filter(date <= "2020-04-01" & date >= "2020-03-17") |> 
  pivot_longer(cols = -date, names_to = "var", values_to = "value") |> 
  ggplot(mapping = aes(x = date, y = value, color = var)) +
  geom_line(lwd = 1) +
  theme_light() +
  scale_x_date(date_breaks = "3 days", date_labels = "%b %d") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".")) +
  theme(
    legend.position = "top",
    axis.text = element_text(size = 13),
    legend.text = element_text(size = 13)
  ) +
  labs(
    title = "Covid total cases in Brazil - Observed vs. Forecasts",
    subtitle = "Estimated on March 16 2020",
    x = "", y = "Total Cases", color = ""
  )

ggsave(filename = "figures/08_covid-brazil-fcts-actual.png", height = 8, width = 12)
graphics.off()

# We see that Iran provided a good approximation for
# the development in Brazil in the early days but over time,
# the development in Brazil started to follow more closely
# that of Italy.


## 8.2 The second wave in Brazil ----

# This time we need an estimate for the peak of the second
# wave and how fast the subsequent decline will be - since
# these are the parameters that trigger policy decisions
# such as opening up retail businesses after lockdowns.

# The question is: How long does it take to reach the peak
# after the second wave has started?

# How long does it take to go down to the bottom after
# reaching the peak?

# The first challenge is that the second wave occurred
# in a non-synchronized way between countries.

# The number of days around the peak was not the same either.

# Looking at the pot below we see a large number of peaks occurring
# in the second period between November 2002 and March 2021.

# We assume this is the typical period for the second wave.
library(tidyverse)

data_url <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv"

covid_data <- read_csv(data_url)


covid_data |> 
  filter(date <= "2021-05-01") |> 
  filter(!(location %in% c("World", "Europe", "Asia", "North America", "South America"))) |> 
  ggplot(mapping = aes(x = date, y = new_cases_smoothed, color = location)) +
  geom_line(lwd = 1) +
  theme_light() +
  theme(legend.position = "none") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".")) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b/%y") +
  annotate(
    geom = "rect",
    xmin = as.Date("2020-11-01"),
    xmax = as.Date("2021-03-01"),
    ymin = -Inf, ymax = Inf,
    alpha = 0.2
    ) +
  labs(
    title = "New Covid cases (smoothed) by country",
    subtitle = "Shaded Area = assumed 2nd wave",
    x = "", y = "New Covid cases (smoothed)"
  )

ggsave(filename = "figures/08_covid-second-wave.png", height = 8, width = 12)
graphics.off()


# We want to zoom in on this time period and compute the
# typical behavior of new cases around the peak.

# First, we filter the November 2020 to March 2021 period,
# excluding Brazil from the data set.

# Then we create a variable `peak_date` as the earliest
# date where each country recorded the maximum number
# of new cases in the period.

# We create a variable `t_around_peak` to count the number
# of days before and after the peak date.

# Finally, we compute the median, first, and second quartiles
# from the distribution of new cases for every `t_around_peak`.

# We standardize the data to prevent countries with
# higher and lower numbers from over or under-weighting the statistic.

exclude_coundtries <- c("World", "Europe", "Asia", "North America", "South America", "Africa",
                        "Afghanistan", "England", "Hong Kong", "Macao", "Northern Cyprus", "Northern Ireland",
                        "Wales", "Scotland", "Taiwan")
covid_2nd_wave <- covid_data |> 
  filter(!(location %in% exclude_coundtries)) |> 
  select(date, continent, location, new_cases_smoothed) |> 
  filter(
    location != "Brazil",
    between(date, left = as.Date("2020-11-01"), right = as.Date("2021-04-01"))
  ) |> 
  group_by(location) |> 
  mutate(
    peak_date = min(date[which(new_cases_smoothed == max(new_cases_smoothed, na.rm = TRUE))]),
    t_around_peak = (date - peak_date) |> as.numeric(),
    new_cases_smoothed_std = scale(new_cases_smoothed)
    ) |>
  ungroup() |> 
  filter(!is.na(peak_date)) |> 
  group_by(t_around_peak) |> 
  summarise(
    median = median(new_cases_smoothed_std, na.rm = TRUE),
    lower  = quantile(new_cases_smoothed_std, probs = 0.25, na.rm = TRUE),
    upper  = quantile(new_cases_smoothed_std, probs = 0.75, na.rm = TRUE)
  ) |> 
  ungroup()

head(covid_2nd_wave)


# Plot the typical distributions around the peak
covid_2nd_wave |> 
  filter(!is.na(median)) |> 
  filter(between(t_around_peak, left = -80, right = 80)) |> 
  ggplot(mapping = aes(x = t_around_peak, y = median)) +
  geom_line(lwd = 1) +
  geom_ribbon(mapping = aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "grey70") +
  theme_light() +
  scale_x_continuous(breaks = seq(from = -80, to = 80, by = 20)) +
  labs(
    title = "2nd Covid wave - typical distribution around the peak",
    x = "# of days before (-) and after (+) the peak",
    y = "New covid cases (smoothed & scaled)"
  )

ggsave(filename = "figures/08_covid-second-wave-peak-distr.png", height = 8, width = 12)
graphics.off()

# On average, the number of new cases grew rapidly for about 20 days
# until it peaked and then took about the same number of days to reach
# the bottom.

# There was great uncertainty about the behavior of the pre-peak period
# as showed i the shaded area.

# END
