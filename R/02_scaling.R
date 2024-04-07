# 02 - Scaling-up tasks ----
# URL: https://book.rleripio.com/ds_googlemob
library(tidyverse)
library(glue)
library(RcppRoll)
library(patchwork)
fig_path <- "figures/"
Sys.setlocale("LC_TIME", "English")
# Google started releasing this data on a daily basis right 
# after the COVID outbreak spread across the world by mid-February 2020.
# Since then this data has been widely used for different purposes,
# from assessing/measuring economic activity to designing public policies.

## 2.1 Importing data ----
# Either get a unique .csv file with all available countries
# or get a .zip file with a separate .csv file for each country.
if (! "Region_Mobility_Report_CSVs.zip" %in% list.files(path = "data/")) {
  download.file(
    url = "https://www.gstatic.com/covid19/mobility/Region_Mobility_Report_CSVs.zip",
    destfile = "data/Region_Mobility_Report_CSVs.zip"
  )
}

# Only import the .csv files corresponding to these countries and then bind them together.

# After a brief inspection of the .zip file, we see a clear pattern in the file names:
# "year_country-code_Region_Mobility_Report.csv"

# The file containing data for Brazil in 2021 is: 2021_BR_Region_Mobility.csv

# We want to define a vector with the desired file names.
# We can call `Sys.Date()` to recover the current year
# and use it as the end point of our sequence of years.
country_codes <- c("BR", "US", "DE", "ZA", "SG", "AU")
years <- seq(from = 2020, to = year(Sys.Date()), by = 1)

# `cross2()` produces all combinations of list elements
# We might use `tidyr::expand_grid()` instead.
# Since it returns a list object, we can apply `map()`
google_filenames <- cross2(.x = years, .y = country_codes) |> 
  map_chr(.f = ~ .x |> 
            glue_collapse(sep = "_") |> 
            glue("_Region_Mobility_Report.csv")
  )

print(google_filenames)
# This has created all combinations of years and countries
# that we need to retrieve the data.

# It is possible that there are missing files for some countries in some years.

# `map()` would throw an error so use `possibly()` to replace the
# error with a `NULL` element in the output list.

# The efficient strategy is:
# 1) Use `map()` to import each file as an element in a list,
#    using `possibly()` to avoid throwing errors and breaking execution.
# 2) Assign names to each list element with `magrittr::set_names()`.
# 3) Call `plyr::ldply()` to stack the list elements.

# `plyr::ldply()` carries the names of the elements in the list
# into the resulting data frame as a new column.

# It also applies a generic function to each element of the list before stacking it.

# Because the file names follow a simple pattern, we can
# extract the relevant information from the first seven 
# characters of each element in the vector "google_filenames".
mobility_data <- map(
  .x = google_filenames, 
  .f = possibly(
    .f = ~ readr::read_csv(file = unz(description = "data/Region_Mobility_Report_CSVs.zip", filename = .x)), 
    otherwise = NULL
    )
  ) |> 
  magrittr::set_names(stringr::str_sub(google_filenames, start = 1, end = 7)) |> 
  plyr::ldply(.id = "year_country")

## 2.2 Preparing the data ----
mobility_data |> 
  glimpse()

dim(mobility_data) # 4.7 million rows and 16 columns.

# The columns are
mobility_data |> 
  colnames()

# Remove the "year_country" column since it won't be needed anymore.
mobility_data <- mobility_data |> 
  select(-year_country)

dim(mobility_data)

mobility_data |> 
  colnames()

# Columns 10 to 15 end in "*_percent_change_from_baseline".
# These are the mobility measures for categorized places we want.

# The "region" and "date" columns are also of interest.

# The "sub_region_*" column refers to regional breakdowns
# such as states and municipalities.
# They are NA for aggregate levels.

# Use `RcppRoll::roll_meanr()` to compute 7-days rolling means.

# The `RcppRoll` package is available on CRAN and GitHub:
# - CRAN: https://cran.r-project.org/package=RcppRoll
# - GitHub: https://github.com/kevinushey/RcppRoll

# Instead of using `mutate()` to successively change
# column variables, we use `across()` to apply
# the same function (`RcppRoll::roll_meanr()`) to a range
# of variables:
mutate_topsolution <- mobility_data |> 
  group_by(country_region) |> 
  arrange(date) |> 
  mutate(across(
    .cols = ends_with("baseline"), 
    .fns = ~ roll_meanr(.x, na.rm = TRUE), 
    .names = "{.col}_ma7d")) |> 
  ungroup()

print(mutate_topsolution)

# Use `rowwise()` to turn every row of the data frame into a single
# group that we can then perform a calculation on.

# Replace `across()` `c_across()`.

# Call `ungroup()` whenfinished.
mobility_final <- mobility_data |> 
  filter(is.na(sub_region_1)) |> 
  mutate(across(.cols = starts_with("residential"), .fns = ~ -1 * .x)) |> 
  group_by(country_region) |> 
  arrange(date) |> 
  mutate(across(
    .cols = ends_with("baseline"),
    .fns = ~ roll_meanr(.x, n = 7, na.rm = TRUE),
    .names = "{.col}_ma7d")
    ) |> 
  ungroup() |> 
  rowwise() |> 
  mutate(
    avg_mobility = mean(c_across(cols = ends_with("ma7d")), na.rm = TRUE)
  ) |> 
  ungroup() |> 
  select(date, country_region, ends_with("ma7d"), avg_mobility)

print(mobility_final)

## 2.3 Plot information ----
# We have mobility data for six countries and want to plot them.
# Use the `facet_wrap()` feature.
# Set the argument `scales = "free_y"` to have the y-axis
# of each plot automatically adjust so as to highlight salient features of the data.
# However, it makes comparisons between countries difficult.
mobility_final |> 
  ggplot(mapping = aes(x = date, y = avg_mobility)) +
  geom_line(lwd = 1.2) +
  facet_wrap(facets = ~ country_region) +
  labs(
    title = "Average mobility in selected countries - % change from baseline",
    subtitle = "7-days moving average",
    x = NULL, y = NULL,
    caption = "Data source: Google"
  ) +
  geom_hline(yintercept = 0, linetype = 2) +
  theme_light()

ggsave(filename = "02_google-mobility.png", path = fig_path, height = 6, width = 10, bg = "white")
graphics.off()

## 2.4 From code to function ----
# Wrap your code into a function, in case you might want to look at other countries.

# The three arguments we might want to change in the future are:
# - The country
# - The time span
# - The window size of the rolling mean

# We might think about how to apply our code to several
# countries in parallel instead of serially.

# We can write a function that plots a single country and then
# use `map()` to create plots for multiple countries.

# Additionally, we have to change `starts_with("ma7d")`
# to look for other window sizes, which we achieve with `glue()`.
plot_mobility <- function(
    country_code,
    start_date,
    end_date,
    ma_window
    ) {
  # Import data
  countries_codes <- country_code
  years <- seq(from = 2020, to = year(Sys.Date()), by = 1)
  google_filenames <- cross2(years, countries_codes) |> 
    map_chr(
      .f = ~ .x |> 
        glue_collapse(sep = "_") |> 
        glue("_Region_Mobility_Report.csv")
        )
  mobility_data <- map_dfr(
    .x = google_filenames,
    .f = possibly(
      ~ readr::read_csv(
        unz(
          "data/Region_Mobility_Report_CSVs.zip", 
          .x
          )
        ), 
      otherwise = NULL
      )
    )
  # Prepare data
  mobility_prep <- mobility_data |> 
    filter(is.na(sub_region_1)) |> 
    mutate(across(starts_with("residential"), ~ -1*.x)) |> 
    group_by(country_region) |> 
    arrange(date) |> 
    mutate(
      across(
        ends_with("baseline"), 
        ~ RcppRoll::roll_meanr(.x, ma_window, na.rm = TRUE), 
        .names = "{.col}_ma{ma_window}d"
        )
      ) |> 
    ungroup() |> 
    rowwise() |> 
    mutate(
      avg_mobility = mean(c_across(ends_with(glue("ma{ma_window}d"))), na.rm = TRUE)
    ) |> 
    ungroup() |> 
    select(date, country_region, ends_with("baseline"), avg_mobility)
  
  # Output plot
  mobility_prep |> 
    filter(
      between(date, left = ymd(start_date), right = ymd(end_date))
    ) |> 
    ggplot(mapping = aes(x = date, y = avg_mobility)) +
    geom_line(lwd = 1.2) +
    labs(
      x = NULL, y = NULL,
      title = glue("Average mobility in {country_codes} - % change from baseline"),
      subtitle = glue("{ma_window}-days moving average"),
      caption = "Data source: Google"
    ) +
    geom_hline(yintercept = 0, linetype = 2) +
    theme_light()
}

# Use`plot_mobility()` to plot any country with the desired time span and window for the rolling mean.
plot_mobility(country_code = "BE", start_date = "2020-03-01", end_date = "2022-10-15", ma_window = 14)
ggsave(filename = "02_brasil-mobility.png", path = fig_path, height = 6, width = 10, bg = "white")
graphics.off()

# Use `map()` to build the plot for several countries and 
# use the special operators from the `patchwork`
# package to arrange them in a convenient way.
countries <- c("BR", "FR")

mobility_countries <- map(
  .x = countries,
  .f = plot_mobility,
  start_date = "2020-03-01",
  end_date = "2022-10-15",
  ma_window = 14
  ) |> 
  magrittr::set_names(countries)

# Use the `patchwork` operator `/` to arrange plots vertically
mobility_countries[[1]] / mobility_countries[[2]]

ggsave(filename = "02_brasil-france-mobility.png", path = fig_path, height = 8, width = 10, bg = "white")
graphics.off()
# To parallelize our tasks, we would have to run `map(.x, .f)` 
# with the `furrr` package: https://furrr.futureverse.org/
# END