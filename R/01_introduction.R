# 01 - Introduction ---
# URL: https://book.rleripio.com/ds_tidyverse
library(tidyverse)
library(glue)
library(httr2)
library(jsonlite)
library(gt)
Sys.setlocale("LC_TIME", "English")
fig_path   <- "figures/"
table_path <- "tables/"
## 1.1 What is the tidyverse? ----
# The tidyverse packages help import, wrangle, program, and plot: https://www.tidyverse.org/

## 1.2 Importing -----
### 1.2.1 Reading from flat files ----
# Use `readr::read_csv()` to read comma-separated values (CSV) file from the web.
data_url <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv"

if (! "owid-covid-data.csv" %in% list.files(path = "data/")) {
  covid_data <- read_csv(data_url)
  write.csv(covid_data, file = "data/owid-covid-data.csv", row.names = FALSE)
}

# Read the file from your server
data_path  <- "data/owid-covid-data.csv"
covid_data <- read_csv(data_path)

# The tidyverse provides packages for reading other data types: 
# - `readxl`: Read Microsoft Excel spreadsheets: https://readxl.tidyverse.org/
# - `haven`: Read proprietary types like SPSS, Stata, and SAS: https://haven.tidyverse.org/index.html

### 1.2.2 Reading from API ----
# - An Application Programming Interface (API) is an interface to a database.
# - REST APIs use Representational State Transfer
# - An application that adheres to the REST architectural constraints is called RESTful.

# US Real Gross National Product: https://api.stlouisfed.org/fred/series/observations?series_id=GNPCA&api_key=abcdefghijklmnopqrstuvwxyz123456&file_type=json
# Federal Reserve Bank of St. Louis Economic Data (FRED) API Documentation: https://fred.stlouisfed.org/docs/api/fred/

# Every API request consists of:
# 1) Static URL: "https://api.stlouisfed.org/fred/series/observations?"
# 2) Series ID:  "id=GNPCA"
# 3) API key:    "key=abcdefghijklmnopqrstuvwxyz123456"
# 4) File type:  "file_type=json"

# - All parameters are separated by an ampersand ("&") sign.
# - The order of the parameters is NOT relevant.

# Monthly US Consumer Price Index (CPI) data: https://fred.stlouisfed.org/series/CPALTT01USM657N
# `series_id`: "CPALTT01USM657N".

# If we only want data between January 2010 and December 2022, we add the parameters 
# `observation_start` and `observation_end` in ISO Date format (YYYY-MM-DD).

# Use `glue::glue()` to merge the parameters into a request using the ampersand ("&") separators.

# Register for a personal key and store it as an environment variable called "api_fred_key" in the `.Renviron` file
# which you open with `usethis::edit_r_environ()`.

# Afterwards you can access the FRED api key with
api_url       <- "https://api.stlouisfed.org/fred/series/observations?"
api_fred_key  <- Sys.getenv("FRED_API_KEY")
api_series_id <- "CPALTT01USM657N"
obs_start     <- "2010-01-01"
obs_end       <- "2022-12-01"
api_filetype  <- "json"

# Use `glue()` instead of `glue()`
api_request <- glue(
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

api_request

# Request the data from the API with the `httr2` package:
cpi_request  <- request(base_url = api_request)
cpi_response <- req_perform(cpi_request)
cpi_content  <- resp_body_string(cpi_response)
cpi_list     <- fromJSON(txt = cpi_content, flatten = FALSE)
cpi_tbl      <- cpi_list[["observations"]] |> as_tibble()
print(cpi_tbl)

# - File formats: https://www.tidyverse.org/packages/#import
# -`rio` package: http://gesistsa.github.io/rio/

## 1.3 Wrangling ----
### 1.3.1 Data manipulation ----
# Use the COVID data set imported from Our World in Data (OWID).
covid_data |> 
  glimpse()

# New COVID cases (column new_cases) in Brazil (column location).
# The tidyverse assigns names (verbs) to functions
# according to the actions they perform - many SQL-inspired.

# - `dplyr::distinct()` drops all observations (rows) that are not unique,
# - `dplyr::select()` picks variables based on their names or positions.
# - `dplyr::filter()` retains the rows that satisfy a given condition, the analogue of the SQL WHERE.
covid_data_sub1 <- covid_data |> 
  distinct() |> 
  select(date, continent, location, new_cases) |> 
  filter(location == "Brazil")

print(covid_data_sub1)

# - Create a new column that shows twice the number of recorded new cases.
# - Create a column that records the dominant strain at each time period.
# - Delta took over by the end of July 2021
# - Omicron overtook Delta beginning 2022.

# - `dplyr::mutate()`: Create new variables from existing ones.
# - `dplyr::case_when()`: Create conditions inside the `dplyr::mutate()` statement.
# It returns `NA` if no condition is met.
# The workaround is to define an extra condition as `TRUE ~ value`
covid_data_sub2 <- covid_data_sub1 |> 
  mutate(
    real_new_cases = 2 * new_cases,
    dominant_stran = case_when(
      date <= "2021-07-31" ~ "Gamma",
      date > "2021-07-31" & date <= "2021-12-31" ~ "Delta",
      date > "2021-12-31" & date <= "2022-02-01" ~ "Omicron",
      TRUE ~ "We don't know"
    )
  )

print(covid_data_sub2)

# `dplyr::between()` is a shortcut for numeric conditions that are bounded both on the left and the right.
# It works with dates if we declare the arguments as date objects.

# Replace conditions 2 and 3 in order to have more compact and efficient code 
# (it is implemented in C++, like many modern R functions).
covid_data_sub2 <- covid_data_sub1 |> 
  mutate(
    real_new_cases = 2 * new_cases,
    dominant_strain = case_when(
      date <= "2021-07-31" ~ "Gamma",
      between(date, as.Date("2021-07-31"), as.Date("2021-12-31")) ~ "Delta",
      between(date, as.Date("2021-12-31"), as.Date("2022-02-01")) ~ "Omicron",
      TRUE ~ "We don't know"
    )
  )

print(covid_data_sub2)

# Find the European country that recorded the highest number of new COVID-19 
# cases in a single day by mid 2022.

# Don't forget to `dplyr::ungroup()` as soon as you don't need
# to perform grouped operations anymore.
covid_data_sub3 <- covid_data |> 
  distinct() |> 
  filter(continent == "Europe", date <= "2022-06-30", !is.na(new_cases)) |> 
  group_by(location) |> 
  summarise(max_new_cases = max(new_cases)) |> 
  ungroup() |> 
  arrange(desc(max_new_cases))

print(covid_data_sub3)
# Some countries such as Spain, Portugal and France
# will return NA if we do not include the condition
# `!is.na(new_cases)` inside of `dplyr::filter()`.

# If we use `base::max(new_cases, na.rm = TRUE)`,
# countries with no data in new_cases will return `-Inf`.
# Filtering first solves this problem.

# Find the date at which the highest number of new cases occurred (the peak date).
covid_data_sub5 <- covid_data |> 
  distinct() |> 
  filter(continent == "Europe", date <= "2022-06-30", !is.na(new_cases)) |> 
  group_by(location) |> 
  summarise(
    max_new_cases = max(new_cases),
    peak_date = date[which(new_cases == max_new_cases)] |> max()
  ) |> 
  ungroup() |> 
  arrange(desc(max_new_cases), peak_date) |> 
  slice(1:10)

print(covid_data_sub5)

# `dplyr::mutate()` and `dplyr::summarise()` allow you to use a
# variable that you have just created in a subsequent task inside the same calling.

# Merging multiple data sets is achieved by the 
# `*_join()` and `*_bind()` family of functions from the `dplyr` package.

# If we want to add the column Pop_Size from `europe_population` to `covid_data_sub5` 
# matching rows based on the location column.
europe_population <- covid_data |> 
  distinct() |> 
  filter(continent == "Europe") |> 
  select(location, population) |> 
  group_by(location) |> 
  summarise(Pop_size = mean(population)) |> 
  rename(Country = location) |> 
  arrange(desc(Pop_size))

print(europe_population)

# The `by` argument is needed since the key columns
# have different names in the two datasets.
covid_data_sub5_with_pop <- covid_data_sub5 |> 
  left_join(europe_population, by = c("location" = "Country"))

print(covid_data_sub5_with_pop)

# Add the information on the maximum daily number
# new cases and peak dates from `covid_data_sub5`
# to `european_population` with `right_join()`.
pop_with_covid_info <- covid_data_sub5 |> 
  right_join(europe_population, by = c("location" = "Country"))

print(pop_with_covid_info)

# Four mutating joins:
# - `left_join()`
# - `right_join()`
# - `full_join()`:  Keep all the data and produce `NA`s
# - `inner_join()`: Receive only keys that exist in both data sets

# Two filtering-joins:
# - `semi_join()`: Filter rows from one data set based on the PRESENCE of matches in the other set
# `anti_join()`:   Filter rows from one data set based on the ABSENCE of matches in the other set

# Suppose the COVID data set were released in single files, one for each country
covid_fr <- read_csv(file = "data/covid_fr.csv")
covid_uk <- read_csv(file = "data/covid_uk.csv")
# Use `dplyr::bind_rows()` to combine several data sets.

# Non-matching columns will be kept with their values
# filled with NA in case a column is absent.
covid_fr_uk <- bind_rows(covid_fr, covid_uk)
print(covid_fr_uk)
# `bind_cols()` combines data sets by matching row position rather than key variable.
# This means the two data sets need to have the same length.

### 1.3.2 Data layout ----
# `tidyr` package: https://tidyr.tidyverse.org/

# Data is tidy when:
# - Every column is a variable,
# - Every row is an observation
# - Each cell is a single value

# Tidy data is known as the wide format, as opposed to the long format.
# `tidyr::pivot_wider()` coinverts from long to wide format.

# - `names_from`: the column we want to widen,
# - `values_from`: the column containing the observations that will fill each cell.
# - `id_cols`: a parameter used to declare the set of  columns that uniquely identifies each observation.
covid_wide_01 <- covid_data |> 
  pivot_wider(id_cols = "date", names_from = "location", values_from = "new_cases")

print(covid_wide_01)

# If we want to have both `new_cases` and `new_deaths` for all countries, 
#  we provide a vector with the desired variables in `values_from`.

# New columns will be named according to the pattern `variable_location`.

# Because our variables are already separated by a single underscore, 
# we can demand that our new variables be separated by a double underscore.
covid_wide_02 <- covid_data |> 
  pivot_wider(
    id_cols = "date",
    names_from = "location",
    values_from = c("new_cases", "new_deaths"),
    names_sep = "__"
  )

print(covid_wide_02)

# The long format is preferable over the wide format when the data set contains more 
# than one grouping variable or we may want to work on more than one variable.

# Long format data sets are also ideal for plotting with the `ggplot2` package.

# Use `tidyr::pivot_longer()` to convert data from wide to long format
# and specify the cols argument to declare what columns you want to stack.
covid_long_01 <- covid_wide_02 |> 
  pivot_longer(
    cols = -"date",
    names_to = c("variable", "location"),
    values_to = "value",
    names_sep = "__"
  )

print(covid_long_01)
# We have specified that we want to leave out the date column with the minus sign (-).

# Suppose the location column included both the continent and the country names.
covid_loc_cont <- covid_data |> 
  distinct() |> 
  select(location, continent, date, new_cases, new_deaths) |> 
  mutate(location = paste0(location, "_", continent)) |> 
  select(-continent)

print(covid_loc_cont)

# Use `tidyr::separate()` function to solve this.
covid_separate <- covid_loc_cont |> 
  separate(
    col = "location",
    into = c("location", "continent"),
    sep = "_"
  )

print(covid_separate)

# This is more difficult if the separators are non-trivial.
covid_loc_cont <- covid_data |> 
  distinct() |> 
  select(location, continent, date, new_cases, new_deaths) |> 
  mutate(location = paste0(location, continent)) |> 
  select(-continent)

print(covid_loc_cont)
# A regular expression (regex) to match the appropriate pattern to split the string
# into location and continent would be ideal.

# Use the `tidyr::unite()` function to convert `covid_separate` back to `covid_loc_cont`.
covid_unite <- covid_separate |> 
  unite(col = "location", c("location", "continent"), sep = "_")

print(covid_unite)

# The `tidyr::nest()` function in conjunction with `dplyr::group_by()`
# is used to create a new data format in which every cell is 
# a `base::list()` object rather than a single observation.
covid_eu_nest <- covid_data |> 
  filter(continent == "Europe", !is.na(total_cases)) |> 
  group_by(location) |> 
  nest()

print(covid_eu_nest)
# Nested data is useful when you need to perform several tasks over whole data sets, 
# for example model fits for each country or plots for each country.
# The `purrr` package enables functional programming is indispensable in this context.

### 1.3.3 Text manipulation ----
# `stringr` package: https://stringr.tidyverse.org/index.html
# Underlying stringi package: https://stringi.gagolewski.com/

# Use `stringr::str_detect()` with dplyr::filter() since it returns TRUE if the specific pattern is found in the string and FALSE otherwise.

# Assuming we want to analyze the COVID data only for the North and South Americas.
covid_americas <- covid_data |> 
  filter(continent %in% c("North America", "South America"))

print(covid_americas)

# Let's pretend we had 50 continents on Earth with 25 having "America" in their names.
# `stringr::str_detect()` will help us here.
covid_americas <- covid_data |> 
  filter(str_detect(continent, "America"))

print(covid_americas)

# Provide multiple patterns to `stringr::str_detect()` by separating them with `|`.
covid_americas <- covid_data |> 
  filter(str_detect(continent, "South|North"))

print(covid_americas)

# Use `negate = TRUE` if we only want the strings that don't match our pattern.
covid_exAsia <- covid_data |> 
  filter(str_detect(continent, "Asia", negate = TRUE))

print(covid_exAsia)

# "covid_numCont" has observations in the `continent` column 
# starting with a random number from 0 to 9 due to a typing error.
covid_numCont <- read_csv(file = "data/covid_numCont.csv")
print(covid_numCont)
# Subset the strings in `continent` from position three onward with `strignr::str_sub()`.
# The `end` argument defaults to the last character so we don't need to make it explicit.
covid_numCont |> 
  mutate(continent = str_sub(continent, start = 3))

# What if the random numbers had ranged from 0 to 10?
covid_numCont2 <- read_csv(file = "data/covid_numCont2.csv")

# A regular expression inside `stringr::str_remove()` to get rid of 
# everything before and up to the dot (`.`), with `".*\\."`.
covid_numCont |> 
  mutate(continent = str_remove(continent, ".*\\."))

# The data set "covid_type" has two different types for North America.
# A missing "h" in rows 1 and 5 and an extra "h" in rows 3 and 8
covid_typo <- read_csv(file = "data/covid_typo.csv")
print(covid_typo)
# Use `stringr::str_replace()` to fix the whole column.
covid_typo |> 
  mutate(
    continent = str_replace(continent, "Nort America", "North America"),
    continent = str_replace(continent, "Northh America", "North America")
  )

# Pass a vector with all replacements to `stringr::str_replace_all()`.
covid_typo |> 
  mutate(
    continent = str_replace_all(
      continent,
      c(
        "Nort America"   = "North America",
        "Northh America" = "North America"
      )
    )
  )

# A common typo is whitespace.
# - `stringr::str_trim()` removes white spaces from the start/end of the string.
# - `stringr::str_squish()` removes white spaces from inside a string.
covid_ws <- read_csv(file = "data/covid_ws.csv")
print(covid_ws)

# Note that `tibble::tibble()` objects automatically add
# quotation marks around the strings to highlight the extra white spaces!
# Get rid of the extra whitespace.
covid_ws |> 
  mutate(
    continent = continent |> 
      str_trim() |> 
      str_squish()
  )

# - `stringr::str_extract()`: extract the name of the continents
# - `stringr::str_remove()`: remove them from the original column.
covid_loc_cont2 <- read_csv(file = "data/covid_loc_cont2.csv")
print(covid_loc_cont2)
# Multiple patterns can be separated by `|`.
continents <- c(
  "Asia", "Europe", "Africa",
  "South America", "North America", "Oceania"
  ) |> 
  paste0(collapse = "|")

print(continents)
# "Asia|Europe|Africa|South America|North America|Oceania"

# Extract the continent names and remove them
covid_loc_cont2 |> 
  mutate(
    continent = str_extract(location, continents),
    location  = str_remove(location, continents)
  )
# This is a creative solution!

### 1.3.4 Date manipulation ----
# `lubridate` package: https://lubridate.tidyverse.org/

# Get ISO 8601 date format YYYY-MM-DD with `lubridate::ymd()`,
# regardless of how the terms are separated (2022/12/01, 20221201, 2022-12-01)
# Full and abbreviated month names are allowed (2021-December-01, 2021-Dec-01)
# Related functions are mdy(), dmy(), ymd(), dym(), my(), ym() and so on.
ymd("2022/12/01")
mdy("december, 1, 2022")
dmy("01122022")
my("Dec-2021")

# If the string format does not match any of the predefined patterns, 
# use `lubridate::as_date()` to declare the unusual format, 
# specifying %Y for year, %m for month, and %d for day.

# %b represents month names (full or abbreviated)
# and %y represents two-digit years.

# Get historical Brazilian Real (BRL) vs US Dollar (USD) exchange rates:
brl_usd <- read_csv(file = "data/brl_usd.csv")
print(brl_usd)

# Convert the column date with `lubridate::dmy()`
brl_usd_aux <- brl_usd |> 
  mutate(date = ymd(date))

# Obtain BRL monthly averages.
brl_usd_monthly <- brl_usd_aux |> 
  mutate(
    year  = year(date),
    month = month(date)
  ) |> 
  group_by(year, month) |> 
  summarise(brl_monthly = mean(brl))

print(brl_usd_monthly)

# Now you need to use `lubridate::make_date()` to recover the date format "YYYY-MM-DD"
# from the `year` and `month` columns.

# Either set the `day` parameter inside `lubridate::make_date()`
# to 1 (default) or to the last day of the month
# with `lubridate::days_in_month()`:
brl_usd_monthly |> 
  mutate(
    date = make_date(year, month, day = 1),
    date2 = make_date(year, month, day = days_in_month(date))
  )

# Creating a column with the number of days in each month is itself often useful.
# If we have only the month totals of a variable and we want daily averages for example.

# When set to TRUE, the `with_year` parameter in `lubridate::quarter()`
# eliminates the need to create a separate column for the year.
brl_usd_quarterly <- brl_usd_aux |> 
  mutate(quarter = quarter(date, with_year = TRUE)) |> 
  group_by(quarter) |> 
  summarise(brl_quarterly = mean(brl))

print(brl_usd_quarterly)

# `lubridate` provides `week()` and `isoweek()`.
week("2022-01-01") # 1

# - `lubridate::week()`: Number of complete seven day periods between the date and January 1st.
# - `lubridate::isoweek()`: umber of the week (from Monday to Sunday) the date belongs to.
isoweek("2022-01-01")        # 52
weekdays(date("2022-01-01")) # "Saturday"
# Because this day is a Saturday, `lubridate::isoweek()` includes it in the last week of the previous year.

# For weekly averages, i.e. averages of seven day periods, use `lubridate::isoweek()`!

# Extract the week day from date objects.
brl_usd_aux |> 
  mutate(wday = wday(date, label = TRUE))

# `lubridate` imports the special operators `%m+%` and `%m-%` 
# that allow addition and subtraction of date objects.
d1 <- ymd("2020-02-29")
d1 %m+% years(2)  # "2022-02-28"
d1 %m-% months(3) # "2019-11-29"
d1 %m+% days(1)   # "2020-03-01"

# `lubridate::add_with_rollback()` allows more control over the output.
# Adding one month to 2022-01-31 could mean
# - 2022-02-28 (the last day of the next month)
# - 2022-03-01 (a period of one month).

# Set the `roll_to_first` parameter to TRUE for the latter.
d2 <- ymd("2022-01-31")
add_with_rollback(d2, months(1), roll_to_first = TRUE)  # "2022-03-01"

add_with_rollback(d2, months(1), roll_to_first = FALSE) # "2022-02-28"

# `floor_date()` and `ceiling_date()` round date objects down or up, respectively,
# to the nearest boundary of the specified time unit.
d3 <- ymd("2021-03-13")
floor_date(d3, unit = "month")     # "2021-03-01"
ceiling_date(d3, unit = "month")   # "2021-04-01"
floor_date(d3, unit = "quarter")   # "2021-01-01"
ceiling_date(d3, unit = "quarter") # "2021-04-01"

# Use them to match or re-write dates that refer to the same period but are written differently,
# (monthly date in set A is "2021-12-01" and in set B is "2021-12-31").

# They can also be used to perform temporal aggregation.
# Instead of creating two new grouping columns called "year" and "month", 
# simply round the dates down, preserving the "date" function.
brl_usd_monthly2 <- brl_usd_aux |> 
  mutate(date = floor_date(date, unit = "month")) |> 
  group_by(date) |> 
  summarise(brl_monthly = mean(brl))

print(brl_usd_monthly2)

# The `lubridate` family of functions `wday()`, `mday()`, `qday()` and `yday()`
# can be used to get the number of days that have occurred within a time period.
wday("2021-06-10") # 5th day of that week

mday("2021-06-10") # 10th day of that month (obviously)

qday("2021-06-10") # 71th day of the second quarter of 2021

yday("2021-06-10") # 161the day of 2021

# The results of `wday()` and `mday()` can be read straight from the date
# if it comes in YYYY-MM-DD format, but `qday()` and `yday()` are very helpful
# when you need to compare observations from the same period in different years
# or create **high frequency seasonal variables**.

## 1.4 Looping ----
# `purrr` package: https://purrr.tidyverse.org/

# Iteration is an indispensable tool in every programming language.
# Loops allow repeat an action over a set of values,
# thus preventing copying-and-pasting things over and over.
# For now, we stick to the `purrr::map_*()` family of functions.

# These always follow the same logic:
# Apply a function (existing or user-defined) 
# over a `base::vector()` or `base::list()` of arguments.

# We have three numeric vectors
v1 <- c(1, 4, 7, 8)
v2 <- c(3, 5, 9, 0)
v3 <- c(12, 0, 7, 1)
# and want to compute the `base::mean()` for each vector.

# We simply put the three vectors into a `base::list()` object
v_all <- list(v1, v2, v3)

# and use `purrr::map()`
map(.x = v_all, .f = mean)
# The return object is of type `base::list()`.

# Other types of return object are available in functions called
# `purrr::map_*()`;
# - `map_dbl()` returns a numeric vector,
# - `map_dfc()` returns a column data frame.

map_dbl(.x = v_all, .f = mean)
# 5.00 4.25 5.00

map_dfc(.x = v_all, .f = mean)
# This creates column names ...1, ...2, ...3.

# Usually, `base::list()` return objects are preferable,
# since we can easily use them for further operations.
# A data frame is the best choice for final results.

# Next, we provide a custom function.
# Additionally to the CPI, we also want GDP and the Unemployment Rate from the FRED API.

# Previously we created the object
print(api_series_id)
# that contained the ID of the CPI time series 

# We create a function whose only parameter is the FRED series ID.
# Then we create a vector or list of the desired IDs
# and use them inside the `purrr::map()` function.

# We call the function `get_series()` in classical API notation.
# We leave `api_series_id` as the parameter `series_id`.

# We keep some objects with their original names, like `cpi_`, to avoid confusion.
get_series      <- function(series_id) {
  api_url       <- "https://api.stlouisfed.org/fred/series/observations?"
  api_key       <- Sys.getenv("FRED_API_KEY")
  api_series_id <- series_id
  obs_start     <- "2010-01-01"
  api_filetype  <- "json"
  api_request   <- glue(
    "{api_url}series_id={
    api_series_id
    }&observation_start={
    obs_start
    }&api_key={
    api_key
    }&file_type={
    api_filetype
    }"
  )
  
  cpi_request  <- request(base_url = api_request)
  cpi_response <- req_perform(cpi_request)
  cpi_content  <- resp_body_string(cpi_response)
  cpi_list     <- fromJSON(txt = cpi_content, flatten = FALSE)
  cpi_tbl      <- cpi_list[["observations"]] |> as_tibble()
  
  return(cpi_tbl)
}

# We test the function with the CPI identifier used previously:
get_series(series_id = "CPALTT01USM657N")

# Create a vector or list with the IDs for CPI, GDP, and unemployment rate.
# Assigning a name to each element in a vector or list is always a good idea!
id_list <- list(
  "CPI" = "CPALTT01USM657N",
  "GDP" = "GDPC1",
  "Unemp" = "UNRATE"
)

print(id_list)

# Use `id_list` as the argument ".x" inside of `purrr::map()`
# and apply the custom function `get_series()` to each element in the list.
# Name the resulting `tibble()` object "fred_data".
fred_data <- purrr::map(.x = id_list, .f = get_series)

print(fred_data)

# Make the function more general by allowing more  parameters to vary.

# Use `purrr::map2(.x, .y, .f)` for different time spans for each series.
get_series2par <- function(series_id, series_start) {
  api_url       <- "https://api.stlouisfed.org/fred/series/observations?"
  api_key       <- Sys.getenv("FRED_API_KEY")
  api_series_id <- series_id
  obs_start     <- series_start
  api_filetype  <- "json"
  api_request   <- glue(
    "{api_url}series_id={
    api_series_id
    }&observation_start={
    obs_start
    }&api_key={
    api_key
    }&file_type={
    api_filetype
    }"
  )
  
  cpi_request  <- request(base_url = api_request)
  cpi_response <- req_perform(cpi_request)
  cpi_content  <- resp_body_string(cpi_response)
  cpi_list     <- fromJSON(txt = cpi_content, flatten = FALSE)
  cpi_tbl      <- cpi_list[["observations"]] |> as_tibble()
  
  return(cpi_tbl)
}

# Create the parameter list "time_list"
time_list <- list(
  "CPI"   = "2010-01-01",
  "GDP"   = "2012-04-01",
  "Unemp" = "2014-06-01"
)

print(time_list)

# Use `purrr::map2(.x, .y, .f)`
fred_data2 <- purrr::map2(.x = id_list, .y = time_list, .f = get_series2par)

print(fred_data2)

# `purrr::map2()` does not take the variable names of its inputs into account.
# The sequence of the inputs therefore matters!
# Use the names instead of the positions as indexes.

# `list[["element_name]]` returns an element of a list by its name, 
id_list[["CPI"]]
time_list[["GDP"]]

# Create a vector of variable names
vars_fred <- c("CPI", "GDP", "Unemp")

# Add a special function that returns list objects by name
fred_data2_names <- purrr::map(
  .x = vars_fred,
  .f = function(x) {
    get_series2par(
      series_id = id_list[[x]],
      series_start = time_list[[x]]
      )
    }
  ) |> 
  magrittr::set_names(vars_fred)

# Use `magrittr::set_names()` to add the names to the returned list object
print(fred_data2_names)

# Generalize this to as many parameters as you need without taking the risk 
# of using the parameter value of one series into another, 
# with the additional work being only to make explicit the parameters of the function in `.f`.

# Nesting a data frame is like converting every cell from a single observation into a `list` object.

# Print the nested data frame "covid_eu_nest" again
print(covid_eu_nest)

# Use `purrr::map(.x, .f)` to perform computations for every country at once.
# Create new columns to store the results,
# thus gathering all the information in the same object.
covid_eu_nest |> 
  unnest(cols = c(data))

covid_eu_nest |> 
  mutate(
    max_total_cases = map_dbl(
      .x = data,
      .f = function(x) {
        x |> 
          pull(total_cases) |> 
          max(na.rm = TRUE)
      }
    ),
    min_total_cases = map_dbl(
      .x = data,
      .f = function(x) {
        x |> 
          pull(total_cases) |> 
          min(na.rm = TRUE)
      }
    )
  )

# Each cell is a `list` object rather than an observation,
# we are by no means restricted to numeric elements.
# We could use the same strategy to create a column with plots.

## 1.5 Graphics ----
# The grammar of graphics used in the `ggplot2` package
# is broad and far from simple at first glance.
# See more: https://ggplot2.tidyverse.org/

# The book by Wickham, Navarro, and Pedersen (2019) 
# is a useful reference: https://ggplot2-book.org/

# The process of making a graphic as a set of layers 
# arranged sequentially is what constitutes the "grammar of graphics".

# The first layer is the data you want to plot.
# We use the CPI data imported from the FRED API.

# The second layer is the `ggplot2::ggplot()` function.

# These two layers set the stage for what's to come.
cpi_tbl |> 
  ggplot()

graphics.off()

# The third layer we must provide is a `geom_*()` function,
# a geometry or *geom* that represents the data.

# For time series data, a line graph is created with `geom_line()`.

# We need to specify the values of the `x` and `y` axes
# inside of the `mapping = aes(x, y)` argument.

# We use the `dplyr::glimpse()` function to see 
# which values for `x` and `y` are appropriate
cpi_tbl |> 
  glimpse()
# Clearly, "date" fits the `x` and "value" the `y` axis.

# Note that the `ggplot2` package uses the plus (`+`)
# operator instead of the pipe `|>` operator to chain functions together.

# We add a meaningful title and labels for both x and y axes.
# We set them using the `labs()` layer.

# We also define shorter intervals for dates (x-axis) and values (y-axis).

# We also want to highlight the upward trend of CPI as of 2021.
# Either the 12-month-accumulated CPI or the 3-months-accumulated
# seasonally-adjusted CPI would be common choices for this task,
# and we see examples on how to compute this later.

# For now we will use the average CPI from 2010 to 2019
# (pre-COVID period) as a measure of "normal" CPI.


# Since the parameter `x = date` appears in both 
# `geom_line()` layers, we can make the code more compact
# by moving this parameter to the `ggplot()` function.
# All parameters inside `ggplot()` are carried forward
# to every subsequent layer.
cpi_tbl |> 
  mutate(
    date = ymd(date),
    value = as.numeric(value),
    value_avg = mean(value[which(year(date) %in% 2010:2019)])
  ) |> 
  ggplot(mapping = aes(x = date)) +
  geom_line(mapping = aes(y = value), lwd = 1.2) +
  geom_line(mapping = aes(y = value_avg), color = "red", lwd = 1.2) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(from = -1, to = 1.5, by = 0.25), limits = c(-1, 1.5)) +
  labs(
    title = "US CPI showing an upward trend as of 2021",
    subtitle = "Red line is the 2010-2019 average",
    x = NULL, y = NULL,
    caption = "Source: FRED"
  ) +
  theme_bw()

ggsave(filename = "01_us-cpi-m-avg.png", path = fig_path, height = 6, width = 10, bg = "white")
graphics.off()

# - If we use `color` as an attribute to highlight different groups from the data, 
# then `color` goes inside `aes()`. In that case we pass a variable to `color`.
# - If we simply specify a color for the *geom*, `color` goes outside of `aes()`.

# Create a new binary variable called "covid_period that has the values 
# "Yes" between the dates "2020-03-01" and "2022-12-01" and "No" otherwise.
cpi_tbl |> 
  mutate(
    date = ymd(date),
    value = as.numeric(value),
    covid_period = if_else(
      condition = between(date, left = ymd("2020-03-01"), right = ymd("2022-12-01")), 
      true = "Yes", false = "No"),
    covid_period = factor(covid_period, levels = c("Yes", "No"))
  ) |> 
  ggplot(mapping = aes(x = date, y = value, color = covid_period, group = covid_period)) +
  geom_line(mapping = aes(group = 1), lwd = 1.2) +
  labs(
    title = "US CPI showing an upward trend as of 2021",
    subtitle = "COVID period is highlighted",
    x = NULL, y = NULL,
    caption = "Source: FRED"
  ) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(from = -1, to = 1.5, by = 0.25), limits = c(-1, 1.5)) +
  scale_color_manual(values = c("orange", "black")) +
  theme_bw() +
  theme(legend.position = "none")

ggsave(filename = "01_us-cpi-m-covperiod.png", path = fig_path, height = 6, width = 10, bg = "white")
graphics.off()

# Build a scatter plot and explore some additional features from `ggplot()`.
# Scatter plots are used to highlight the relationship between two variables at a given moment in time.
# We use the Covid data for the date "2021-07-31".

# The `ggplot` package also works with tools such as
# HEX codes and the ColorBrewer: https://colorbrewer2.org/

# ColorBrewer is a source for color schemes  designed to improve visual communication.

# We use `theme_light()` to make the plot cleaner.
# A theme is a set of pre-defined features for a graph, 
# and built-in themes include `theme_dark()`, `theme_minimal()` and `theme_bw()`.

# Note that this works for 2022, but not for 2021.
covid_data |> 
  filter(date == "2022-07-31") |> 
  ggplot() +
  geom_point(
    mapping = aes(
      x = people_fully_vaccinated_per_hundred,
      y = new_deaths_smoothed_per_million,
      color = continent
      ), 
    size = 3
    ) +
  labs(
    title = "New deaths in late July were above 5 per million people in \n countries where vaccination falls below 30% of population"
  ) +
  scale_color_brewer(type = "qual", palette = 2) +
  theme_light()

ggsave(filename = "01_deaths-vaccines.png", path = fig_path, height = 6, width = 10, bg = "white")
graphics.off()


# Instead of showing all continetns in a single plot we
# can separate them into multiple panels with
# `facet_wrap()`.

covid_data |> 
  filter(date == "2022-07-31", !is.na(continent)) |> 
  ggplot() +
  geom_point(
    mapping = aes(
      x = people_fully_vaccinated_per_hundred,
      y = new_deaths_smoothed_per_million
    ),
    size = 3
  ) +
  labs(
    title = "New deaths in late July were above 5 million people in \n countries where vaccination falls below 30% of population"
  ) +
  theme_light() +
  facet_wrap(facets = ~ continent)

ggsave(filename = "01_covid-deaths-vaccines-continents.png", path = fig_path, height = 4, width = 8)
graphics.off()
# Since we have a separate panel for each continent, 
# it is no longer necessary to assign a different color for each of them.

## 1.6 Tables ----
# Graphs are the right tool to visualize historical data
# (time series) or relationships between variables (dot pltos).
# We often need to send reports with detailed data from the
# latest release of a certain variable.
# `gt` package: https://gt.rstudio.com/

# The data frame "us_gdp" contains data on the contributions to
# percent change in real gross domestic product (annual rates)
# for the US economy released by the Bureau of Economic Analysis
# of the U.S. Department of Commerce (BEA): https://github.com/leripio/R4ER/tree/main/data

# Load the data with `readr::read_delim()`
us_gdp <- read_delim(file = "data/us_gdp.csv", delim = ";")
print(us_gdp)

us_gdp <- us_gdp |> 
  mutate(
    Quarter = as_date(Quarter, format = "%Y Q%q"),
    across(.cols = Goods:`Motor vehicle output`, .fns = ~ as.numeric(str_replace(string = .x, pattern = ",", replacement = ".")))
  )

print(us_gdp)

# The data is in wide format, and we pivot it into long format.
us_gdp_gt <- us_gdp |> 
  pivot_longer(cols = -Quarter, names_to = "var", values_to = "value") |> 
  filter(Quarter >= "2021-01-01") |> 
  mutate(Quarter = paste0(year(Quarter), " Q", quarter(Quarter))) |> 
  pivot_wider(
    names_from = Quarter,
    values_from = value
  ) |> 
  gt(rowname_col = "var")

print(us_gdp_gt)

# This is a rough draft table.
# We can add some labs such as title, subtitle, and source note:
us_gdp_gt <- us_gdp_gt |> 
  tab_header(
    title = "Contributions to percent change in real gross domestic product",
    subtitle = "Annual rates"
  ) |> 
  tab_source_note(
    source_note = "Source: U.S. Bureau of Economic Analysis (BEA)"
  )

print(us_gdp_gt)

# The `tab_spanner` can organize columns under a common label.

# We use one of its variations, `tab_spanner_delim`
# to organize the quarters under the respective year
# in order to make our table more compact.
us_gdp_gt <- us_gdp_gt |> 
  tab_spanner_delim(
    columns = -var,
    delim = " "
  )

print(us_gdp_gt)

# Save the table
gtsave(data = us_gdp_gt, filename = "01_us-gdp-contrib.png", path = table_path)

# END