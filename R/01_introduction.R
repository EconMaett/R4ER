# 01 - Introduction ---

## 1.1 What is the tidyverse? ----

# The tidyverse packages can be used to import,
# wrangle, program, and plot.

## 1.2 Importing -----

### 1.2.1 Reading from flat files ----

library(tidyverse)

data_url <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv"

covid_data <- read_csv(data_url)


### 1.2.2 Reading from API ----

# An Application Programming Interface (API) is
# an interface to a database.

# Create a request that specifies which data you want.

# Federal Reserve Bank of St. Louis Economic Data (FRED)
# API Documentation: 
# https://fred.stlouisfed.org/docs/api/fred/

# US Real Gross National Product: 
# https://api.stlouisfed.org/fred/series/observations?series_id=GNPCA&api_key=abcdefghijklmnopqrstuvwxyz123456&file_type=json

# The request can be broken down into the following parts:

# 1. The static URL to access the observations

# https://api.stlouisfed.org/fred/series/observations?

# 2. The series ID that uniquely identifies US Real gross National Product data

# id=GNPCA

# Our personal API key

# key=abcdefghijklmnopqrstuvwxyz123456

# 4. The file type of the output, in this case JSON

# file_type=json

# All parameters after the static URL are separated by
# an ampersand (&) sign.

# The order of the parameters is not relevant.


# To read monthly US Consumer Price Index (CPI) data,
# available at
# https://fred.stlouisfed.org/series/CPALTT01USM657N

# The series_id is CPALTT01USM657N

# If we only want data between January 2010 and December 2022,
# we add the parameters 
# observation_start and observation_end in ISO Date format
# (YYYY-MM-DD).

# We use the glue() function from the homonyms package
# to merge the parameters into a request using the ampersand (&)
# separators.


# Note that to access data from FRED, you need to register
# for a personal key that you store as an environment
# variable called api_fred_key in the .Renviron file.

# The simplest way to do this is to open the .Renviron file
# with 
usethis::edit_r_environ()

# Afterwards you can access the FRED api key with
Sys.getenv("FRED_API_KEY")

library(glue)

api_url       <- "https://api.stlouisfed.org/fred/series/observations?"
api_fred_key  <- Sys.getenv("FRED_API_KEY")
api_series_id <- "CPALTT01USM657N"
obs_start     <- "2010-01-01"
obs_end       <- "2022-12-01"
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

# Now we use the httr package to connect to the API,
# send the request and get the content.

# Then we transform the JSON file to a standard R object
# (a list) and convert it into a tibble.

# Notice that the CPI data is stored in the list
# element called observations.

# This is specific to this API.

library(httr)
library(jsonlite)
library(tidyverse)
cpi_request <- GET(url = api_request)
cpi_content <- content(cpi_request, as = "text")
cpi_list    <- fromJSON(cpi_content, flatten = FALSE)
cpi_tbl     <- cpi_list[["observations"]] |> as_tibble()

cpi_tbl


# The official tidyverse website provides a comprehensive list
# of all supported file formats and the respective
# packages to handle them
# https://www.tidyverse.org/packages/#import

# The rio package also enables import from multiple 
# file types
# http://gesistsa.github.io/rio/


## 1.3 Wrangling ----

# We use the functions from the dplyr package to
# get the data ready to use.

### 1.3.1 Data manipulation ----

# We use the COVID data set imported from Our World in Data (OWID).

# The glimpse() function lets us have a grasp of the data.
covid_data |> 
  glimpse()

# Let's say we are only interested in the new COVID cases
# (column new_cases) in Brazil (column location).

# The tidyverse assigned names (verbs) to functions
# according to the actions they perform - many are
# admittedly SQL-inspired.

# distinct() drops all observations (rows) that are not unique,
# whereas select() picks variables based on their names or positions.

# The filter() function retains the rows that satisfy
# a given condition, the analogue of the SQL WHERE.

# Such logical conditions either return TRUE or FALSE.

covid_data_sub1 <- covid_data |> 
  distinct() |> 
  select(date, continent, location, new_cases) |> 
  filter(location == "Brazil")

# We may guess that cases are under reported and 
# want to create a new column that shows twice the
# number of recorded new cases.

# We also create a column that records the dominant
# strain at each time period.

# We know that the Delta strain took over by the end of July 2021
# and that Omicron overtook Delta beginning 2022.


# The mutate() verb is used to create new variables from existing ones.

# case_when() is an SQL-inspired function that creates conditions
# inside the mutate() statement.

# It returns NA if no condition is met.

# The workaround is to define an extra condition as
# TRUE ~ value

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


# The between() function is a shortcut for numeric conditions
# that are bounded both on the left and the right.

# It also works with dates if we declare the argumetns as
# date objects.

# We can replace conditions 2 and 3 in order to have more compact
# and efficient code (it is implemented in C++, like many
# modern R functions).

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


# We might be interested in new cases in all European countries.

# We use the group_by() function in conjunction with
# mutate() or summarise().


# Suppose we want to know which European country recorded
# the highest number of new covid cases in a single day
# by mid 2022.

# Don't forget to ungroup() as soon as you don't need
# to perform grouped operations anymore.

covid_data_sub3 <- covid_data |> 
  distinct() |> 
  filter(
    continent == "Europe",
    date <= "2022-06-30",
    !is.na(new_cases)
  ) |> 
  group_by(location) |> 
  summarise(max_new_cases = max(new_cases)) |> 
  ungroup() |> 
  arrange(desc(max_new_cases))

covid_data_sub3

# Some countries such as Spain, Portugal and France
# will return NA if we do not include the condition
# !is.na(new_cases) inside of filter().

# If we use max(new_cases, na.rm = TRUE),
# countries with no data in new_cases will return 
# -Inf.

# Filtering first solves this problem.


# We want to find the date at which the highest number
# of new cases occurred (the peak date).

covid_data_sub5 <- covid_data |> 
  distinct() |> 
  filter(
    continent == "Europe",
    date <= "2022-06-30",
    !is.na(new_cases)
  ) |> 
  group_by(location) |> 
  summarise(
    max_new_cases = max(new_cases),
    peak_date = date[which(new_cases == max_new_cases)] |> max()
  ) |> 
  ungroup() |> 
  arrange(desc(max_new_cases), peak_date) |> 
  slice(1:10)

covid_data_sub5


# mutate() and summarise() allow you to use a
# variable that you have just created in a subsequent
# task inside the same calling.


# Merging multiple data sets is achieved by the 
# *_join() and *_bind() family of functions from the dplyr package.


# If we want to add the column Pop_Size from 
# europe_population to covid_data_sub5 matching
# rows based on the location column.
europe_population <- covid_data |> 
  distinct() |> 
  filter(continent == "Europe") |> 
  select(location, population) |> 
  group_by(location) |> 
  summarise(Pop_size = mean(population)) |> 
  rename(Country = location) |> 
  arrange(desc(Pop_size))

europe_population

# the by argument is needed since the key columns
# have different names in the two datasets.
covid_data_sub5_with_pop <- covid_data_sub5 |> 
  left_join(europe_population, by = c("location" = "Country"))

covid_data_sub5_with_pop


# We can add the information on the maximum daily number
# new cases and peak dates from covid_data_sub5
# to european_population with right_join().

pop_with_covid_info <- covid_data_sub5 |> 
  right_join(
    europe_population,
    by = c("location" = "Country")
  )

pop_with_covid_info

# Use full_join() to keep all the data and produce NAs
# and inner_join() to receive only keys that exist in both
# data sets.


# left_join(), right_join(), full_join() and inner_join()
# are the four mutating-joins.

# filtering-joins filter the rows from one data set
# based on the presence (semi_join()) or absence 
# (anti_join()) of matches in the other data set.


# Suppose the Covid data set were released in single
# files, one for each country

covid_fr <- covid_data |> 
  distinct() |> 
  select(date, location, new_cases, total_cases) |> 
  filter(location == "France") |> 
  arrange(date)

covid_fr

covid_uk <- covid_data |> 
  distinct() |> 
  select(date, location, new_cases) |> 
  filter(location == "United Kingdom") |> 
  arrange(date)

covid_uk


# We use bind_rows() to combine several data sets.

# Non-matching columns will be kept with their values
# filled with NA in case a column is absent.

covid_fr_uk <- bind_rows(covid_fr, covid_uk)

covid_fr_uk


# The bind_cols() function will combine two data sets
# by matching row position rather than key variable.

# This means the two data sets need to have the same length.


### 1.3.2 Data layout ----

# The tidyr package helps us create tidy data.

# Data is tidy when every column is a variable and
# every row is an observation
# and each cell is a single value.


# Tidy data is known as the wide format, as opposed
# to the long format.

# Ask yourself the following question:
# Is all the information contained in a single cell?
# If yes, the data is in wide or tidy format.
# If no, it is in long format.


# Use pivot_wider() to convert from long to wide format.

# names_from is the column we want to widen,
# values_from is the column containing the observations
# that will fill each cell.
# id_cols is a parameter used to declare the set of 
# columns that uniquely identifies each observation.
# All other columns will be dropped.

covid_wide_01 <- covid_data |> 
  pivot_wider(
    id_cols = "date",
    names_from = "location",
    values_from = "new_cases"
  )

covid_wide_01

# If we want to have both new_cases and new_deaths
# for all countries, we provide a vector with the
# desired variables in values_from.

# New columns will be named according to the pattern
# variable_location.


# Because our variables are already separated
# by a single underscore, we can demand that our
# new variables be separated by a double underscore.

covid_wide_02 <- covid_data |> 
  pivot_wider(
    id_cols = "date",
    names_from = "location",
    values_from = c("new_cases", "new_deaths"),
    names_sep = "__"
  )

covid_wide_02


# The long format is preferable over the wide format
# when the data set contains more than one grouping
# variable or we may want to work on more than one variable.

# Long format data sets are also ideal for plotting with
# the ggplot2() package.


# Use pivot_longer() to convert data from wide to long format
# and specify the cols argument to declare what
# columns you want to stack.

covid_long_01 <- covid_wide_02 |> 
  pivot_longer(
    cols = -"date",
    names_to = c("variable", "location"),
    values_to = "value",
    names_sep = "__"
  )

covid_long_01

# We have specified that we want to leave out the
# date column with the minus sign (-).


# Suppose the location column included both the
# continent and the country names.
covid_loc_cont <- covid_data |> 
  distinct() |> 
  select(location, continent, date, new_cases, new_deaths) |> 
  mutate(location = paste0(location, "_", continent)) |> 
  select(-continent)

covid_loc_cont

# We use the separate() function to solve this.
covid_separate <- covid_loc_cont |> 
  separate(
    col = "location",
    into = c("location", "continent"),
    sep = "_"
  )

covid_separate


# This is more difficult if the separators are non-trivial.
covid_loc_cont <- covid_data |> 
  distinct() |> 
  select(location, continent, date, new_cases, new_deaths) |> 
  mutate(location = paste0(location, continent)) |> 
  select(-continent)

covid_loc_cont

# Ideally we would provide e regular expression (regex)
# to match the appropriate pattern to split the string
# into location and continent.


# Use the unite() function to convert covid_separate
# back to covid_loc_cont.
covid_unite <- covid_separate |> 
  unite(
    col = "location",
    c("location", "continent"),
    sep = "_"
  )

covid_unite


# The nest() function in conjunction with group_by()
# is used to create a new data format in which
# every cell is a list() object rather than a single
# observation.

covid_eu_nest <- covid_data |> 
  filter(continent == "Europe") |> 
  group_by(location) |> 
  nest()

covid_eu_nest

# Nested data is useful when you need to perform
# several tasks over whole data sets, for example
# model fits for each country or plots for each country.

# The purrr package which enables functional programming
# is indispensable in this context.


### 1.3.3 Text manipulation ----

# The stringr package is used for text manipulation
# in the tidyverse.

# It is based on the underlying stringi package.
# https://stringi.gagolewski.com/


# We use str_detect in conjunction with dplyr::filter()
# since it returns TRUE if the specific pattern is
# found in the string and FALSE otherwise.


# Assuming we want to analyze the Covid data only for
# the North and South Americas.
covid_americas <- covid_data |> 
  filter(
    continent %in% c("North America", "South America")
  )


# Let's pretend we had 50 continents on Earth
# with 25 having "America" in their names.
# str_detect() will help us here.
covid_americas <- covid_data |> 
  filter(
    stringr::str_detect(continent, "America")
  )


# We can provide multiple patterns to str_detect()
# by separating them with |.
covid_americas <- covid_data |> 
  filter(
    stringr::str_detect(continent, "South|North")
  )


# We use negate = TRUE if we only want the strings
# that don't match our pattern.
covid_exAsia <- covid_data |> 
  filter(
    str_detect(continent, "Asia", negate = TRUE)
  )


# Suppose we have a data set covid_numCont
# in which the observations in the continent
# column starts with a random number from 0 to 9
# due to a typing error.
covid_numCont <- covid_data |> 
  distinct() |> 
  select(continent, location, date, new_cases) |> 
  mutate(
    continent = paste0(floor(runif(n = nrow(covid_data), min = 0, max = 9)), ".", continent)
  )

covid_numCont


# We want to subset the strings in continent from position
# three onwards using str_sub().

# The end argument defaults to the last character so
# we don't need to make it explicit.

covid_numCont |> 
  mutate(
    continent = str_sub(continent, start = 3)
  )


# But what if the random numbers had ranged from
# 0 to 10?
covid_numCont <- covid_data |> 
  distinct() |> 
  select(continent, location, date, new_cases) |> 
  mutate(
    continent = paste0(ceiling(runif(n = nrow(covid_data), min = 1, max = 10)), ".", continent)
  )

covid_numCont

# We use a regular expression inside str_remove()
# to get rid of everything before and up to the
# dot (.), with ".*\\.".
covid_numCont |> 
  mutate(
    continent = str_remove(continent, ".*\\.")
  )


# The data set covid_type has two different typoes
# for North America.
# A missing h in rows 1 and 5 and
# an extra h in rows 3 and 8

covid_typo <- covid_data |> 
  distinct() |> 
  select(continent, location, date, new_cases) |> 
  filter(continent == "North America")

covid_typo[1, ]$continent <- "Nort America"
covid_typo[5, ]$continent <- "Nort America"

covid_typo[3, ]$continent <- "Northh America"
covid_typo[8, ]$continent <- "Northh America"

covid_typo

# We use str_replace() to fix the whole column.
covid_typo |> 
  mutate(
    continent = str_replace(
      continent,
      "Nort America",
      "North America"
    ),
    continent = str_replace(
      continent,
      "Northh America",
      "North America"
    )
  )

# Or we pass a vector with all replacements to the
# str_replace_all() function.
covid_typo |> 
  mutate(
    continent = str_replace_all(
      continent,
      c(
        "Nort America" = "North America",
        "Northh America" = "North America"
      )
    )
  )


# A common typo is whitespace.
# The stringr package provides the functions
# str_trim() and str_squish().

# str_trim() removes whitespaces from the start/end
# of the string.

# str_squish() removes whitespaces from inside a string.


# The data set covid_ws has a whitespace in the end
# of observations 1 and 5 and a repeated whitespace
# inside the observations 3 and 8

covid_ws <- covid_data |> 
  distinct() |> 
  select(continent, location, date, new_cases) |> 
  filter(continent == "North America")

covid_ws[1, ]$continent <- "Nort America "
covid_ws[5, ]$continent <- "Nort America "

covid_ws[3, ]$continent <- "Northh  America"
covid_ws[8, ]$continent <- "Northh  America"

covid_ws

# Note that tibble() objects automatically add
# quotation marks around the strings to highlight
# the extra white spaces!


# Use str_trim() and str_squish() sequentially
# to get rid of the extra whitespace.
covid_ws |> 
  mutate(
    continent = continent |> 
      str_trim() |> 
      str_squish()
  )


# Employ str_extract() to extract the name of the continents
# in conjunction with str_remove() to remove them from the
# original column.
covid_loc_cont2 <- covid_data |> 
  distinct() |> 
  select(location, continent, date, new_cases, new_deaths) |> 
  mutate(location = paste0(location, continent)) |> 
  select(-continent)

covid_loc_cont2


# Multiple patterns can be separated by |.
continents <- c(
  "Asia",
  "Europe",
  "Africa",
  "South America",
  "North America",
  "Oceania"
) |> 
  paste0(collapse = "|")

continents


covid_loc_cont2 |> 
  mutate(
    continent = str_extract(
      location,
      continents
    ),
    location = str_remove(
      location,
      continents
    )
  )

# This is a creative solution!


### 1.3.4 Date manipulation ----

# The lubridate package supports handling date objects.

# The standard ISO date format YYYY-MM-DD is achieved
# with teh ymd() function.
# It works regardless of how the terms are separated,
# be it 2022/12/01 or 20221201 or 2022-12-01.

# Both full and abbreviated month names are allowed
# 2021-December-01 or 2021-Dec-01

# The same logic applies to the related functions
# mdy(), dmy(), ymd(), dym(), my(), ym() and so on.

library(lubridate)
ymd("2022/12/01")

mdy("december, 1, 2022")

dmy("01122022")

my("Dec-2021")


# If the string format does not match any of the predefined
# patterns, use lubridate::as_date() to declare
# the unusual format, specifying %Y for year,
# %m for month, and %d for day.

# %b represents month names (full or abbreviated)
# and %y represents two-digit years.

# Use the priecR package 
# https://stevecondylios.github.io/priceR/
library(priceR)

# Got to the website
# https://exchangerate.host/
# and sign up for a free account, and 
# use
Sys.setenv("EXCHANGERATEHOST_ACCESS_KEY"="eed0f0acf698b5565cbba8a5f6954526")
usethis::edit_r_environ(scope = "project")
Sys.getenv("EXCHANGERATEHOST_ACCESS_KEY")

# to set the environment variable
# EXCHANGERATEHOST_ACCESS_KEY



# Get historical Brazilian Real (BRL) vs US Dollar
# (USD) exchange rates:
brl_usd <- historical_exchange_rates(
  from = "USD", 
  to = "BRL",
  start_date = "2010-01-01",
  end_date = today()
  )

colnames(brl_usd) <- c("date", "brl")

brl_usd <- brl_usd |> 
  as_tibble() |> 
  mutate(
    date = str_replace_all(date, "-", "/")
  )

brl_usd

# We fist convert the column date with dmy()
brl_usd_aux <- brl_usd |> 
  mutate(
    date = ymd(date)
  )

brl_usd_aux


# Suppose we want to obtain BRL monthly averages.

brl_usd_monthly <- brl_usd_aux |> 
  mutate(
    year = year(date),
    month = month(date)
  ) |> 
  group_by(year, month) |> 
  summarise(brl_monthly = mean(brl))

brl_usd_monthly

# We use make_date() to recover the date format
# YYYY-MM-DD

# For the day parameter, we either set it to 1 (default)
# or to the last day of the month using 
# days_in_month() 

brl_usd_monthly |> 
  mutate(
    date = make_date(
      year = year,
      month = month,
      day = 1
    ),
    date2 = make_date(
      year = year,
      month = month,
      day = days_in_month(date)
    )
  )


# Note that creating a column with the number of days in
# each month is itself often useful.

# If we have only the month totals of a variable and we
# need to compute daily averages for example.


# Next we want quarterly means.
# Be aware that the quarter() function has a parameter
# named with_year that when set to TRUE eliminates
# the need to create a separate column for the year.

brl_usd_quarterly <- brl_usd_aux |> 
  mutate(
    quarter = quarter(date, with_year = TRUE)
  ) |> 
  group_by(quarter) |> 
  summarise(brl_quarterly = mean(brl))

brl_usd_quarterly


# Special attention is needed when working with
# weeks, because the lubridate package provides
# the functions week() and isoweek().


# week() returns the number of complete seven day periods
# that occurred between the date and January 1st.

# isoweek() returns the number of the week
# (from Monday to Sunday) the date belongs to.


week("2022-01-01")
# 1

isoweek("2022-01-01")
# 52

weekdays(date("2022-01-01"))
# "Saturday"

# Because this day is a Saturday,
# isoweek() will include it in the last week of 
# the previous year.


# To compute weekly averages, meaning averages of 
# seven day periods, we use isoweek().


# We can also extract the week day from date objects.
brl_usd_aux |> 
  mutate(wday = wday(date, label = TRUE))

# You may need to change the language settings
Sys.setlocale("LC_TIME", "English")


# The lubridate package imports the special operators
# %m+%
# and
# %m-%
# that allow addition and subtraction of date() objects.
d1 <- ymd("2020-02-29")
d1 %m+% years(2)
# "2022-02-28"

d1 %m-% months(3)
# "2019-11-29"

d1 %m+% days(1)
# "2020-03-01"


# The lesser known add_with_rollback() function gives
# us more control over the output.

# When adding one month to 2022-01-31 we might want
# either 2022-02-28 (the last day of the next month)
# or 2022-03-01 (a period of one month).


# To get the latter, we set the roll_to_first parameter
# to TRUE.
d2 <- ymd("2022-01-31")
add_with_rollback(e1 = d2, e2 = months(1), roll_to_first = TRUE)
# "2022-03-01"

add_with_rollback(e1 = d2, e2 = months(1), roll_to_first = FALSE)
# "2022-02-28"


# The functions floor_date() and ceiling_date()
# will round date objects down or up, respectively,
# to the nearest boundary of the specified time unit.
d3 <- ymd("2021-03-13")

floor_date(d3, unit = "month")
# "2021-03-01"

ceiling_date(d3, unit = "month")
# "2021-04-01"

floor_date(d3, unit = "quarter")
# "2021-01-01"

ceiling_date(d3, unit = "quarter")
# "2021-04-01"

