# 05 - Deflating nominal values to real values ----

# We saw earlier that it is important to seasonally adjust
# time series in order to make meaningful comparisons
# between time periods.

# Similarly, nominal time series (those denominated in current
# prices) should be deflated to real values to remove the effect
# of inflation over time.


# First, we need to choose the base (or reference) year.
# The prevailing prices in that year will be used as a
# reference for other years.

# We can transform a sample of the US nominal GDP from 2000
# to 2020 using 2010 prices as reference, for example.

# This is generally an arbitrary choice, but it is a good practice
# to choose a year in which prices have been close to the sample average.


# Secondly, we need to choose an appropriate price index
# as the deflator.

# The ideal deflator is one that measures the change in prices
# of the basked of goods and services represented in the series
# we want to adjust.

# For example, if we want to deflate a series of retail sales
# we should use a specific price index for the basket of goods
# considered in the retail sales series.

# Only if such a price index is absent should we use
# a more general consumer price index as a proxy for inflation.


# We will sue data on nominal GDP for the United States provided 
# by the U.S. Department of Commerce's Bureau of Economic Analysis (BEA).

# This is a good opportunity to introduce how to access
# data from this relevant source using its API.

# You need to register in order to receive access to the API.
# Store your personal API key in your `.Renviron` file, which
# you open with `usethis::edit_r_environ()`.
# You will have to restart your R session after saving the 
# API key as "BEA_API_KEY"
# Check that R recognizes the BEA API key
Sys.getenv("BEA_API_KEY")

# Check out the BEA API documentation
# at https://apps.bea.gov/api/_pdf/bea_web_service_api_user_guide.pdf

# We want to retrieve the table names from the National 
# Income and Product Accounts (NIPA) data base, where GDP
# data are stored.

# Note that we use `purrr::pluck()` to substitute for `[[]]`
# to access elements from a `base::list()` object.

# `purrr::pluck(.x, 1, 2, 1)` is equivalent to
# `.x[[1]][[2]][[1]]`

library(tidyverse)
library(glue)
library(httr)
library(jsonlite)

api_bea_key <- Sys.getenv("BEA_API_KEY")
bea_nipa_table <- httr::GET(
  url = glue::glue(
    "
    https://apps.bea.gov/api/data?UserID={
    api_bea_key
    }&method=GetParameterValues&datasetname=NIPA&ParameterName=tablename&ResultFormat=JSON
    "
    )
  ) |> 
  httr::content(as = "text") |> 
  jsonlite::fromJSON() |> 
  purrr::pluck(1, 2, 1)

names(bea_nipa_table)
# "TableName" "Description"

as_tibble(bea_nipa_table)

# We see that the two tables of interest are
# - "T10104": Table 1.1.4. Price Indexes for Gross Domestic Product (A) (Q)                                 
# - "T10105": Table 1.1.5. Gross Domestic Product (A) (Q)

# Given that the import procedure is the same for both series,
# it makes sense to create a function and use `purrr::map(.x, .f)`
# to aply it to each table name.
bea_tables <- list(
  "Nominal GDP" = "T10105",
  "Deflator" = "T10104"
)

get_bea_data <- function(tablename, api_bea_key = Sys.getenv("BEA_API_KEY")) {
  
  api_bea_request <- glue::glue(
    "
    https://apps.bea.gov/api/data?UserID={
    api_bea_key
    }&method=GetData&DataSetName=NIPA&TableName={
    tablename
    }&Frequency=A&Year=ALL&ResultFormat=json
    "
  )
  
  gdp_request <- httr::GET(url = api_bea_request)
  gdp_content <- httr::content(gdp_request, as = "text")
  gdp_list    <- jsonlite::fromJSON(txt = gdp_content, flatten = FALSE)
  gdp_tbl     <- purrr::pluck(gdp_list, 1, 2, 4)
  
}

# Apply the function to both table names
bea_data <- purrr::map(
  .x = bea_tables,
  .f = ~ get_bea_data(.x, api_bea_key)
)

names(bea_data)
# "Nominal GDP" "Deflator"

head(bea_data$`Nominal GDP`)

head(bea_data$Deflator)


# Arrange the data in tidy format to facilitate future
# calculations.

bea_data_tbl <- purrr::map(
  .x = bea_data,
  .f = ~ .x |> 
    dplyr::filter(LineDescription == "Gross domestic product") |> 
    dplyr::select(TimePeriod, DataValue)
  ) |> 
  plyr::ldply(.id = "Serie") |> 
  tidyr::pivot_wider(names_from = Serie, values_from = DataValue) |> 
  dplyr::mutate(dplyr::across(.cols = c(`Nominal GDP`), .fns = ~ stringr::str_remove_all(string = .x, pattern = ","))) |> 
  dplyr::mutate(dplyr::across(.cols = -TimePeriod, .fns = ~ .x |> as.numeric())) |> 
  dplyr::arrange(TimePeriod)


print(bea_data_tbl)
# TimePeriod `Nominal GDP` Deflator
# <chr>       <dbl>       <dbl>

# Now we are ready to convert nominal GDP into real GDP.

# We pick 2005 as the arbitrary reference year, and divide
# the whole price index series by its value in 2205.

# This means the "Deflator" series will be equal to 1 in 2005.

# We then divide the nominal GDP series by the new price index series.

gdp_real <- bea_data_tbl |> 
  mutate(
    Deflator_2005 = (Deflator / Deflator[which(TimePeriod == 2005)]),
    `Real GDP` = `Nominal GDP` / Deflator_2005
  )

print(gdp_real)
# TimePeriod `Nominal GDP` Deflator Deflator_2005 `Real GDP`

# Now we plot nominal and real GDP

gdp_real |> 
  pivot_longer(cols = -TimePeriod, names_to = "var", values_to = "value") |> 
  filter(str_detect(string = var, pattern = "Deflator", negate = TRUE)) |> 
  mutate(TimePeriod = as_date(TimePeriod, format = "%Y")) |> 
  ggplot(mapping = aes(x = TimePeriod, y = value, color = var)) +
  geom_line(lwd = 2) +
  scale_y_continuous(
    labels = scales::dollar_format(scale = 1/1e6, prefix = "$", suffix = "T")
  ) +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
  labs(
    title = "US Annual GDP: Nominal vs. Real (2005 Dollars)",
    x = "", y = "",
    color = "",
    caption = "Data source: Bureau of Economic Analysis (BEA)"
  ) +
  theme(legend.position = "top") +
  theme_light()

ggsave(filename = "05_us-gdp-real-nominal.png", path = "figures/", height = 4, width = 8)
graphics.off()

# It is clear that ignoring price changes would lead to misinterpretations of 
# GDP trajectory.

# This is why we need to adjust economic time series
# for inflation.

# END