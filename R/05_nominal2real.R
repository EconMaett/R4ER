# 05 - Deflating nominal values to real values ----
# URL: https://book.rleripio.com/ds_nominal2real
library(tidyverse)
library(scales)
library(glue)
library(httr)
library(jsonlite)
fig_path <- "figures/"
Sys.setlocale("LC_TIME", "English")
# Seasonally adjust time series to make meaningful comparisons between time periods.

# Nominal time series (those denominated in current prices) should be deflated 
# to real values to remove the effect of inflation over time.

# First, we need to choose the base (or reference) year.
# The prevailing prices in that year will be used as a reference for other years.

# We can transform a sample of the US nominal GDP from 2000
# to 2020 using 2010 prices as reference, for example.

# This is generally an arbitrary choice, but it is a good practice
# to choose a year in which prices have been close to the sample average.

# Choose an appropriate price index as the deflator.

# The ideal deflator is one that measures the change in prices of the basked of 
# goods and services represented in the series we want to adjust.

# For example, if we want to deflate a series of retail sales
# we should use a specific price index for the basket of goods
# considered in the retail sales series.

# Only if such a price index is absent should we use
# a more general consumer price index as a proxy for inflation.

# We will sue data on nominal GDP for the United States provided 
# by the U.S. Department of Commerce's Bureau of Economic Analysis (BEA).

# This is a good opportunity to introduce how to access
# data from this relevant source using its API.

# Check that R recognizes the BEA API key
Sys.getenv("BEA_API_KEY")

# BEA API documentation: https://apps.bea.gov/api/_pdf/bea_web_service_api_user_guide.pdf

# Retrieve the table names from the National Income and Product Accounts (NIPA) data base, 
# where GDP data are stored.

# Note that we use `pluck()` to substitute for `[[]]` to access elements from a `base::list()` object.
# `pluck(.x, 1, 2, 1)` is equivalent to `.x[[1]][[2]][[1]]`
api_bea_key <- Sys.getenv("BEA_API_KEY")

bea_nipa_table <- GET(
  url = glue(
    "https://apps.bea.gov/api/data?UserID={
    api_bea_key
    }&method=GetParameterValues&datasetname=NIPA&ParameterName=tablename&ResultFormat=JSON"
    )
  ) |> 
  content(as = "text") |> 
  fromJSON() |> 
  pluck(1, 2, 1)

names(bea_nipa_table)
# "TableName" "Description"

as_tibble(bea_nipa_table)
# We see that the two tables of interest are
# - "T10104": Table 1.1.4. Price Indexes for Gross Domestic Product (A) (Q)                                 
# - "T10105": Table 1.1.5. Gross Domestic Product (A) (Q)

# Given that the import procedure is the same for both series,
# it makes sense to create a function and use `map(.x, .f)`
# to apply it to each table name.
bea_tables <- list(
  "Nominal GDP" = "T10105",
  "Deflator"    = "T10104"
)

get_bea_data <- function(tablename, api_bea_key = Sys.getenv("BEA_API_KEY")) {
  
  api_bea_request <- glue(
    "https://apps.bea.gov/api/data?UserID={
    api_bea_key
    }&method=GetData&DataSetName=NIPA&TableName={
    tablename
    }&Frequency=A&Year=ALL&ResultFormat=json"
  )
  
  gdp_request <- GET(url = api_bea_request)
  gdp_content <- content(gdp_request, as = "text")
  gdp_list    <- fromJSON(txt = gdp_content, flatten = FALSE)
  gdp_tbl     <- pluck(gdp_list, 1, 2, 4)
  
}

# Apply the function to both table names
bea_data <- map(.x = bea_tables, .f = ~ get_bea_data(.x, api_bea_key))

names(bea_data)
# "Nominal GDP" "Deflator"

head(bea_data$`Nominal GDP`)

head(bea_data$Deflator)

# Arrange the data in tidy format to facilitate future calculations.
bea_data_tbl <- map(
  .x = bea_data,
  .f = ~ .x |> 
    filter(LineDescription == "Gross domestic product") |> 
    select(TimePeriod, DataValue)
  ) |> 
  plyr::ldply(.id = "Serie") |> 
  pivot_wider(names_from = Serie, values_from = DataValue) |> 
  mutate(across(.cols = c(`Nominal GDP`), .fns = ~ str_remove_all(string = .x, pattern = ","))) |> 
  mutate(across(.cols = -TimePeriod, .fns = ~ .x |> as.numeric())) |> 
  arrange(TimePeriod)

print(bea_data_tbl)
# Now we are ready to convert nominal GDP into real GDP.

# We pick 2005 as the arbitrary reference year, and divide
# the whole price index series by its value in 2205.

# This means the "Deflator" series will be equal to 1 in 2005.

# We then divide the nominal GDP series by the new price index series.
gdp_real <- bea_data_tbl |> 
  mutate(
    Deflator_2005 = (Deflator / Deflator[which(TimePeriod == 2005)]),
    `Real GDP`    = `Nominal GDP` / Deflator_2005
  )

print(gdp_real)
# TimePeriod `Nominal GDP` Deflator Deflator_2005 `Real GDP`

# Now we plot nominal and real GDP
gdp_real |> 
  pivot_longer(cols = -TimePeriod, names_to = "var", values_to = "value") |> 
  filter(str_detect(string = var, pattern = "Deflator", negate = TRUE)) |> 
  mutate(TimePeriod = as_date(TimePeriod, format = "%Y")) |> 
  ggplot(mapping = aes(x = TimePeriod, y = value, color = var)) +
  geom_line(lwd = 1.2) +
  scale_y_continuous(
    labels = dollar_format(scale = 1/1e6, prefix = "$", suffix = "T")
  ) +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
  labs(
    title = "US Annual GDP: Nominal vs. Real (2005 Dollars)",
    x = NULL, y = NULL,
    color = "",
    caption = "Data source: Bureau of Economic Analysis (BEA)"
  ) +
  theme_light() +
  theme(
    legend.position = "top"
  )

ggsave(filename = "05_us-gdp-real-nominal.png", path = fig_path, height = 8, width = 10, bg = "white")
graphics.off()

# Ignoring price changes would lead to misinterpretations of GDP trajectory.
# END