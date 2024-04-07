# data ----
# Create some of the data used in R4ER
library(tidyverse)
library(priceR)

## 01 Introduction ----
covid_fr <- covid_data |> 
  distinct() |> 
  select(date, location, new_cases, total_cases) |> 
  filter(location == "France") |> 
  arrange(date)

print(covid_fr)
write.csv(covid_fr, file = "data/covid_fr.csv", row.names = FALSE)

covid_uk <- covid_data |> 
  distinct() |> 
  select(date, location, new_cases) |> 
  filter(location == "United Kingdom") |> 
  arrange(date)

print(covid_uk)
write.csv(covid_uk, file = "data/covid_uk.csv", row.names = FALSE)

# The observations in the continent column starts with a random number from 0 to 9
covid_numCont <- covid_data |> 
  distinct() |> 
  select(continent, location, date, new_cases) |> 
  mutate(
    continent = paste0(floor(runif(n = nrow(covid_data), min = 0, max = 9)), ".", continent)
  )

print(covid_numCont)
write.csv(covid_numCont, file = "data/covid_numCont.csv", row.names = FALSE)

# But what if the random numbers had ranged from 0 to 10?
covid_numCont2 <- covid_data |> 
  distinct() |> 
  select(continent, location, date, new_cases) |> 
  mutate(
    continent = paste0(ceiling(runif(n = nrow(covid_data), min = 1, max = 10)), ".", continent)
  )

print(covid_numCont2)
write.csv(covid_numCont2, file = "data/covid_numCont2.csv", row.names = FALSE)

# Typos
covid_typo <- covid_data |> 
  distinct() |> 
  select(continent, location, date, new_cases) |> 
  filter(continent == "North America")

covid_typo[1, ]$continent <- "Nort America"
covid_typo[5, ]$continent <- "Nort America"

covid_typo[3, ]$continent <- "Northh America"
covid_typo[8, ]$continent <- "Northh America"
print(covid_typo)
write.csv(covid_typo, file = "data/covid_typo.csv", row.names = FALSE)


# White space
# The data set "covid_ws" has a whitespace in the endof observations 1 and 5 and 
# a repeated whitespace inside the observations 3 and 8
covid_ws <- covid_data |> 
  distinct() |> 
  select(continent, location, date, new_cases) |> 
  filter(continent == "North America")

covid_ws[1, ]$continent <- "Nort America "
covid_ws[5, ]$continent <- "Nort America "

covid_ws[3, ]$continent <- "Northh  America"
covid_ws[8, ]$continent <- "Northh  America"

print(covid_ws)
write.csv(covid_ws, file = "data/covid_ws.csv", row.names = FALSE)

# Covid_loc_cont2
covid_loc_cont2 <- covid_data |> 
  distinct() |> 
  select(location, continent, date, new_cases, new_deaths) |> 
  mutate(location = paste0(location, continent)) |> 
  select(-continent)

print(covid_loc_cont2)
write.csv(covid_loc_cont2, file = "data/covid_loc_cont2.csv", row.names = FALSE)


# Get historical Brazilian Real (BRL) vs US Dollar (USD) exchange rates:
brl_usd <- historical_exchange_rates(
  from = "USD", 
  to = "BRL",
  start_date = "2010-01-01",
  end_date = today()
)

colnames(brl_usd) <- c("date", "brl")

brl_usd <- brl_usd |> 
  as_tibble() |> 
  mutate(date = str_replace_all(date, "-", "/"))

print(brl_usd)
write.csv(brl_usd, file = "data/brl_usd.csv", row.names = FALSE)


## 04 Seasonal ----
# The data are available here: https://sidra.ibge.gov.br/tabela/8880

# Website of the monthly commercial survey of Brazil
# Psesquisa Mensual de Comercio (PMC): https://sidra.ibge.gov.br/home/pmc/brasil

# Use the `sidrar` R package to access the IBGE data
# - CRAN: https://cran.r-project.org/package=sidrar
# - GitHub: https://github.com/rpradosiqueira
# - Vignette: https://cran.r-project.org/web/packages/sidrar/vignettes/Introduction_to_sidrar.html

# info_sidra(x = 8880, wb = TRUE) # Opens https://apisidra.ibge.gov.br/desctabapi.aspx?c=8880

# table number: 8880
# Variables:
# Index: 7169
# Seasonally adjusted index: 7170

# Nominal sales revenue index: 56733
# Sales volume index: 56734
pmc_ts_nsa <- get_sidra(
  x = 8880, 
  variable  = 7169, 
  classific = "C11046",
  category  = list(56734),
  period    = c(last = 12*24)
)

# Note that 2020 = 100
pmc_ts_nsa <- pmc_ts_nsa |> 
  as_tibble() |> 
  select(`Mês (Código)`, Valor) |> 
  set_names(c("date", "value")) |> 
  mutate(
    year = str_sub(date, end = 4),
    month = str_sub(date, start = 5),
    date = make_date(year = year, month = month, day = 1),
    date2 = make_date(year = year, month = month, day = days_in_month(date))
  ) |> 
  mutate(date = date2, value = as.numeric(value), .keep = "used") |> 
  select(date, value)

print(pmc_ts_nsa)
write.csv(pmc_ts_nsa, file = "data/pmc_ts_nsa.csv", row.names = FALSE)

pmc_ts_sa <- get_sidra(
  x = 8880, 
  variable  = 7170, 
  classific = "C11046",
  category  = list(56734),
  period    = c(last = 12*24)
)

# Note that 2020 = 100
pmc_ts_sa <- pmc_ts_sa |> 
  as_tibble() |> 
  select(`Mês (Código)`, Valor) |> 
  set_names(c("date", "value")) |> 
  mutate(
    year = str_sub(date, end = 4),
    month = str_sub(date, start = 5),
    date = make_date(year = year, month = month, day = 1),
    date2 = make_date(year = year, month = month, day = days_in_month(date))
  ) |> 
  mutate(date = date2, value = as.numeric(value), .keep = "used") |> 
  select(date, value)

print(pmc_ts_sa)
write.csv(pmc_ts_sa, file = "data/pmc_ts_sa.csv", row.names = FALSE)

## 06 hpfilter ----
# Import Brazilian GDP with the `sidrar` R package from the IBGE's API.
# - CRAN: https://cran.r-project.org/package=sidrar
# - GitHub: https://github.com/rpradosiqueira/sidrar/
# - Vignette: https://cran.r-project.org/web/packages/sidrar/vignettes/Introduction_to_sidrar.html

# Define the "date" variable with the`as.yearqrtr()` function from the `zoo` R package
# since it is compatible with `ts` objects, the default input for the `mFilter::hpfilter()` function.

# IBGE website: https://sidra.ibge.gov.br/home/pmc/brasil
# Tab *CNT* - Contas Nacionais Trimestrais - the Quarterly National Accounts
# Table tab: https://sidra.ibge.gov.br/pesquisa/cnt/tabelas
# Table  1620 - Série encadeada do índice de volume trimestral (Base: média 1995 = 100)
# Chained series of the quarterly volume index (Base: 1995 average = 100)

# `sidrar::info_sidra("1620", wb = TRUE)`: 
# Variable description: https://apisidra.ibge.gov.br/desctabapi.aspx?c=1620

# Add parameters to `sidrar::get_sidra()` or click on the *link* symbol: https://sidra.ibge.gov.br/tabela/1621 
# to get the API address directly.

# Select all quarters and receive the link: https://apisidra.ibge.gov.br/values/t/1620/n1/all/v/all/p/all/c11255/90707/d/v583%202
# Only use the part after "values": /t/1620/n1/all/v/all/p/all/c11255/90707/d/v583%202
gdp_br <- sidrar::get_sidra(api = "/t/1620/n1/all/v/all/p/all/c11255/90707/d/v583%202")

print(gdp_br)

write.csv(gdp_br, file = "data/gdp_br.csv", row.names = FALSE)

# END