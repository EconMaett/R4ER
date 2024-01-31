# 11 - Multiple equations model ----

# In the previous section we used a single equation to
# analyze the effect of exogenous variables on the outcome.

# Unfortunately, however, economic relationships often
# constitute a system of endogenous variables.

# If we manage to fit such a system of endogenous variables,
# we can back out the effect of each variable on the whole
# system and the resulting feedback.


# For example, interest rate hikes are expected to lower
# inflation through their contractionary impact on economic
# activity.

# Additionally, the more restrictive monetary policy stance
# should lead to an appreciation of the exchange rate through
# greater capital inflows. This is especially true for emerging
# markets.


# In the Phillips Curve we saw in the previous section,
# all these links were present but implicit.

# We can make these links explicit by creating specific
# equations for bot the exchange rate and capital inflows.

# In summary, working with systems of endogenous equations 
# makes it possible to greatly expand the scope of the analysis
# by incorporating any conceived relationship.


# To do this manually quickly becomes a cumbersome task,
# which is why we use the `bimets` R package.

# It provides a concise interface to write down
# a system of equations, estimate its parameters,
# and forecast based on scenarios for the exogenous
# variables.


# The `bimets` R package is available on:
# - CRAN: https://cran.r-project.org/web/packages/bimets/
# - GitHub: https://github.com/andrea-luciani/bimets
# Vignette: https://cran.r-project.org/web/packages/bimets/vignettes/bimets.pdf

# The `bimets` R package is developed and maintained by
# Andrea Luciani, technical adviser for the directorate general
# for economics, statistics, and research at the Bank of Italy.


# We will present a variation of the standard
# three-equation macroeconomic model composed of
# - an IS curve
# - a Phillips Curve
# - a Monetary Policy rule

# The model is described as follows.

# The IS curve relates the output gap to 
# - its own lag
# - the deviation of the real ex-ante interest rate form its equilibrium
# - the deviation of the terms of trade (ToT) from its trend

# The Phillips Curve is the same from the previous section
# and relates the core CPI to its own lag, the expectation for
# the next 12 months, the percentage change in imported
# prices and output gap.

# The monetary policy rule relates the nominal interest rate to
# its own lags, the deviation of expected inflation from the
# target and the nominal equilibrium rate.


# The first step is to write down the system of equations
# according to the standard adopted by the package.

model_spec <- "
MODEL

COMMENT> IS Curve
BEHAVIORAL> YGAP
TSRANGE 2005 1 2022 4
EQ> YGAP = b1*TSLAG(YGAP,1) + b2*TSLAG((MPR - CPI_EXP - IR_EQ),1) + b3*TOT_GAP
COEFF> b1 b2 b3

COMMENT> Phillips Curve
BEHAVIORAL> CPI_CORE
TSRANGE 2005 1 2022 4
EQ> CPI_CORE = b4*TSLAG(CPI_CORE,1) + b5*CPI_EXP + b6*(TSDELTALOG(CI_USD)) + b7*TSLAG(YGAP,1)
COEFF> b4 b5 b6 b7
RESTRICT> b4+b5+b6=1

COMMENT> Monetary Policy Rule
BEHAVIORAL> MPR
TSRANGE 2005 1 2022 4
EQ> MPR = b8*TSLAG(MPR,1) + b9*TSLAG(MPR,2) + b10*(CPI_EXP - CPI_TARGET_ADJ) + b11*(IR_EQ + CPI_TARGET_ADJ)
COEFF> b8 b9 b10 b11
RESTRICT> b8+b9+b10=1

END
"

# The next step is to load the model specifications and the
# data, which must be supplied as a list of time series (`ts`) objects
library(tidyverse)
library(bimets)

br_economy_data <- readRDS(file = "data/ch13_br_economy_data.rds")
head(br_economy_data)
# date YGAP CPI_CORE CPI_TARGET_ADJ CPI_EXP CI_USD MPR IR_EQ TOT_GAP

macro_model <- LOAD_MODEL(modelText = model_spec)

# Use the `plyr::dlply()` function to convert the data frame
# into a list of `ts` time series objects.
model_data_ts <- br_economy_data |> 
  pivot_longer(-date, names_to = "var", values_to = "value") |> 
  plyr::dlply(
    .variables = "var", 
    .fun = function(x) {
      TIMESERIES(x$value, START = c(2004,1), FREQ = 4)
    }
  )

macro_model <- LOAD_MODEL_DATA(
  model = macro_model,
  modelData = model_data_ts,
  quietly = FALSE
)

# By default, the equations are estimated with OLS,
# but it is also possible to use instrumental variables (IV).

# Use `quietly = TRUE` to suppress the model output.
model_fit <- ESTIMATE(
  model = macro_model,
  estTech = "OLS",
  quietly = FALSE
)

# We can use the estimated model to produce forecasts
# for future values.

# We first need to provide future values for the exogenous
# variables, which we create with the `TSEXTEND()` function
# from the `bimets` R package.


# We assume that both the CPI target and the equilibrium
# real interest rate will remain constant.

# The expectations for CPI will evolve according to a linear trend.

# The gap of terms of trade will decrease by 2.4% each quarter,
# which is the mean of the last four quarters.

# Imported prices will decrease 2% each quarter, partially
# reverting the surge of the last years.

model_fit$modelData <- within(
  
  data = model_fit$modelData, expr = {
    
    CPI_TARGET_ADJ = TSEXTEND(
      CPI_TARGET_ADJ,
      UPTO = c(2026,1),
      EXTMODE = "CONSTANT"
    )
    
    CPI_EXP = TSEXTEND(
      CPI_EXP,
      UPTO = c(2026,1),
      EXTMODE = "LINEAR"
    )
    
    CI_USD = TSEXTEND(
      CI_USD,
      UPTO = c(2026,1),
      EXTMODE = "MYRATE",
      FACTOR = (1-0.02)
    )
    
    IR_EQ = TSEXTEND(
      IR_EQ,
      UPTO = c(2026,1),
      EXTMODE = "CONSTANT"
    )
    
    TOT_GAP = TSEXTEND(
      TOT_GAP,
      UPTO = c(2026,1),
      EXTMODE = "MYCONST",
      FACTOR = -2.4
    )
  }
)

# Note that we have used `base::within(data, expr)` for this
# purpose
model_fit$modelData$CI_USD |> time() |> last()

# Use `base::do.call(what, args)` to convert this object
# into tidy format to make it easier to lot all the extended
# time series in a single panel.

# We use the `stats::ts.union()` function to bind the
# time series objects inside `model_fit$modelData` together.

# We use `timetk::tk_tbl()` to coerce the `ts` objects into a tibble
do.call(what = ts.union, args = model_fit$modelData) |> 
  timetk::tk_tbl() |> 
  pivot_longer(cols = -index, names_to = "var", values_to = "value") |> 
  filter(!(var %in% c("CPI_CORE", "YGAP", "MPR"))) |> 
  mutate(scenario = if_else(condition = index >= "2023 Q1", true = "Y", false = "N")) |> 
  ggplot(mapping = aes(x = index, y = value, color = scenario)) +
  geom_line(lwd = 1) +
  scale_color_manual(values = c("black", "firebrick")) +
  facet_wrap(facets = ~ var, scales = "free_y") +
  theme_bw() +
  theme(legend.position = "top") +
  zoo::scale_x_yearqtr(n = 10, format = "%Y") +
  labs(
    title = "Scenarios for exogenous variables",
    x = "", y = "", color = "Scenario",
    caption = "Data source: Brazilian Central Bank"
  )

ggsave(filename = "figures/11_fcst-input.png", height = 8, width = 12)
graphics.off()


# We can use the `SIMULATE()` function from the
# `bimets` R package to simulate a range of forecasts
model_sim <- SIMULATE(
  model = model_fit,
  simType = "FORECAST",
  TSRANGE = c(2023,1,2025,1),
  simConvergence = 0.00001,
  simIterLimit = 100,
  quietly = FALSE
)

# Again we convert the output to a data frame.
# This time we call `base::do.call(what, args)`
# to apply the function `stats::ts.intersect()`
# to the elements of the list `model_fit$modelData`
output <- do.call(what = ts.intersect, args = model_fit$modelData) |> 
  timetk::tk_tbl(rename_index = "date") |> 
  mutate(type = "Observed") |> 
  bind_rows(
    do.call(
      what = ts.intersect,
      args = model_sim$simulation[1:3]
    ) |> 
      timetk::tk_tbl(rename_index = "date") |> 
      mutate(type = "Forecast")
  ) |> 
  mutate(date = zoo::as.yearqtr(date))


# Plot the output data
output |> 
  select(date, YGAP, CPI_CORE, MPR, type) |> 
  pivot_longer(cols = -c(date, type), names_to = "var", values_to = "value") |> 
  ggplot(mapping = aes(x = date, y = value, color = type, linetype = type)) +
  geom_line(lwd = 1) +
  scale_linetype_manual(values = c(2, 1)) +
  zoo::scale_x_yearqtr(n = 5, format = "%YQ%q") +
  facet_wrap(facets = ~ var, scales = "free_y", nrow = 3) +
  theme_bw() +
  theme(legend.position = "top") +
  labs(
    title = "Forecasts for economic variables",
    x = "", y = "", linetype = "", color = "",
    caption = "Data source: Brazilian Central Bank"
  )

ggsave(filename = "figures/11_fcst-output.png")
graphics.off()

# END