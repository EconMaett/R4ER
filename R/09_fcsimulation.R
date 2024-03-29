# 09 - Simulations ----

# Often we have some equation with calibrated coefficients,
# either form literature or from a previously estimated
# model, that informs us how a variable of interest evolves.

# We can make forecasts conditional on scenarios for the explanatory
# variables.

# Suppose that it is widely known that the CPI increases
# around 0.5pp for every 10% of exchange rate depreciation
# in a given country.

# In this case we can make predictions for the CPI based
# on what we expect for the exchange rate in different
# scenarios, assuming all other things being equal.

# In scenario A, the exchange rate increases by 5%,
# in scenario B, it decreases by 7%, etc.


# This approach has some limitations, such as that we do not
# provide the complete distribution of possible predictions
# that would allow us to infer the uncertainty around a
# central value.


# We want to generate distributions that reflect our scenario
# for each variable and then simulate a large number of 
# joint scenarios for the outcome.

# Take the common example from a macroeconomics textbook
# about the evolution of public debt.

# Assume that the Debt-to-GDP ratio evolves according the the
# expression
# \delta b_{t+1} \equals \left(r_{t+1} - g_{t+1}\right) \times b_{t} - s_{t+1}

# Where `b` is the Debt-to-GDP ratio, `r` is the real interest rate,
# `g` is the real GDP growth rate, and `s` is the government primary
# surplus as proportion of real GDP.


# We make the following assumptions for the variables on the RHS of the
# above equation:

# 1. CPI is around the target at 2% but with a higher probability of
#    ending the year above rather than below the target.

# 2. Real GDP is expected to grow 3%, but with downward risks.

# 3. The real interest rate will be raised to either 3% (40% chance)
#     or to 4% (60%) chance.

# 4. The primary surplus will be zero, with no uncertainty.


# We use the `sn` R package to build skewed distributions for 
# both CPI and GDP.

# sn: The Skew-Normal and Related Distributions Such as the Skew-t and the SUN

# The package is available on:
# - CRAN: https://cran.r-project.org/package=sn
# - Website: http://azzalini.stat.unipd.it/SN/

# The first step is to supply the parameters of a Gaussian
# distribution, the mean and the standard deviation,
# plus a `gamma` parameter that controls the degree of skewness.

# We use the `sn::rsn()` function to sample random values from the
# distributions generated by these parameters.

# For the interest rate variables, we use the `base::sample()` function.
library(tidyverse)
library(sn)

set.seed(123)

sn_parameters <- purrr::map(
  .x = list(
    "CPI" = c(mean = 2.0, s.d. = 0.4, gamma =  0.8),
    "GDP" = c(mean = 3.0, s.d. = 0.6, gamma = -0.8)
  ),
  .f = sn::cp2dp, family = "sn"
)


# Number of simulations
n_sim <- 100

variables_sim <- tibble::tibble(
  CPI = sn::rsn(n = n_sim, dp = sn_parameters[["CPI"]]),
  GDP = sn::rsn(n = n_sim, dp = sn_parameters[["GDP"]]),
  IR  = base::sample(x = c(3.0, 4.0), size = n_sim, prob = c(0.40, 0.60), replace = TRUE)
)


# Defining the values of the gamma parameter is a matter of trial and error
# until we get the desired shape of the distribution.

# We somewhat exaggerate the parameter values to make the assymetry
# of the distributions obvious.

# Additionally, we must be cautions about the number of simulation
# since they are very memory consuming.

# Let's plot the results below.


variables_sim |> 
  rowid_to_column(var = "n") |> 
  pivot_longer(cols = -n, names_to = "var", values_to = "value") |> 
  ggplot(mapping = aes(x = value)) +
  geom_histogram(fill = "steelblue2", alpha = 0.8) +
  theme_bw() +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  facet_wrap(facets = ~ var, scales = "free_y") +
  labs(
    title = "Assymetric distributions",
    x = "value", y = "n"
  )

ggsave(filename = "figures/09_gdp-cpi-ir-distr.png", height = 8, width = 12)
graphics.off()


# We use the `purrr::cross()` function to create all possible
# combinations from the three variables.
# These are our scenarios where we make the assumption that
# the input variables are independent from each other.

scenarios_sim <- purrr::cross3(
  .x = variables_sim$CPI,
  .y = variables_sim$GDP,
  .z = variables_sim$IR
)

# Not that this will take a while
length(scenarios_sim)

# Finally, we compute the Debt-to-GDP equation for each scenario
# assuming the initial Debt-to-GDP ratio is 60%.

debt2gdp_sim <- purrr::map_dbl(
  .x = scenarios_sim,
  .f = function(x) {
    x <- unlist(x) 
    CPI <- x[1]
    GDP <- x[2]
    IR  <- x[3]
    r   <- IR - CPI
    b0   <- 60.0
    b1  <- (r - GDP) * (b0 / 100)
    return(b1)
  }
)

summary(debt2gdp_sim)

# Some interesting statistics are returned by the `summary()`
# method.

# In the most extreme scenarios, the Debt-to-GDP ratio would
# increase by 0.93pp or decrease by -2.78pp, while the expected
# scenario is a more modest drop around -0.86pp.

# Also, at least 75% of the values in the distribution
# are negative.

tibble::tibble(b1 = debt2gdp_sim) |> 
  ggplot(mapping = aes(x = b1)) +
  geom_histogram(fill = "steelblue3") +
  labs(
    title = "Debt-to-GDP variation for the next period (pp)",
    y = "Frequency", x = "Variation (pp)"
  ) +
  theme(
    axis.text = element_text(size = 12), 
    title = element_text(size = 12)
    ) +
  theme_light()

ggsave(name = "figures/09_gdp-scenarios.png", height = 8, width = 12)
graphics.off()


# Note that it would be more realistic if the interest rate
# would assume the 4% value if inflation were above target.

# We could create rules to account for such dependencies
# and use them to filter the `variables_sim` data frame.

# END