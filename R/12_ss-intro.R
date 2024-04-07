# 12 - Introduction to state-space models ----

# One of the first R packages to effectively
# fit state-space models to time series data was the
# `MARSS` R package.

# Multivariate Auto-Regressive State-Space Modeling (MARSS)

# Provides maximum-likelihood parameter estimation for
# dynamic linear models (DLMs) and
# vector auto-regressive models (VARs)

# Fitting is available via the 
# Expectation-Maximization (EM) algorithm
# BFGS
# TMB (using the `marssTMP` companion package).

# Functions are provided for parametric and innovations 
# bootstrapping

# Kalman filtering and smoothing

# Model selection criteria including
# bootstrap AICb
# confidence intervals via the Hessian approximation
# or bootstrapping
# All conditional residual types.


# The `MARSS` R package is available on:
# - CRAN: https://cran.r-project.org/package=MARSS
# - GitHub: https://github.com/atsa-es/MARSS
# - Website: https://atsa-es.github.io/


# Even though these models are widely used in practice,
# they go often unmentioned in data science and econometrics
# textbooks.


# There are two common applications of state-space models:

# 1. Estimate time-varying coefficients from regression models
# 2. Estimate a common underlying process when there are multiple sources
#    of information.


# 
