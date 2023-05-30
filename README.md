# cntYearTrends

The *cntYearTrends* package enables performing simulations to analyze the properties of Solt's and Claassen's models for country-year trends data in different conditions including different levels of survey items' measurement non-invariance.

## Installation

On Windows you need to install Rtools42 first (into the folder which path doesn't contain any blank or special characters).

Then you can proceed to install the package dependencies and the package itself:

``` r
# cmdstanr and Stan
install.packages("cmdstanr",
                 repos = c("https://mc-stan.org/r-packages/",
                           getOption("repos")))
cmdstanr::check_cmdstan_toolchain(fix = TRUE)
cmdstanr::install_cmdstan()
# package itself
remotes::install_github("tzoltak/cntYearTrends")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(cntYearTrends)
str(conditions)
str(coverageScheme)
set.seed(12345)
run_simulation(conditions[1:2, ], coverageScheme, nIterPerCond = 1L)
```

Simulation results will be saved to the file "cntYearTrends_results.RData".
