# cntYearTrends 0.5.0 (26.02.2025)

## New features

-   Generation of item thresholds can now be performed using two different approaches:
    1.   The one used before with item difficulty sampled from a truncated-normal distribution, which parameters are definded by `difficultyMean`, `difficultySD`, `difficultyLB` and `difficultyUB`.
         -    Separation between thresholds is sampled from a uniform distribution but now its parameters are not fixed to (0.3, 1) but are defined in a data frame describing simulation conditions using columns `thresholdsIncrLB` and `thresholdsIncrUB`.
    2.   New approach introduced to not cause too much problems for the DCPO model, which (because of its priors) tends to fail encountering items with too negative value of first threshold:
         -    First threshold is sampled from a uniform distribution which parameters are defined by `difficultyLB` and `difficultyUB`.
         -    Separation between consecutive thresholds are sampled from a uniform distribution which parameters are defined by `thresholdsIncrLB` and `thresholdsIncrUB` (as in the previous approach).
         -    Simulation parameters `difficultyMean` and `difficultySD` should be set to `NA_real_` for this approach to be used.

## Changes

-   Simulation conditions in the `conditions` dataset and examples for `prepare_conditions()` changed so they use the second of the approaches described above with `difficultyLB` set to -1, `difficultyUB` set to 0, `thresholdsIncrLB` set to 0.3 and `thresholdsIncrUB` set to 0.5.
-   Updated deprecated Stan syntax defining arrays in *inst/claassen.stan* and *inst/dcpo.stan*.

# cntYearTrends 0.4.0 (26.01.2025)

## Changes

-   Changed the way the item thresholds are generated, to get more plausible distributions of item responses. Now:
    -   Item (in fact project-item) difficulties are generated from the truncated-normal distribution with parameters set using new arguments `difficultyMean`, `difficultySD`, `difficultyLB`, `difficultyUB` defined in the `conditions` data frame.
    -   Item thresholds are generated to have average of the previously generated item difficulty and distances between consecutive thresholds sampled from the distribution uniform on [0.3, 1].
-   Changed the value of `difficultyYSD` in the `conditions` data set to 0.1 (from 0.2).
-   Standardization of the estimated results was moved from `summarize_dcpo_results()` and `summarize_claassen_results()` to `run_iteration()` so it is performed considering only the set of actually observed country-years. Consequently, columns `meanStd` and `varSdt` were added to the results returned by the `run_iteration()`, which contain analogously standardized values from the data-generating model - to be compared with the estimated ones
# cntYearTrends 0.4.0 (26.01.2025)

## New features

-   Function `bind_results()` enabling to bind simulation results saved in different .RData files into a one set of data frames (and possibly saving them in one .RData file).

## Changes

-   Changed the way the item thresholds are generated, to get more plausible distributions of item responses. Now:
    -   Item (in fact project-item) difficulties are generated from the truncated-normal distribution with parameters set using new arguments `difficultyMean`, `difficultySD`, `difficultyLB`, `difficultyUB` defined in the `conditions` data frame.
    -   Item thresholds are generated to have average of the previously generated item difficulty and distances between consecutive thresholds sampled from the distribution uniform on [0.3, 1].
-   Changed the value of `difficultyYSD` in the `conditions` data set to 0.1 (from 0.2).
-   Standardization of the estimated results was moved from `summarize_dcpo_results()` and `summarize_claassen_results()` to `run_iteration()` so it is performed considering only the set of actually observed country-years. Consequently, columns `meanStd` and `varSdt` were added to the results returned by the `run_iteration()`, which contain analogously standardized values from the data-generating model - to be compared with the estimated ones

# cntYearTrends 0.3.1 (31.07.2023)

## Bug fixes

-   Changed the way columns *r* and *rr* are created in `format_dcpo()` to take into account, that values of response categories in the generated data starts from 0, while they are assumed to start from 1 in Solt's code.

## Changes

-   Switched to *dplyr* joins instead of `merge()` for merging data tables to increase speed and decrease memory footproint.

# cntYearTrends 0.3.0 (30.06.2023)

## Changes

-   Switched to sampling country-year means in the first period from a uniform distribution. API changed accordingly.
-   API for defining bounds for countr-year means changed to two separate parameters.
-   Changed the `conditions` dataset and example in `prepare_conditions()` that generates it.

# cntYearTrends 0.2.2 (29.06.2023)

## New features

-   Country-year means can be generated within given bounds using additional simulation parameter `arMeanBounds`.

# cntYearTrends 0.2.1 (13.06.2023)

## New features

-   `check_generated_data()` reports also on the frequency of one point distributions of responses dichotomized according to the (original) Claassen's codding scheme (i.e. `>= response_scale_length / 2`).

## Other changes

-   The dataset `conditions` was changed to assure that items in the generated data will have one-point distribution (in some project-country-years) much less frequently. For this purpose:
    - `projectBiasesSD` was decreased from 1 to 0.5,
    - `nRespondents` was increased from 1000 to 1500,
    - `relThresholdsL` was decreased from 1 to 0.5,
    - `arVarStartLB` was increased from 0.4. to 0.6 and `arVarStartUB` was decreased from 1.6 to 1.4.
-   Values in the use example for the `prepare_conditions()` were changed accordingly.
-   For the same reasons, multiplication factors in the default values of the `unstLoadingsCL`, `unstLoadingsYL`, `difficultyCL` and `difficultyYL` arguments to the `generate_items()` were changed from 3 to 2.

# cntYearTrends 0.2.0 (1.06.2023)

## New features

-   Distributions of responses to the items returned as additional element of simulation results.
-   New arguments `iter` and `stanPars` to `run_simulation` (and `run_iteration`) allowing to provide arguments that will be passed to the Stan models `sample` method.
-   Information about current iteration is displayed by `run_simulation`.
-   New `check_generated_data()` function allowing to check whether the assumed simulation conditions do not lead (too often) to generating of the data with empty cells in project-country-year-item response distributions, or even worse distributions with only one non-zero cell.

## Bug fixes

-   `run_simulation()` doesn't fail due to wrongly assessing number of columns storing items' threshold values.
-   Data generating functions consequently use integer as a format for variables storing project, country, year and item ids.

# cntYearTrends 0.1.0 (28.05.2023)

-   First usable version of the package - see README.md.
