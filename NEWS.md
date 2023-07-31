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
