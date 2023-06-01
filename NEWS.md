# cntYearTrends 0.2.0 (31.05.2023)

## New features

-   Distributions of responses to the items returned as additional element of simulation results.
-   New `check_generated_data()` function allowing to check whether the assumed simulation conditions do not lead (too often) to generating of the data with empty cells in project-country-year-item response distributions, or even worse distributions with only one non-zero cell.

## Bug fixes

-   `run_simulation()` doesn't fail due to wrongly assessing number of columns storing items' threshold values.
-   Data generating functions consequently use integer as a format for variables storing project, country, year and item ids.

# cntYearTrends 0.1.0 (28.05.2023)

-   First usable version of the package - see README.md.
