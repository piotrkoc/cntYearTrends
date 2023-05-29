#' @title Preparing simulation conditions
#' @description
#' Checks structure of a data frame storing projects' country-years coverage.
#' @param coverage a one-row data frame containing values of parameters that
#' should be hold constant across all the simulation conditions
#' @param conditions a data frame storing simulation conditions (other than
#' projects' country-years coverage) - see [prepare_conditions]
#' @details
#' Data frame given by the `coverage` argument stores the data in a *long*
#' format with each row representing that a given country in a given year was
#' covered by a given survey project. It must contain columns:
#' \describe{
#'   \item{variant}{ (a string) - name of the coverage variant (must match names in the column `variant` of the data frame provided by the `conditions` argument)}
#'   \item{year}{ (an integer) - year}
#'   \item{project}{ (a string) - survey project name}
#'   \item{countryGroup}{ (a string) - name of the group of country that represents a given row}
#' }
#' @returns its argument `coverage` (or throws an error)
#' @seealso [prepare_conditions], [check_conditions]
#' @export
check_coverage_scheme <- function(coverage, conditions) {
  stopifnot(is.data.frame(coverage),
            all(c("variant", "year", "project", "countryGroup") %in% names(coverage)),
            is.character(coverage$variant),
            is.numeric(coverage$year),
            all(as.integer(coverage$year) == coverage$year),
            is.character(coverage$project),
            is.character(coverage$countryGroup),
            !anyNA(coverage))
  stopifnot(is.data.frame(conditions),
            "variant" %in% names(conditions))
  stopifnot(!anyNA(conditions$variant),
            all(conditions$variant %in% coverage$variant))
  return(coverage)
}
