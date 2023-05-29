#' @title Simulation flow
#' @description
#' Runs the simulation.
#' @param conditions a data frame with simulation conditions, typically
#' constructed using [prepare_conditions]
#' @param coverageScheme a data frame with country-year-survey project coverage
#' scheme (see [check_coverage_scheme])
#' @param nIterPerCond a positive integer - number of iterations to be run for
#' each condition
#' @param suffix optionally a string - suffix that will be added to the names
#' of files storing simulation results (that will be saved to the disk)
#' @details
#' See [run_iteration]
#' @returns (invisibly) a list of three data frames:
#' \describe{
#'   \item{modelSummaries}{basic model summary statistics in a *long* format}
#'   \item{countryMeans}{generated country-year means (as returned by
#'                       [generate_data], i.e. not standardized) and their
#'                       estimates from the models (standardized within the
#'                       group of observed country-means)}
#'   \item{items}{item parameters (generated, not estimated)}
#' }
#' @examples
#' \dontrun{
#' str(conditions)
#' str(coverageScheme)
#' set.seed(12345)
#' run_simulation(conditions[1:2, ], coverageScheme, nIterPerCond = 1L)
#' }
#' @export
run_simulation <- function(conditions, coverageScheme, nIterPerCond,
                           suffix = "") {
  check_conditions(conditions)
  check_coverage_scheme(coverageScheme, conditions)
  stopifnot(is.numeric(nIterPerCond), length(nIterPerCond) == 1,
            as.integer(nIterPerCond) == nIterPerCond, nIterPerCond > 0,
            is.character(suffix), length(suffix) == 1L, !anyNA(suffix))
  models <- prepare_stan_models()
  modelSummaries <- countryMeans <- items <- data.frame()
  for (i in seq_len(nIterPerCond)) {
    for (j in seq_len(nrow(conditions))) {
      resultsIter <- run_iteration(models, coverageScheme, conditions[j, ])
      modelSummaries <- dplyr::bind_rows(modelSummaries,
                                         cbind(i = i,
                                               cond = j,
                                               resultsIter$modelSummaries))
      countryMeans <- dplyr::bind_rows(countryMeans,
                                       cbind(i = i,
                                             cond = j,
                                             resultsIter$countryMeans))
      items <- dplyr::bind_rows(
        items,
        cbind(i = i,
              cond = j,
              resultsIter$items[, names(resultsIter$items) != "thresholds"],
              stats::setNames(as.data.frame(resultsIter$items$thresholds),
                              paste0("threshold", seq_len(ncol(resultsIter$items))))))
      save(conditions, modelSummaries, countryMeans, items,
           file = paste0("cntYearTrends_results", suffix, ".RData"))
    }
  }
  invisible(list(conditions = conditions,
                 modelSummaries = modelSummaries,
                 countryMeans = countryMeans,
                 items = items))
}
