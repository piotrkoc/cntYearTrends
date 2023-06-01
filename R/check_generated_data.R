#' @title Preparing simulation conditions
#' @description
#' Checks whether the data generated according to the given simulations contains
#' empty cells or cells with only several observations in distributions of
#' answers within a given county-year-project.
#' @inheritParams run_simulation
#' @returns a data frame provided by the `conditions` argument with additional
#' columns:
#' \describe{
#'   \item{maxNPointDistrib}{maximum number of project-country-year-items
#'                           with one-point distributions (i.e. with only one
#'                           cell with non-zero frequency) across replications
#'                           of generating data under a given condition}
#'   \item{q95NPointDistrib}{95. percentile as above}
#'   \item{q90NPointDistrib}{90. percentile as above}
#'   \item{maxPctPointDistrib}{maximum percent of project-country-year-items
#'                             as above}
#'   \item{q95PctPointDistrib}{95. percentile as above}
#'   \item{q90PctPointDistrib}{90. percentile as above}
#'   \item{maxNZeroCells}{maximum number of project-country-year-items
#'                        with some zero-count cells in response distribution
#'                        across replications of generating data under a given
#'                        condition}
#'   \item{q95NZeroCells}{95. percentile as above}
#'   \item{q90NZeroCells}{90. percentile as above}
#'   \item{maxPctZeroCells}{maximum percent of project-country-year-items
#'                          as above}
#'   \item{q95PctZeroCells}{95. percentile as above}
#'   \item{q90PctZeroCells}{90. percentile as above}
#' }
#' @export
check_generated_data <- function(conditions, coverageScheme, nIterPerCond) {
  check_conditions(conditions)
  check_coverage_scheme(coverageScheme, conditions)
  stopifnot(is.numeric(nIterPerCond), length(nIterPerCond) == 1,
            as.integer(nIterPerCond) == nIterPerCond, nIterPerCond > 0)

  results <- data.frame(
    maxNPointDistrib = rep(NA_real_, nrow(conditions)),
    q95NPointDistrib = rep(NA_real_, nrow(conditions)),
    q90NPointDistrib = rep(NA_real_, nrow(conditions)),
    maxPctPointDistrib = rep(NA_real_, nrow(conditions)),
    q95PctPointDistrib = rep(NA_real_, nrow(conditions)),
    q90PctPointDistrib = rep(NA_real_, nrow(conditions)),
    maxNZeroCells = rep(NA_real_, nrow(conditions)),
    q95NZeroCells = rep(NA_real_, nrow(conditions)),
    q90NZeroCells = rep(NA_real_, nrow(conditions)),
    maxPctZeroCells = rep(NA_real_, nrow(conditions)),
    q95PctZeroCells = rep(NA_real_, nrow(conditions)),
    q90PctZeroCells = rep(NA_real_, nrow(conditions)))
  pb <- utils::txtProgressBar(min = 0, max = nrow(conditions) * nIterPerCond,
                              style = 3)
  for (j in seq_len(nrow(conditions))) {
    conditionSummary <-
      data.frame(nPointDistrib = rep(NA_integer_, nIterPerCond),
                 nZeroCells = rep(NA_integer_, nIterPerCond),
                 pctPointDistrib = rep(NA_integer_, nIterPerCond),
                 pctZeroCells = rep(NA_integer_, nIterPerCond))
    for (i in seq_len(nIterPerCond)) {
      data <- do.call(generate_data, c(pCGY = list(coverageScheme),
                                       as.list(conditions[j, ])))
      distributions <- aggregate_data(data$responses, "distributions")
      distributions$nNonZero <-
        rowSums(!is.na(distributions[, grep("^n\\.[[:digit:]]+$",
                                            names(distributions))]))
      distributions$pointDistrib <- distributions$nNonZero == 0
      distributions$zeroCells <-
        distributions$respScaleLength > distributions$nNonZero
      distributions$nOver9 <-
        rowSums(distributions[, grep("^n\\.[[:digit:]]+$",
                                     names(distributions))] > 9, na.rm = TRUE)
      distributions$cellsBelow10 <-
        distributions$respScaleLength > distributions$nOver9

      conditionSummary[i, c("nPointDistrib", "nZeroCells")] <-
        colSums(distributions[, c("pointDistrib", "zeroCells")])
      conditionSummary[i, c("pctPointDistrib", "pctZeroCells")] <-
        100 * conditionSummary[i, c("nPointDistrib",
                                    "nZeroCells")] / nrow(distributions)
      utils::setTxtProgressBar(pb, utils::getTxtProgressBar(pb) + 1)
    }
    results$maxNPointDistrib[j] <- max(conditionSummary$nPointDistrib)
    results$q95NPointDistrib[j] <-
      stats::quantile(conditionSummary$nPointDistrib, 0.95)
    results$q90NPointDistrib[j] <-
      stats::quantile(conditionSummary$nPointDistrib, 0.9)
    results$maxPctPointDistrib[j] <- max(conditionSummary$pctPointDistrib)
    results$q95PctPointDistrib[j] <-
      stats::quantile(conditionSummary$pctPointDistrib, 0.95)
    results$q90PctPointDistrib[j] <-
      stats::quantile(conditionSummary$pctPointDistrib, 0.9)
    results$maxNZeroCells[j] <- max(conditionSummary$nZeroCells)
    results$q95NZeroCells[j] <-
      stats::quantile(conditionSummary$nZeroCells, 0.95)
    results$q90NZeroCells[j] <-
      stats::quantile(conditionSummary$nZeroCells, 0.9)
    results$maxPctZeroCells[j] <- max(conditionSummary$pctZeroCells)
    results$q95PctZeroCells[j] <-
      stats::quantile(conditionSummary$pctZeroCells, 0.95)
    results$q90PctZeroCells[j] <-
      stats::quantile(conditionSummary$pctZeroCells, 0.9)
  }
  close(pb)
  return(cbind(conditions, results))
}
