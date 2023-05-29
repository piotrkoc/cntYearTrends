#' @title Model estimation
#' @description
#' Returns a list of Stan models that can be used for estimation.
#' @param which optionally a character vector specifying which models should be
#' returned (Claassen's, Solt's or both)
#' @returns a list of *CmdStanModel* objects
#' @export
prepare_stan_models <- function(which = c("claassen", "dcpo")) {
  stopifnot(all(which %in% c("claassen", "dcpo")))
  models <- vector(mode = "list", length = 0L)
  if ("claassen" %in% which) {
    models$claassen <-
      cmdstanr::cmdstan_model(system.file("claassen.stan",
                                          package = "cntYearTrends",
                                          mustWork = TRUE))
  }
  if ("dcpo" %in% which) {
    models$dcpo <-
      cmdstanr::cmdstan_model(system.file("dcpo.stan",
                                                       package = "cntYearTrends",
                                                       mustWork = TRUE))
  }
  return(models)
}
