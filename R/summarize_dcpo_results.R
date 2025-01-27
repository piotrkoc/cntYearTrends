#' @title Model estimation
#' @description
#' Gets country mean parameters out of the fitted DCPO model.
#' @param input an object returned by [DCPOtools::format_dcpo]
#' @param output an object of the *CmdStanFit* class
#' @importFrom dplyr .data %>%
summarize_dcpo_results <- function(input, output) {
  stopifnot(inherits(output, "CmdStanFit"))
  kcodes <- input$data %>%
    dplyr::group_by(.data$country) %>%
    dplyr::summarize(kk = as.numeric(dplyr::first(.data$kk)))
  tcodes <- input$data %>%
    dplyr::group_by(.data$year) %>%
    dplyr::summarize(tt = dplyr::first(.data$tt))
  ktcodes <- input$data %>%
    dplyr::group_by(.data$country) %>%
    dplyr::summarize(first_yr = min(.data$year),
                     last_yr = max(.data$year))

  variableNames <- dimnames(output$draws("raw_theta"))$variable
  thetaOut <- apply(output$draws("raw_theta"), "variable", as.vector)
  thetaPE <- apply(thetaOut, "variable", mean)
  thetaU95 <- apply(thetaOut, "variable", stats::quantile, probs = c(0.975))
  thetaL95 <- apply(thetaOut, "variable", stats::quantile, probs = c(0.025))
  thetaSD <- apply(thetaOut, "variable", stats::sd)

  data.frame(variable = variableNames,
             q05 = thetaL95, q95 = thetaU95,
             mean = thetaPE, sd = thetaSD) %>%
    dplyr::mutate(tt = as.numeric(gsub("raw_theta\\[(\\d+),\\d+\\]",
                                       "\\1",
                                       .data$variable)),
                  kk = as.numeric(gsub("raw_theta\\[\\d+,(\\d+)\\]",
                                       "\\1",
                                       .data$variable))) %>%
    dplyr::left_join(kcodes, by = "kk") %>%
    dplyr::left_join(tcodes, by = "tt") %>%
    dplyr::mutate(year =
                    dplyr::if_else(.data$tt == 1,
                                   as.integer(.data$year),
                                   as.integer(min(.data$year,
                                                  na.rm = TRUE) +
                                                .data$tt - 1))) %>%
    dplyr::left_join(ktcodes, by = "country") %>%
    dplyr::filter(.data$year >= .data$first_yr,
                  .data$year <= .data$last_yr) %>%
    dplyr::arrange(.data$kk, .data$tt) %>%
    dplyr::select("country", "year",
                  mean_dcpo = "mean", sd_dcpo = "sd",
                  q05_dcpo = "q05",  q95_dcpo = "q95") %>%
    return()
}
