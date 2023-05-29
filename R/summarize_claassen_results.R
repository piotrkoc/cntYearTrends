#' @title Model estimation
#' @description
#' Gets country mean parameters out of the fitted Claassen's model.
#' @param input an object returned by [format_claassen]
#' @param output an object of the *CmdStanFit* class
#' @importFrom dplyr .data
summarize_claassen_results <- function(input, output) {
  stopifnot(inherits(output, "CmdStanFit"))

  thetaOut <- apply(output$draws("theta"), "variable", as.vector)
  thetaMean = mean(thetaOut)
  thetaSD = stats::sd(thetaOut)
  thetaStd <- (thetaOut - thetaMean) / thetaSD # standardize
  thetaPE <- apply(thetaStd, "variable", mean)
  thetaU95 <- apply(thetaStd, "variable", stats::quantile, probs = c(0.975))
  thetaL95 <- apply(thetaStd, "variable", stats::quantile, probs = c(0.025))
  thetaSD <- apply(thetaStd, "variable", stats::sd)

  return(data.frame(country = input$cnt.names[input$r.map$Cntry],
                    year = as.integer(input$r.map$Yr),
                    mean_claassen = thetaPE, sd_claassen = thetaSD,
                    q05_claassen = thetaL95,  q95_claassen = thetaU95))
}
