#' @title Postprocessing of the simulation results
#' @description
#' Enables to bind simulation results saved in different .RData files into
#' one set of data frames.
#' @param files a character vector with names of the .RData files storing
#' results which should be binded
#' @param saveFile optionally a string with a name of the .RData file to which
#' results should be written
#' @param verbose logical value indicating whether the progress of data
#' processing should be indicated using a text progress bar
#' @details
#' See [run_iteration]
#' @returns (invisibly) a list of five data frames:
#' \describe{
#'   \item{conditions}{sets of parameter values set in specific simulation
#'                     conditions}
#'   \item{modelSummaries}{basic model summary statistics in a *long* format}
#'   \item{countryMeans}{generated country-year means (as returned by
#'                       [generate_data], i.e. not standardized) and their
#'                       estimates from the models (standardized within the
#'                       group of observed country-means)}
#'   \item{items}{item parameters (generated, not estimated)}
#'   \item{itemDistributions}{distributions (counts) of responses to items for
#'                            each project-country-year-item}
#' }
#' Comparing to the results  returned by [run_simulation]: 1) a data frame
#' describing simulation conditions is also returned 2) specific conditions
#' are referenced using the `idConditon` variable, which is present also in the
#' data frame describing simulation conditions 3) specific iterations are
#' referenced using the `idIter` variable.
#'
#' Moreover, if a name of file is provided using the `saveFile` argument,
#' function will write the aforementioned set of data frames into a specified
#' file using the .RData format. Please note that they will be writen as
#' separate data frame, not the list of them.
#' `idCondition`
#' @examples
#' \dontrun{
#' files <- list.files(path = ".", pattern = "\\.RData$",
#'                     recursive = TRUE, full.names = TRUE)
#' results <- bind_results(files)
#' with(results, save(list = ls(), file = "allResultds.RData"))
#' # or simply
#' gather_results(files, "allResultds.RData")
#' }
#' @export
#' @importFrom dplyr .data %>%
bind_results <- function(files, saveFile = NULL, verbose = TRUE) {
  stopifnot(is.character(files), length(files) > 0, !anyNA(files),
            is.null(saveFile) | is.character(saveFile),
            is.logical(verbose), length(verbose) == 1, !anyNA(verbose))
  if (!is.null(saveFile)) {
    stopifnot(length(saveFile) == 1, !anyNA(saveFile))
  }
  checkAcces <- file.access(files, mode = 4)
  if (any(checkAcces != 0)) {
    stop("The following files do not exist (or perhaps you don't have permissions to read them):\n",
         paste(files[checkAcces != 0], collapse = ", "))
  }

  conditionsAll <- countryMeansAll <- itemDistributionsAll <- itemsAll <-
    modelSummariesAll <- data.frame()
  objects <- c("conditions", "countryMeans", "itemDistributions", "items",
               "modelSummaries")
  maxIdConditionFile <- 0L

  if (verbose) pb <- utils::txtProgressBar(0, length(files), style = 3)
  for (f in files) {
    objectsInFile <- load(f)
    stopifnot(all(objectsInFile %in% objects))
    conditions <- get("conditions")
    countryMeans <- get("countryMeans")
    itemDistributions <- get("itemDistributions")
    items <- get("items")
    modelSummaries <- get("modelSummaries")

    conditions$cond = seq_len(nrow(conditions))
    conditions$idConditionFile = maxIdConditionFile + conditions$cond
    maxIdConditionFile <- max(conditions$idConditionFile)
    countryMeans$idConditionFile <- conditions$idConditionFile[countryMeans$cond]
    names(itemDistributions)[names(itemDistributions) == "j"] <- "cond"
    itemDistributions$idConditionFile <-
      conditions$idConditionFile[itemDistributions$cond]
    items$idConditionFile <- conditions$idConditionFile[items$cond]
    modelSummaries$idConditionFile <-
      conditions$idConditionFile[modelSummaries$cond]
    for (o in objects) {
      assign(paste0(o, "All"), dplyr::bind_rows(get(paste0(o, "All")), get(o)))
    }
    rm(list = objects)
    if (verbose) utils::setTxtProgressBar(pb, utils::getTxtProgressBar(pb) + 1L)
  }
  if (verbose) close(pb)
  for (o in objects) {
    assign(o, get(paste0(o, "All")))
  }
  rm(list = paste0(objects, "All"))

  uniqueConditions <- conditions %>%
    dplyr::select(-c("cond", "idConditionFile")) %>%
    dplyr::distinct() %>%
    dplyr::mutate(idCondition = seq_len(dplyr::n()))
  conditionsMapping <- uniqueConditions %>%
    dplyr::inner_join(conditions,
                      by = intersect(names(uniqueConditions),
                                     names(conditions))) %>%
    dplyr::select("idConditionFile", "idCondition")
  conditions <- uniqueConditions %>%
    dplyr::select("idCondition", dplyr::everything())

  modelSummaries <- modelSummaries %>%
    dplyr::inner_join(conditionsMapping,
                      by = "idConditionFile") %>%
    dplyr::group_by(.data$idCondition) %>%
    dplyr::mutate(idIter = as.integer(interaction(.data$idConditionFile,
                                                  .data$i))) %>%
    dplyr::ungroup()
  iterationsMapping <- modelSummaries %>%
    dplyr::select("idConditionFile", "i", "idCondition", 'idIter') %>%
    dplyr::distinct()
  modelSummaries <- modelSummaries %>%
    dplyr::select("idCondition", "idIter", dplyr::everything(),
                  -c("idConditionFile", "i", "cond"))

  items <- items %>%
    dplyr::inner_join(iterationsMapping,
                      by = c("idConditionFile", "i")) %>%
    dplyr::select("idCondition", "idIter", dplyr::everything(),
                  -c("idConditionFile", "i", "cond"))
  itemDistributions <- itemDistributions %>%
    dplyr::inner_join(iterationsMapping,
               by = c("idConditionFile", "i")) %>%
    dplyr::select("idCondition", "idIter", dplyr::everything(),
                  -c("idConditionFile", "i", "cond"))
  countryMeans <- countryMeans %>%
    dplyr::inner_join(iterationsMapping,
                      by = c("idConditionFile", "i")) %>%
    dplyr::select("idCondition", "idIter", dplyr::everything(),
                  -c("idConditionFile", "i", "cond"))

  if (!is.null(saveFile)) {
    save(list = objects,
         file = paste0(sub("\\.([Rr][Dd][Aa][Tt][Aa]|[Rr][Dd][Tt])$", "",
                           saveFile), ".RData"))
  }
  invisible(list(conditions = conditions,
                 modelSummaries = modelSummaries,
                 countryMeans = countryMeans,
                 items = items,
                 itemDistributions = itemDistributions))
}
