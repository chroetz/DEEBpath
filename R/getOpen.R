#' @export
getOpenTruthNrs <- function(
    dbPath,
    truthNrFilter,
    obsNr,
    model,
    method,
    expansionNr = NULL
) {
  paths <- getPaths(dbPath, model)
  if (is.null(expansionNr)) {
    methodName <- method
  } else {
    hyperParmsPath <- DEEBpath::getMethodFile(dbPath, method)
    hyperParmsList <- ConfigOpts::readOptsBare(hyperParmsPath)
    hyperParmsList <- ConfigOpts::expandList(hyperParmsList)
    hyperParms <- hyperParmsList$list[[expansionNr]]
    methodName <- DEEBpath::nameWithHash(hyperParmsList$name, hyperParms)
  }
  methodEstiPath <- file.path(paths$esti, methodName)
  if (!dir.exists(methodEstiPath)) return(truthNrFilter)
  meta <- getMetaGeneric(
    methodEstiPath,
    tagFileFilter = list(c("truth", "obs", "task", "esti")))
  if (NROW(meta) == 0) return(truthNrFilter)
  info <- meta |> dplyr::filter(.data$obsNr == .env$obsNr)
  if (NROW(info) == 0) return(truthNrFilter)
  setdiff(truthNrFilter, info$truthNr)
}

