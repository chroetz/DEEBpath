#' @export
getOpenTruthNrs <- function(
    dbPath,
    truthNrFilter,
    obsNr,
    model,
    methodFile,
    expansionNr = NULL
) {
  paths <- getPaths(dbPath, model)
  if (is.null(expansionNr)) {
    methodName <- methodFile
  } else {
    hyperParmsPath <- getMethodFile(dbPath, methodFile)
    hyperParmsList <- ConfigOpts::readOptsBare(hyperParmsPath)
    if (nchar(hyperParmsList$name) == 0) hyperParmsList$name <- basename(methodFile)
    hyperParmsList <- ConfigOpts::expandList(hyperParmsList)
    hyperParms <- hyperParmsList$list[[expansionNr]]
    methodName <- nameWithHash(hyperParmsList$name, hyperParms)
  }
  methodEstiPath <- file.path(paths$esti, methodName)
  if (!dir.exists(methodEstiPath)) return(truthNrFilter)
  meta <- getMetaGeneric(
    methodEstiPath,
    tagFileFilter = list(c("truth", "obs", "task", "esti")))
  if (NROW(meta) == 0) return(truthNrFilter)
  info <- meta |> filter(.data$obsNr == .env$obsNr)
  if (NROW(info) == 0) return(truthNrFilter)
  setdiff(truthNrFilter, info$truthNr)
}

