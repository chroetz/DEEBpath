#' @export
getOpenTruthNrs <- function(
    dbPath,
    truthNrFilter = NULL,
    obsNr,
    model,
    methodName
) {
  paths <- getPaths(dbPath, model)
  methodEstiPath <- file.path(paths$esti, methodName)
  if (is.null(truthNrFilter)) {
    truthNrFilter <- getUniqueTruthNrs(dbPath, model)
  }
  if (!dir.exists(methodEstiPath)) return(truthNrFilter)
  meta <- getMetaGeneric(
    methodEstiPath,
    tagFileFilter = list(c("truth", "obs", "task", "esti")))
  if (NROW(meta) == 0) return(truthNrFilter)
  info <- meta |> filter(.data$obsNr == .env$obsNr)
  if (NROW(info) == 0) return(truthNrFilter)
  setdiff(truthNrFilter, info$truthNr)
}
