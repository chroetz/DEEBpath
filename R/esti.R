#' @export
getModels <- function(dbPath, modelPattern=NULL) {
  models <-
    list.dirs(dbPath, full.names = FALSE, recursive=FALSE) |>
    stringr::str_subset("^_", negate=TRUE)
  if (!is.null(modelPattern)) {
    models <- grep(modelPattern, models, value=TRUE)
  }
  return(models)
}

#' @export
getObservationNrs <- function(dbPath, model, obsNrFilter = NULL) {
  paths <- getPaths(dbPath, model)
  meta <- getMetaGeneric(paths$obs)
  obsNrs <- meta$obsNr |> unique()
  if (!is.null(obsNrFilter)) {
    obsNrs <- intersect(obsNrs, obsNrFilter)
  }
  return(obsNrs)
}

#' @export
getMethodFile <- function(dbPath, method) {
  file.path(dbPath, "_hyper", paste0(method, ".json"))
}

#' @export
getEstiOptsPath <- function(dbPath, model, fileName=NULL) {
  if (is.null(fileName))
    file.path(dbPath, "_hyper", "Opts_Estimation.json")
  else
    file.path(dbPath, "_hyper", fileName)
}
