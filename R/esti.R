#' @export
getModels <- function(dbPath, modelPattern=NULL) {
  models <- list.dirs(dbPath, full.names = FALSE, recursive=FALSE)
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
getMethodOptsDirSpecific <- function(methodOptsDir, model, obsNr) {
  file.path(methodOptsDir, sprintf("obs%04d", obsNr), model)
}

#' @export
getEstiOptsPath <- function(methodOptsDirSpecific) {
  file.path(methodOptsDirSpecific, "Opts_Estimation.json")
}

#' @export
getHyperParmsFiles <- function(methodOptsDirSpecific, methodPattern = NULL) {
  hpFiles <- list.files(methodOptsDirSpecific, pattern = "^Opts_List_HyperParms_.*\\.json$")
  if (!is.null(methodPattern)){
    hpFiles <- grep(methodPattern, hpFiles, value=TRUE)
  }
  return(hpFiles)
}


#' @export
getHyperParmsPath <- function(methodOptsDirSpecific, method) {
  path <- file.path(methodOptsDirSpecific, sprintf("Opts_List_HyperParms_%s.json", method))
  return(path)
}

