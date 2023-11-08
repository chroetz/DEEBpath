#' @export
getModels <- function(dbPath, modelPattern=NULL) {
  list.dirs(dbPath, full.names = FALSE, recursive=FALSE)
  if (!is.null(modelPattern)) {
    models <- grep(modelPattern, models, value=TRUE)
  }
  return(models)
}

#' @export
getObservationNrs <- function(dbPath, model) {
  paths <- getPaths(dbPath, model)
  meta <- getMetaGeneric(paths$obs)
  obsNrs <- meta$obsNr |> unique()
  return(obsNrs)
}

#' @export
getMethodOptsDirSpecific <- function(methodOptsDir, model, obsNr) {
  file.path(methodOptsDir, model, sprintf("obs%04d", obsNr))
}

#' @export
getEstiOptsPath <- function(methodOptsDirSpecific) {
  file.path(methodOptsDirSpecific, "Opts_Estimation.json")
}

#' @export
getHyperParmsPaths <- function(methodOptsDirSpecific, methodPattern = NULL) {
  hpFiles <- list.files(methodOptsDirSpecific, pattern = "^Opts_List_HyperParms_.*\\.json$")
  if (!is.null(methodPattern)){
    hpFiles <- grep(methodPattern, hpFiles, value=TRUE)
  }
  return(hpFiles)
}

