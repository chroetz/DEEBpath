#' @export
getModels <- function(dbPath, modelPattern=NULL) {
  models <-
    list.dirs(dbPath, full.names = FALSE, recursive=FALSE) |>
    str_subset("^(_|.)", negate=TRUE)
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
  file.path(hyperDir(dbPath), paste0(method, ".json"))
}

#' @export
getMethodPaths <- function(methodOptsDir) {
  paths <- list.files(methodOptsDir, pattern = "\\.json$")
  sel <- sapply(paths, isHyperParmFile) | sapply(paths, isHyperParmListFile)
  return(normalizePath(paths[sel]))
}

#' @export
isHyperParmFile <- function(path) {
  opts <- tryCatch(
    ConfigOpts::readOptsBare(path),
    error = \(cond) FALSE)
  if (!ConfigOpts::isOpts(opts)) return(FALSE)
  return(ConfigOpts::getClassAt(opts, 1) == "HyperParms")
}


#' @export
isHyperParmListFile <- function(path) {
  opts <- tryCatch(
    ConfigOpts::readOptsBare(path),
    error = \(cond) FALSE)
  if (!ConfigOpts::isOpts(opts)) return(FALSE)
  if (!ConfigOpts::getClassAt(opts, 1) == "List") return(FALSE)
  return(ConfigOpts::getClassAt(opts, 2) == "HyperParms")
}

