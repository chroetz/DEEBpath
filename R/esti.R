#' @export
getModels <- function(dbPath, modelPattern=NULL) {
  models <-
    list.dirs(dbPath, full.names = FALSE, recursive=FALSE) |>
    str_subset("^(_|\\.)", negate=TRUE)
  if (!is.null(modelPattern)) {
    models <- grep(modelPattern, models, value=TRUE)
  }
  return(models)
}


#' @export
getMethods <- function(dbPath, models = NULL, methodPattern=NULL) {
  if (is.null(models)) models <- getModels(dbPath)
  tbl <-
    lapply(models, \(model) {
      paths <- getPaths(dbPath, model)
      methodDirPaths <- list.dirs(paths$esti, recursive=FALSE, full.names=TRUE)
      methods <- basename(methodDirPaths)
      if (!is.null(methodPattern)) {
        sel <- str_detect(methods, methodPattern)
        methods <- methods[sel]
        methodDirPaths <- methodDirPaths[sel]
      }
      tibble(model = model, method = methods, path = methodDirPaths)
    }) |>
    bind_rows()
  return(tbl)
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

