#' @export
getPaths <- function(dbPath, model) {
  modelPath <- file.path(dbPath, model)
  list(
    truth = file.path(modelPath, "truth"),
    esti = file.path(modelPath, "estimation"),
    eval = file.path(modelPath, "evaluation"),
    plots = file.path(modelPath, "evaluation", "plots"),
    task = file.path(modelPath, "task"),
    obs = file.path(modelPath, "observation"),
    runOpts = file.path(modelPath, "Opts_Run.json")
  )
}

#' @export
summaryDir <- function(dbPath) {
  file.path(dbPath, "_summary")
}

#' @export
summaryTablePath <- function(dbPath) {
  file.path(summaryDir(dbPath), "scores.csv")
}

#' @export
summaryHyperPath <- function(dbPath, model, methodBase) {
  file.path(summaryDir(dbPath), paste0(model, "_", methodBase, ".csv"))
}


#' @export
getRanMethodOptsPath <- function(dbPath, model, method) {
  paths <- getPaths(dbPath, model)
  dirPath <- file.path(paths$esti, method)
  optsFilePaths <- list.files(dirPath, "^Opts_HyperParms.*\\.json$", full.names=TRUE)
  return(optsFilePaths)
}

#' @export
getLogDir <- function(dbPath = NULL, relative = FALSE) {
  stopifnot(is.logical(relative))
  stopifnot(length(relative) == 1)
  if (relative) {
    return("_log")
  }
  stopifnot(is.character(dbPath))
  stopifnot(length(dbPath) == 1)
  normalizePath(file.path(dbPath, "_log"), mustWork = FALSE)
}
