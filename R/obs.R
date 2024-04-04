#' @export
getObsNrFromName <- function(dbPath, model, obsName) {
  obsNames <- getObsNamesOfModel(dbPath, model)
  nr <- which(obsName == obsNames)
  stopifnot(length(nr) == 1)
  return(nr)
}

#' @export
getObsNameFromNr <- function(dbPath, model, obsNr) {
  stopifnot(length(unique(dbPath)) == 1)
  stopifnot(length(unique(model)) == 1)
  dbPath <- dbPath[[1]]
  model <- model[[1]]
  obsNames <- getObsNamesOfModel(dbPath, model)
  return(obsNames[obsNr])
}


#' @export
getObsNamesOfModel <- function(dbPath, model) {
  runOpts <- ConfigOpts::readOptsBare(getPaths(dbPath, model)$runOpts)
  sapply(runOpts$observation$list, \(entry) entry$name)
}

#' @export
getObsNames <- function(dbPath) {
  models <- getModels(dbPath)
  obsNames <- lapply(models, getObsNamesOfModel, dbPath=dbPath)
  names(obsNames) <- models
  return(obsNames)
}
