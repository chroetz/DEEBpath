#' @export
getObsNrFromName <- function(dbPath, model, obsName) {
  obsNames <- getObsNamesOfModel(dbPath, model)
  nr <- which(obsName == obsNames)
  stopifnot(length(nr) == 1)
  return(nr)
}

#' @export
getObsNameFromNr <- function(dbPath, model, obsNr) {
  if (length(dbPath) == 0 || length(model) == 0) return(character())
  stopifnot(length(unique(dbPath)) == 1)
  stopifnot(length(unique(model)) == 1)
  dbPath <- dbPath[[1]]
  model <- model[[1]]
  obsNames <- getObsNamesOfModel(dbPath, model)
  return(obsNames[obsNr])
}


#' @export
getObsNamesOfModel <- function(dbPath, model) {
  runOptsPath <- getPaths(dbPath, model)$runOpts
  if (file.exists(runOptsPath)) {
    runOpts <- ConfigOpts::readOptsBare(runOptsPath)
    sapply(runOpts$observation$list, \(entry) entry$name)
  } else {
    modelPath <- file.path(dbPath, model)
    obsFiles <- list.files(
      file.path(modelPath, "observation"),
      pattern = "truth\\d+obs\\d+\\.csv",
      full.names = FALSE, recursive = FALSE)
    obsNrs <-
      obsFiles |>
      str_extract("(?<=obs)(\\d+)") |>
      as.integer() |>
      unique()
    as.character(obsNrs)
  }
}

#' @export
getObsNames <- function(dbPath) {
  models <- getModels(dbPath)
  obsNames <- lapply(models, getObsNamesOfModel, dbPath=dbPath)
  names(obsNames) <- models
  return(obsNames)
}
