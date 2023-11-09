#' @export
estiFile <- function(info=NULL, truthNr=NULL, obsNr=NULL, taskNr=NULL, ending = TRUE) {
  info <- as.list(info)
  if (is.null(truthNr)) truthNr <- info$truthNr
  if (is.null(obsNr)) obsNr <- info$obsNr
  if (is.null(taskNr)) taskNr <- info$taskNr
  stopifnot(!is.null(truthNr))
  stopifnot(!is.null(obsNr))
  stopifnot(!is.null(taskNr))
  n <- max(length(truthNr), length(obsNr), length(taskNr))
  stopifnot(length(truthNr) == n || length(truthNr) == 1)
  stopifnot(length(obsNr) == n || length(obsNr) == 1)
  stopifnot(length(taskNr) == n || length(taskNr) == 1)
  truthNr <- as.integer(truthNr)
  obsNr <- as.integer(obsNr)
  taskNr <- as.integer(taskNr)
  stopifnot(!any(is.na(truthNr)))
  stopifnot(!any(is.na(obsNr)))
  stopifnot(!any(is.na(taskNr)))
  fileName <- sprintf("truth%04dobs%04dtask%02desti", truthNr, obsNr, taskNr)
  if (isTRUE(ending)) {
    fileName <- paste0(fileName, ".csv")
  } else if (is.character(ending)) {
    fileName <- paste0(fileName, ending)
  }
  return(fileName)
}

#' @export
obsFile <- function(info=NULL, truthNr=NULL, obsNr=NULL, ending = TRUE) {
  info <- as.list(info)
  if (is.null(truthNr)) truthNr <- info$truthNr
  if (is.null(obsNr)) obsNr <- info$obsNr
  stopifnot(!is.null(truthNr))
  stopifnot(!is.null(obsNr))
  n <- max(length(truthNr), length(obsNr))
  stopifnot(length(truthNr) == n || length(truthNr) == 1)
  stopifnot(length(obsNr) == n || length(obsNr) == 1)
  truthNr <- as.integer(truthNr)
  obsNr <- as.integer(obsNr)
  stopifnot(!any(is.na(truthNr)))
  stopifnot(!any(is.na(obsNr)))
  fileName <- sprintf("truth%04dobs%04d", truthNr, obsNr)
  if (isTRUE(ending)) {
    fileName <- paste0(fileName, ".csv")
  } else if (is.character(ending)) {
    fileName <- paste0(fileName, ending)
  }
  return(fileName)
}

#' @export
parmsFile <- function(info=NULL, truthNr=NULL, ending = FALSE) {
  info <- as.list(info)
  if (is.null(truthNr)) truthNr <- info$truthNr
  stopifnot(!is.null(truthNr))
  truthNr <- as.integer(truthNr)
  stopifnot(!any(is.na(truthNr)))
  fileName <- sprintf("truth%04dparms", truthNr)
  if (isTRUE(ending)) {
    fileName <- paste0(fileName, ".json")
  } else if (is.character(ending)) {
    fileName <- paste0(fileName, ending)
  }
  return(fileName)
}

#' @export
obsTruthFile <- function(info=NULL, truthNr=NULL, obsNr=NULL, ending = TRUE) {

  info <- as.list(info)

  if (is.null(truthNr)) truthNr <- info$truthNr
  stopifnot(!is.null(truthNr))
  truthNr <- as.integer(truthNr)
  stopifnot(!any(is.na(obsNr)))

  if (is.null(obsNr)) obsNr <- info$obsNr
  stopifnot(!is.null(obsNr))
  obsNr <- as.integer(obsNr)
  stopifnot(!any(is.na(obsNr)))

  fileName <- sprintf("obs%04dtruth%04d", obsNr, truthNr)
  if (isTRUE(ending)) {
    fileName <- paste0(fileName, ".csv")
  } else if (is.character(ending)) {
    fileName <- paste0(fileName, ending)
  }

  return(fileName)
}


#' @export
truthFile <- function(info=NULL, truthNr=NULL, ending = TRUE) {
  info <- as.list(info)
  if (is.null(truthNr)) truthNr <- info$truthNr
  stopifnot(!is.null(truthNr))
  truthNr <- as.integer(truthNr)
  stopifnot(!any(is.na(truthNr)))
  fileName <- sprintf("truth%04d", truthNr)
  if (isTRUE(ending)) {
    fileName <- paste0(fileName, ".rds")
  } else if (is.character(ending)) {
    fileName <- paste0(fileName, ending)
  }
  return(fileName)
}


#' @export
taskTruthFile <- function(info=NULL, truthNr=NULL, taskNr=NULL, ending = TRUE) {
  info <- as.list(info)
  if (is.null(truthNr)) truthNr <- info$truthNr
  if (is.null(taskNr)) taskNr <- info$taskNr
  stopifnot(!is.null(truthNr))
  stopifnot(!is.null(taskNr))
  n <- max(length(truthNr), length(taskNr))
  stopifnot(length(truthNr) == n || length(truthNr) == 1)
  stopifnot(length(taskNr) == n || length(taskNr) == 1)
  truthNr <- as.integer(truthNr)
  taskNr <- as.integer(taskNr)
  stopifnot(!any(is.na(truthNr)))
  stopifnot(!any(is.na(taskNr)))
  fileName <- sprintf("task%02dtruth%04d", taskNr, truthNr)
  if (isTRUE(ending)) {
    fileName <- paste0(fileName, ".csv")
  } else if (is.character(ending)) {
    fileName <- paste0(fileName, ending)
  }
  return(fileName)
}

#' @export
taskFile <- function(info=NULL, taskNr=NULL, ending = FALSE) {
  info <- as.list(info)
  if (is.null(taskNr)) taskNr <- info$taskNr
  stopifnot(!is.null(taskNr))
  taskNr <- as.integer(taskNr)
  stopifnot(!any(is.na(taskNr)))
  fileName <- sprintf("task%02d", taskNr)
  if (isTRUE(ending)) {
    fileName <- paste0(fileName, ".json")
  } else if (is.character(ending)) {
    fileName <- paste0(fileName, ending)
  }
  return(fileName)
}

#' @export
evalDataFile <- function(info=NULL, taskNr=NULL, method = NULL, ending = TRUE) {
  info <- as.list(info)
  if (is.null(taskNr)) taskNr <- info$taskNr
  if (is.null(method)) method <- info$method
  stopifnot(!is.null(taskNr))
  stopifnot(!is.null(method))
  n <- max(length(taskNr), length(method))
  stopifnot(length(taskNr) == n || length(taskNr) == 1)
  stopifnot(length(method) == n || length(method) == 1)
  taskNr <- as.integer(taskNr)
  method <- as.character(method)
  stopifnot(!any(is.na(taskNr)))
  stopifnot(!any(is.na(method)))
  fileName <- sprintf("task%02d%s_eval", taskNr, method)
  if (isTRUE(ending)) {
    fileName <- paste0(fileName, ".csv")
  } else if (is.character(ending)) {
    fileName <- paste0(fileName, ending)
  }
  return(fileName)
}

#' @export
getScoreFiles <- function(path) {
  dir(path, "task\\d+.*_eval.csv", full.names = TRUE)
}

#' @export
hyperParmsFile <- function(info=NULL, truthNr=NULL, obsNr=NULL, ending = FALSE) {
  info <- as.list(info)
  if (is.null(truthNr)) truthNr <- info$truthNr
  if (is.null(obsNr)) obsNr <- info$obsNr
  stopifnot(!is.null(truthNr))
  stopifnot(!is.null(obsNr))
  n <- max(length(truthNr), length(obsNr))
  stopifnot(length(truthNr) == n || length(truthNr) == 1)
  stopifnot(length(obsNr) == n || length(obsNr) == 1)
  truthNr <- as.integer(truthNr)
  obsNr <- as.integer(obsNr)
  stopifnot(!any(is.na(truthNr)))
  stopifnot(!any(is.na(obsNr)))
  fileName <- sprintf("truth%04dobs%04dhyperParms", truthNr, obsNr)
  if (isTRUE(ending)) {
    fileName <- paste0(fileName, ".json")
  } else if (is.character(ending)) {
    fileName <- paste0(fileName, ending)
  }
  return(fileName)
}

#' @export
smoothFile <- function(info=NULL, truthNr=NULL, obsNr=NULL, ending = TRUE) {
  info <- as.list(info)
  if (is.null(truthNr)) truthNr <- info$truthNr
  if (is.null(obsNr)) obsNr <- info$obsNr
  stopifnot(!is.null(truthNr))
  stopifnot(!is.null(obsNr))
  n <- max(length(truthNr), length(obsNr))
  stopifnot(length(truthNr) == n || length(truthNr) == 1)
  stopifnot(length(obsNr) == n || length(obsNr) == 1)
  truthNr <- as.integer(truthNr)
  obsNr <- as.integer(obsNr)
  stopifnot(!any(is.na(truthNr)))
  stopifnot(!any(is.na(obsNr)))
  fileName <- sprintf("truth%04dobs%04dsmooth", truthNr, obsNr)
  if (isTRUE(ending)) {
    fileName <- paste0(fileName, ".csv")
  } else if (is.character(ending)) {
    fileName <- paste0(fileName, ending)
  }
  return(fileName)
}

#' @export
validationErrorFile <- function(info=NULL, truthNr=NULL, obsNr=NULL, ending = TRUE) {
  info <- as.list(info)
  if (is.null(truthNr)) truthNr <- info$truthNr
  if (is.null(obsNr)) obsNr <- info$obsNr
  stopifnot(!is.null(truthNr))
  stopifnot(!is.null(obsNr))
  n <- max(length(truthNr), length(obsNr))
  stopifnot(length(truthNr) == n || length(truthNr) == 1)
  stopifnot(length(obsNr) == n || length(obsNr) == 1)
  truthNr <- as.integer(truthNr)
  obsNr <- as.integer(obsNr)
  stopifnot(!any(is.na(truthNr)))
  stopifnot(!any(is.na(obsNr)))
  fileName <- sprintf("truth%04dobs%04dvalidationError", truthNr, obsNr)
  if (isTRUE(ending)) {
    fileName <- paste0(fileName, ".csv")
  } else if (is.character(ending)) {
    fileName <- paste0(fileName, ending)
  }
  return(fileName)
}


#' @export
plotsDir <- function(info=NULL, dbPath=NULL, example=NULL, model=NULL) {
  info <- as.list(info)
  if (is.null(dbPath)) dbPath <- info$dbPath
  if (is.null(example)) example <- info$example
  if (is.null(model)) model <- info$model
  stopifnot(length(dbPath) == 1)
  stopifnot(length(example) == 1)
  stopifnot(length(model) == 1)
  if (example) {
    modelPath <- file.path(dbPath, model, "example")
  } else {
    modelPath <- file.path(dbPath, model)
  }
  dirPath <- file.path(modelPath, "evaluation", "plots")
  return(dirPath)
}
