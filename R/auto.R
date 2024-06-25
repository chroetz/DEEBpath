#' @export
newAutoId <- function(dbPath) {
  autoBasePath <- autoDir(dbPath)
  autoPath <-
    autoBasePath |>
    tempfile(pattern = "", tmpdir = _) |>
    normalizePath(winslash = "/", mustWork=FALSE)
  dir.create(autoPath, recursive=TRUE)
  return(basename(autoPath))
}


#' @export
autoDir <- function(dbPath) {
  file.path(dbPath, "_hyper", "auto")
}


#' @export
autoIdDir <- function(dbPath, autoId) {
  file.path(autoDir(dbPath), autoId) |> normalizePath(winslash = "/", mustWork=FALSE)
}


#' @export
autoIdDirRelativeToHyper <- function(autoId) {
  file.path("auto", autoId)
}


.jobColTypes <- readr::cols(
  methodName = readr::col_character(),
  expansionNr = readr::col_integer(),
  expression = readr::col_character(),
  prefix = readr::col_character(),
  model = readr::col_character(),
  methodFile = readr::col_character(),
  obsNr = readr::col_integer(),
  obs = readr::col_character(),
  timeInMinutes = readr::col_integer(),
  nCpus = readr::col_integer(),
  round = readr::col_integer()
)


#' @export
getPastJobs <- function(dbPath, autoId, autoRound = NULL) {
  filePath <- file.path(autoIdDir(dbPath, autoId), "jobs.csv")
  if (!file.exists(filePath)) return(NULL)
  jobs <- readr::read_delim(filePath, delim="\t", col_types=.jobColTypes)
  if (hasValue(autoRound)) {
    jobs <- jobs |> filter(round %in% autoRound)
  }
  return(jobs)
}


#' @export
addToPastJobs <- function(dbPath, autoId, newJobs) {
  filePath <- file.path(autoIdDir(dbPath, autoId), "jobs.csv")
  if (!file.exists(filePath)) {
    oldJobs <- NULL
    round <- 1
  } else {
    oldJobs <- readr::read_delim(filePath, delim="\t", col_types=.jobColTypes)
    if (NROW(oldJobs) > 0) {
      round <- max(oldJobs$round, na.rm=TRUE) + 1
    } else {
      oldJobs <- NULL
      round <- 1
    }
  }
  if (NROW(newJobs) == 0) return(NULL)
  newJobs$round <- as.integer(round)
  newJobs$expression <- sapply(newJobs$expression, rlang::expr_text, width = 500L)
  readr::write_delim(bind_rows(oldJobs, newJobs), file=filePath, delim="\t")
  return(round)
}


#' @export
getNewAuto <- function(dbPath, autoId, autoRound) {
  jobs <- getPastJobs(dbPath, autoId, autoRound)
  models <- unique(jobs$model)
  unevaled <- lapply(models, \(model) {
    path <- getPaths(dbPath, model)
    jobsModel <- jobs |> filter(model %in% .env$model)
    methods <- unique(jobsModel$methodName)
    methods <- methods[dir.exists(file.path(path$esti, methods))]
    meta <- lapply(methods, \(method) {
      methodEstiPath <- file.path(path$esti, method)
      if (!dir.exists(methodEstiPath)) return(NULL)
      meta <- getMetaGeneric(methodEstiPath)
      if (NROW(meta) == 0) return(NULL)
      meta$method <- method
      meta$estiPath <- NULL
      return(meta)
    }) |>
      bind_rows()
    if (NROW(meta) == 0) return(NULL)
    scoreFiles <- getScoreFiles(path$eval, methods)
    scores <-
      lapply(scoreFiles, \(sf) {
        scores <- readr::read_csv(sf, col_types = readr::cols())
        scores[c("method", "truthNr", "obsNr", "taskNr")]
      }) |>
      bind_rows()
    if (NROW(scores) != 0) {
      unevaled <- meta |> anti_join(scores, by = c("truthNr", "obsNr", "taskNr", "method"))
    } else {
      unevaled <- meta
    }
    unevaled$model <- model
    unevaled
  }) |>
    bind_rows()
  return(unevaled)
}


#' @export
initializeAuto <- function(
    dbPath,
    methodInfo,
    ...
) {
  moreInfo <- list(...)
  autoId <- newAutoId(dbPath)
  filePath <- file.path(autoIdDir(dbPath, autoId), "methodInfo.json")
  DEEButil::writeJson(c(lst(dbPath, autoId), methodInfo, moreInfo), filePath)
  return(autoId)
}

#' @export
readAutoInfo <- function(
    dbPath,
    autoId
) {
  filePath <- file.path(autoIdDir(dbPath, autoId), "methodInfo.json")
  DEEButil::readJson(filePath)
}


#' @export
isFirstAutoCall <- function(dbPath, autoId) {
  dirs <- dir(file.path(autoIdDir(dbPath, autoId)))
  !any(str_detect(dirs, "methods_BestCube"))
}
