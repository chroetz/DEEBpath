#' @export
isDeebDb <- function(path) {
  models <- getModels(path)
  folders <- list.dirs(path = file.path(path, models[1]), full.names = FALSE, recursive = FALSE)
  all(c("truth", "observation", "task") %in% folders)
}

#' @export
getUniqueEntriesForEval <- function(dbPath) {
  models <- getModels(dbPath)
  modelPaths <- file.path(dbPath, models)
  methods <- unique(unlist(lapply(
    file.path(modelPaths, "estimation"),
    list.dirs,
    full.names = FALSE, recursive = FALSE)))
  truthFiles <- unique(unlist(lapply(
    file.path(modelPaths, "truth"),
    list.files,
    pattern = "truth\\d+\\.",
    full.names = FALSE, recursive = FALSE)))
  truthNrs <-
    truthFiles |>
    str_extract("(?<=truth)(\\d+)") |>
    as.integer() |>
    unique()
  obsFiles <- unique(unlist(lapply(
    file.path(modelPaths, "observation"),
    list.files,
    pattern = "truth\\d+obs\\d+\\.csv",
    full.names = FALSE, recursive = FALSE)))
  obsNrs <-
    obsFiles |>
    str_extract("(?<=obs)(\\d+)") |>
    as.integer() |>
    unique()
  taskFiles <- unique(unlist(lapply(
    file.path(modelPaths, "task"),
    list.files,
    pattern = "task\\d+\\.json",
    full.names = FALSE, recursive = FALSE)))
  taskNrs <-
    taskFiles |>
    str_extract("(?<=task)(\\d+)") |>
    as.integer() |>
    unique()
  scoreFunctions <- unique(unlist(lapply(
    file.path(modelPaths, "task"),
    \(path) {
      files <- list.files(path, pattern = "task\\d+\\.json", full.names = TRUE, recursive = FALSE)
      sapply(files, \(fl) sapply(
        ConfigOpts::readOptsBare(fl)$scoreList$list,
        \(x) x$name))
    })))

  return(list(
    models = models,
    methods = methods,
    truthNrs = truthNrs,
    obsNrs = obsNrs,
    taskNrs = taskNrs,
    scoreFunctions = scoreFunctions
  ))
}

#' @export
getUniqueTruthNrs <- function(
    dbPath, modelFilter = NULL, obsNrFilter = NULL
) {
  models <-  getModels(dbPath)
  if (!is.null(modelFilter)) models <- intersect(models, modelFilter)

  modelPaths <- file.path(dbPath, models)
  obsFiles <- unique(unlist(lapply(
    models,
    \(model) {
      paths <- getPaths(dbPath, model)
      list.files(paths$obs, pattern = "truth\\d+obs\\d+\\.csv", full.names = FALSE, recursive = FALSE)
    })))
  truthNrs <-
    obsFiles |>
    str_extract("(?<=truth)(\\d+)") |>
    as.integer()
  if (!is.null(obsNrFilter)) {
    obsNrs <-
      obsFiles |>
      str_extract("(?<=obs)(\\d+)") |>
      as.integer()
    truthNrs <- truthNrs[obsNrs %in% obsNrFilter]
  }
  return(unique(truthNrs))
}


#' @export
getNew <- function(dbPath, modelFilter = NULL) {
  models <- getModels(dbPath)
  if (!is.null(modelFilter)) models <- intersect(models, modelFilter)
  unevaled <- lapply(models, \(model) {
    path <- getPaths(dbPath, model)
    methods <- list.dirs(path$esti, full.names = FALSE, recursive = FALSE)
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
    scoreFiles <- getScoreFiles(path$eval)
    scores <-
      lapply(scoreFiles, \(sf) {
        scores <- readr::read_csv(sf, col_types = readr::cols())
        scores[c("method", "truthNr", "obsNr", "taskNr")]
      }) |>
      bind_rows()
    if (NROW(scores) != 0) {
      unevaled <- anti_join(meta, scores, by = c("truthNr", "obsNr", "taskNr", "method"))
    } else {
      unevaled <- meta
    }
    unevaled$model <- model
    unevaled
  }) |>
    bind_rows()
  return(unevaled)
}
