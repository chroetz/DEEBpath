#' @export
getMethodTableNames <- function(dbPath, autoId = NULL, fullNames = TRUE) {
  if (hasValue(autoId)) { # TODO: get file by num not by date?
    methodTableFilePaths <- list.files(
      autoIdDir(dbPath, autoId),
      pattern = "^methods.*\\.csv$",
      full.names = TRUE)
    i <- which.max(file.mtime(methodTableFilePaths))
    methodTableFilePaths <- methodTableFilePaths[i]
  } else {
    methodTableFilePaths <- list.files(
      hyperDir(dbPath),
      pattern = "^methods.*\\.csv$",
      full.names = TRUE)
  }
  if (fullNames)  {
    methodTableFiles <- methodTableFilePaths
    names(methodTableFiles) <- basename(methodTableFilePaths)
  } else {
    methodTableFiles <- basename(methodTableFilePaths)
  }
  return(methodTableFiles)
}


#' @export
getMethodTable <- function(dbPath, methodTablePaths, addSlurm = TRUE, baseName = NULL) {
  colTypes <- readr::cols(
    model = readr::col_character(),
    obs = readr::col_character(),
    methodFile = readr::col_character()
  )
  methodTableRegex <-
    lapply(
      methodTablePaths,
      readr::read_csv,
      col_types = colTypes
    ) |>
    bind_rows()
  models <- getModels(dbPath)
  obsNames <- getObsNames(dbPath)
  methodTable <-
    methodTableRegex |>
    mutate(methodBaseFile = methodNameFromMethodFile(dbPath, methodFile)) |>
    mutate(model = lapply(.data$model, \(modeRegex) str_subset(models, modeRegex))) |>
    tidyr::unnest(.data$model) |>
    left_join(tibble(model = names(obsNames), obsName = obsNames), join_by("model")) |>
    mutate(obs = lapply(seq_along(.data$obs), \(i) str_subset(.data$obsName[[i]], .data$obs[i]))) |>
    select(-"obsName") |>
    tidyr::unnest("obs") |>
    distinct()



  if (addSlurm) {
    slurmTimeTable <- loadSlurmTimeTable(dbPath)
    if (is.null(slurmTimeTable)) {
      methodTable$timeInMinutes <- 60
      methodTable$nCpus <- 1
    } else {
      methodTable <-
        methodTable |>
        left_join(slurmTimeTable, join_by("methodBaseFile")) |>
        mutate(timeInMinutes = ifelse(is.na(.data$timeInMinutes), 60, .data$timeInMinutes))|>
        mutate(nCpus = ifelse(is.na(.data$nCpus), 1, .data$nCpus))
    }
  }

  return(methodTable)
}


loadSlurmTimeTable <- function(dbPath) {
  filePath <- file.path(hyperDir(dbPath), "_slurmTime.csv")
  if (!file.exists(filePath)) {
    cat("slurmTime.csv not found. Using always default time for jobs.\n")
    return(NULL)
  }
  readr::read_csv(
    filePath,
    col_types = readr::cols(
      methodBaseFile = readr::col_character(),
      timeInMinutes = readr::col_integer(),
      nCpus = readr::col_integer()
    ))
}


methodNameFromMethodFile <- function(dbPath, methodFiles) {
  methodNames <- character(length(methodFiles))
  for (i in seq_along(methodFiles)) {
    methodFile <- methodFiles[i]
    hyperParmsPath <- getMethodFile(dbPath, methodFile)
    hyperParmsList <- ConfigOpts::readOptsBare(hyperParmsPath)
    if (nchar(hyperParmsList$name) > 0) {
      methodNames[i] <- hyperParmsList$name
    } else {
      methodNames[i] <- methodFile
    }
  }
  return(methodNames)
}


#' @export
getTargetTaskAndScore <- function(dbPath) {
  path <- file.path(hyperDir(dbPath), "_targetTaskAndScore.csv")
  table <- readr::read_csv(path, col_types = readr::cols())
  return(table)
}
