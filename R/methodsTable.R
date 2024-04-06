#' @export
getMethodTableNames <- function(dbPath) {
  methodTableFiles <- list.files(
    file.path(dbPath, "_hyper"),
    pattern = "^methods.*\\.csv$")
  return(methodTableFiles)
}


#' @export
getMethodTable <- function(dbPath, methodTableNames) {
  methodsTableFiles <- file.path(dbPath, "_hyper", methodTableNames)
  colTypes <- readr::cols(
    model = readr::col_character(),
    obs = readr::col_character(),
    method = readr::col_character()
  )
  methodTableRegex <-
    lapply(
      methodsTableFiles,
      readr::read_csv,
      col_types = colTypes
    ) |>
    bind_rows()
  models <- getModels(dbPath)
  obsNames <- getObsNames(dbPath)
  methodTable <-
    methodTableRegex |>
    mutate(model = lapply(model, \(mdl) str_subset(models, mdl))) |>
    tidyr::unnest(model) |>
    left_join(tibble(model = names(obsNames), obsNames = obsNames), join_by(model)) |>
    mutate(obs = lapply(seq_along(obs), \(i) str_subset(obsNames[[i]], obs[i]))) |>
    select(-obsNames) |>
    tidyr::unnest(obs)
  methodTable <-
    methodTable |>
    left_join(loadSlurmTimeTable(dbPath), join_by(method))
  return(methodTable)
}


loadSlurmTimeTable <- function(dbPath) {
  filePath <- file.path(dbPath, "_hyper", "slurmTime.csv")
  readr::read_csv(
    filePath,
    col_types = readr::cols(
      method = readr::col_character(),
      timeInMinutes = readr::col_integer()
    ))
}



#' @export
getTargetTaskAndScore <- function(dbPath) {
  path <- file.path(dbPath, "_hyper", "_targetTaskAndScore.csv")
  table <- readr::read_csv(path, col_types = readr::cols())
  return(table)
}
