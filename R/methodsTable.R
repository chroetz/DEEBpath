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
    method = readr::col_character(),
    timeInMinutes = readr::col_integer()
  )
  methodTable <-
    lapply(
      methodsTableFiles,
      readr::read_csv,
      col_types = colTypes
    ) |>
    dplyr::bind_rows()
  return(methodTable)
}
