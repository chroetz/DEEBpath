#' @export
getMethodTableHyper <- function(dbPath) {
  methodsTableFile <- file.path(dbPath, "_hyper", "methods.csv")
  methods <- readr::read_csv(
    methodsTableFile,
    col_types = readr::cols(
      model = readr::col_character(),
      obs = readr::col_character(),
      method = readr::col_character(),
      timeInMinutes = readr::col_integer()
    ))
  return(methods)
}
