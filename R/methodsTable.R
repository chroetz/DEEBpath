#' @export
getMethodTableHyper <- function(dbPath) {
  methodsTableFile <- file.path(dbPath, "_hyper", "methods.csv")
  methods <- readr::read_csv(
    methodsTableFile,
    col_types = readr::cols(
      model = readr::col_character(),
      obs = readr::col_integer(),
      method = readr::col_character()
    ))
  return(methods)
}
