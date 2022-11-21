#' @export
parenthesisFileName <- function(pairs = list(...), ..., .ending=NULL) {
  pairs <- pairs[sapply(pairs, length) > 0]
  stopifnot(all(sapply(pairs, length) == 1))
  fileName <- paste(paste0(names(pairs), "(", sapply(pairs, as.character), ")"), collapse="")
  if (!is.null(.ending)) fileName <- paste0(fileName, ".", .ending)
  return(fileName)
}

#' @export
getParenthesisFileNameData <- function(path) {

  fileNames <-
    path |>
    dir() |>
    str_subset("^([^\\(]+\\([^\\)]+\\))+\\.[^\\.]+$")

  elements <- str_extract_all(fileNames, "([^\\(]+\\([^\\)]+\\))")
  lapply(seq_along(fileNames), \(i) {
    ele <- elements[[i]]
    keys <- getKeys(ele)
    values <- getValues(ele)
    result <- as.list(values)
    names(result) <- keys
    result$file = fileNames[i]
    result
  })
}

getKeys <- function(strs) {
  str_extract(strs, "[^\\(]+(?=\\()")
}

getValues <- function(strs) {
  str_extract(strs, "(?<=\\()[^\\)]+(?=\\))")
}

