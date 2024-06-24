#' @export
nameWithHash <- function(name, lst) {
  lst$name <- NULL
  lst <- sortList(lst, removePattern = "^_")
  nameWithHash <- paste0(name, "_", rlang::hash(lst))
  return(nameWithHash)
}

#' @export
removeHashFromName <- function(name, recursive = FALSE) {
  name <- str_remove(name, "_[0-9a-f]{32}$")
  if (recursive) {
    len <- nchar(name)
    for (i in seq_len(1e4)) {
      name <- str_remove(name, "_[0-9a-f]{32}$")
      newLen <- nchar(name)
      if (all(len == newLen)) break
      len <- newLen
    }
  }
  return(name)
}

sortList <- function(lst, removePattern = NULL) {
  nms <- names(lst)
  if (is.null(nms)) return(lst)
  sel <- if (is.null(removePattern)) TRUE else !grepl(removePattern, nms)
  nms <- sort(nms[sel])
  resList <- lapply(nms, \(nm) {
    if (is.list(lst[[nm]])) {
      sortList(lst[[nm]], removePattern)
    } else {
      lst[[nm]]
    }
  })
  names(lst) <- NULL
  attributes(resList) <- attributes(lst)
  names(resList) <- nms
  return(resList)
}
