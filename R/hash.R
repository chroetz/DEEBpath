#' @export
nameWithHash <- function(name, lst) {
  lst$name <- NULL
  lst <- normalizeList(lst, removePattern = "^_")
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

normalizeList <- function(lst, removePattern = NULL) {
  if (!is.list(lst)) return(normalizeType(lst))
  nms <- names(lst)
  if (is.null(nms)) {
    resList <- lapply(lst, normalizeList)
  } else {
    sel <- if (is.null(removePattern)) TRUE else !grepl(removePattern, nms)
    nms <- sort(nms[sel])
    resList <- lapply(nms, \(nm) {
      normalizeList(lst[[nm]], removePattern)
    })
    names(lst) <- NULL
    names(resList) <- nms
  }
  attributes(resList) <- attributes(lst)
  return(resList)
}

normalizeType <- function(x) {
  if (is.numeric(x)) return(as.double(x))
  return(x)
}
