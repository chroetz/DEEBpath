#' @export
nameWithHash <- function(name, lst) {
  lst$name <- NULL
  lst <- sortList(lst, removePattern = "^_")
  nameWithHash <- paste0(name, "_", rlang::hash(lst))
  return(nameWithHash)
}

sortList <- function(lst, removePattern = NULL) {
  nms <- names(lst)
  if (is.null(nms)) return(lst)
  sel <- if (is.null(removePattern)) TRUE else !grepl(removePattern, nms)
  nms <- sort(nms[sel])
  resList <- list()
  for (i in seq_along(nms)) {
    nm <- nms[i]
    resList[[i]] <- if (is.list(lst[[nm]])) sortList(lst[[nm]], removePattern) else lst[[nm]]
  }
  names(lst) <- NULL
  attributes(resList) <- attributes(lst)
  names(resList) <- nms
  return(resList)
}
