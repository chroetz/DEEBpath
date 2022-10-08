#' @export
estiFile <- function(info=NULL, truthNr=NULL, obsNr=NULL, taskNr=NULL, ending = TRUE) {
  info <- as.list(info)
  if (is.null(truthNr)) truthNr <- info$truthNr
  if (is.null(obsNr)) obsNr <- info$obsNr
  if (is.null(taskNr)) taskNr <- info$taskNr
  stopifnot(!is.null(truthNr))
  stopifnot(!is.null(obsNr))
  stopifnot(!is.null(taskNr))
  n <- max(length(truthNr), length(obsNr), length(taskNr))
  stopifnot(length(truthNr) == n || length(truthNr) == 1)
  stopifnot(length(obsNr) == n || length(obsNr) == 1)
  stopifnot(length(taskNr) == n || length(taskNr) == 1)
  truthNr <- as.integer(truthNr)
  obsNr <- as.integer(obsNr)
  taskNr <- as.integer(taskNr)
  stopifnot(!any(is.na(truthNr)))
  stopifnot(!any(is.na(obsNr)))
  stopifnot(!any(is.na(taskNr)))
  fileName <- sprintf("truth%04dobs%04dtask%02desti", truthNr, obsNr, taskNr)
  if (isTRUE(ending)) {
    fileName <- paste0(fileName, ".csv")
  } else if (is.character(ending)) {
    fileName <- paste0(fileName, ending)
  }
  return(fileName)
}
