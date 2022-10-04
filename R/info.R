#' works with list and environment
#' @export
loadPathsInInfo <- function(info) {
  selPath <- endsWith(names(info), "Path")
  for (nm in names(info)[selPath]) {
    path <- info[[nm]]
    if (!is.character(path) || !length(path) == 1) next
    if (is.na(path)) stop("Path in ", nm, " is NA!")
    nmWoPath <- substr(nm, 1, nchar(nm)-4)
    if (!is.null(info[[nmWoPath]])) next
    if (endsWith(path, ".csv")) {
      info[[nmWoPath]] <- DEEBtrajs::readTrajs(path)
    } else if (endsWith(path, ".json")) {
      info[[nmWoPath]] <- ConfigOpts::readOptsBare(path)
    }
  }
  return(info)
}
