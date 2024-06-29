#' @export
removeEmptyMethodFolders <- function(dbPath, removeNonCsvFolders = FALSE) {
  pt <- proc.time()
  models <- getModels(dbPath)
  cntEmpty <- 0
  cntNoncsv <- 0
  for (model in models) {
    cat("MODEL:", model, "\n")
    paths <- getPaths(dbPath, model)
    allDirs <- list.files(paths$esti, full.names = TRUE, include.dirs = TRUE)
    for (dir in allDirs) {
      if (!dir.exists(dir)) next
      files <- list.files(dir, full.names=TRUE)
      isEmpty <- file.size(files) == 0
      if (sum(isEmpty) > 0) {
        cat("Removing empty files:", paste0(files[isEmpty], collapse=","), "\n")
        file.remove(files[isEmpty])
        files <- files[!isEmpty]
      }
      if (length(files) == 0) {
        cat("Folder", dir, "is empty. Removing... ")
        unlink(dir, recursive = TRUE, force = TRUE)
        if (dir.exists(dir)) cat("failed") else cat("ok")
        cntEmpty <- cntEmpty + 1
        cat("\n")
        next
      }
      if (removeNonCsvFolders) {
        files <- str_subset(files, "\\.csv$")
        if (length(files) == 0) {
          cat("Folder", dir, "no csv", files, ". Removing... ")
          unlink(dir, recursive = TRUE, force = TRUE)
          if (dir.exists(dir)) cat("failed") else cat("ok")
          cat("\n")
          cntNoncsv <- cntNoncsv + 1
        }
      }
    }
  }
  cat("Removed", cntEmpty, "empty dirs.\n")
  if (removeNonCsvFolders) cat("Removed", cntNoncsv, "non-csv dirs.\n")
  cat("Removing empty method folders took", (proc.time()-pt)[3], "s\n")
}
