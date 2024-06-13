#' @export
removeEmptyMethodFolders <- function(dbPath, removeNonCsvFolders = FALSE) {
  pt <- proc.time()
  models <- getModels(dbPath)
  cntEmpty <- 0
  cntNoncsv <- 0
  for (model in models) {
    cat("MODEL:", model, "\n")
    paths <- getPaths(dbPath, model)
    allDirs <- list.dirs(paths$esti)
    for (dir in allDirs) {
      files <- list.files(dir)
      if (length(files) == 0) {
        cat("Folder", dir, "is empty. Removing... ")
        unlink(dir, recursive = TRUE, force = TRUE)
        if (dir.exists(dir)) cat("failed") else cat("ok")
        cntEmpty <- cntEmpty + 1
        cat("\n")
        next
      }
      if (removeNonCsvFolders) {
        files <- str_subset(files, "\\.csv$", negate=TRUE)
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
