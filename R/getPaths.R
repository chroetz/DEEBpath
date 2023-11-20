#' @export
getPaths <- function(dbPath, model) {
  modelPath <- file.path(dbPath, model)
  list(
    truth = file.path(modelPath, "truth"),
    esti = file.path(modelPath, "estimation"),
    eval = file.path(modelPath, "evaluation"),
    plots = file.path(modelPath, "evaluation", "plots"),
    task = file.path(modelPath, "task"),
    obs = file.path(modelPath, "observation"),
    runOpts = file.path(modelPath, "Opts_Run.json")
  )
}

#' @export
summaryDir <- function(dbPath) {
  file.path(dbPath, "_summary")
}

#' @export
summaryTablePath <- function(dbPath) {
  file.path(summaryDir(dbPath), "scores.csv")
}

#' @export
summaryHyperPath <- function(dbPath, model, methodBase) {
  file.path(summaryDir(dbPath), paste0(model, "_", methodBase, ".csv"))
}
