#' @export
getPaths <- function(dbPath, model, example = FALSE) {
  if (example) {
    modelPath <- file.path(dbPath, model, "example")
  } else {
    modelPath <- file.path(dbPath, model)
  }
  list(
    truth = file.path(modelPath, "truth"),
    esti = file.path(modelPath, "estimation"),
    eval = file.path(modelPath, "evaluation"),
    plots = file.path(modelPath, "evaluation", "plots"),
    example = if (example) NULL else file.path(modelPath, "example"),
    task = file.path(modelPath, "task"),
    obs = file.path(modelPath, "observation"),
    runOpts = file.path(modelPath, "Opts_Run.json")
  )
}

