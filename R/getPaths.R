#' @export
getPaths <- function(dbPath, model) {
  modelPath <- file.path(dbPath, model)
  list(
    truth = file.path(modelPath, "truth"),
    esti = file.path(modelPath, "estimation"),
    eval = file.path(modelPath, "evaluation"),
    example = file.path(modelPath, "example"),
    task = file.path(modelPath, "task"),
    obs = file.path(modelPath, "observation"),
    runOpts = file.path(modelPath, "Opts_Run.json")
  )
}
