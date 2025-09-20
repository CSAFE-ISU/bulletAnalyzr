# Load a large json snapshot
load_large_snapshot <- function(
    snapshot,
    snapshot_dir = "app/tests/testthat/_snaps/shinytest2") {
  json_data <- jsonlite::fromJSON(file.path(snapshot_dir, snapshot))
  return(json_data)
}
