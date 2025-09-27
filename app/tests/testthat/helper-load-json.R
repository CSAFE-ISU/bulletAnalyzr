# Load a json snapshot
load_snapshot <- function(
    snapshot,
    snapshot_dir = "app/tests/testthat/_snaps/shinytest2") {
  json_data <- jsonlite::fromJSON(file.path(snapshot_dir, snapshot))
  return(json_data)
}

compare_new_old_snaps <- function(snapshot = "pipeline-001") {
  actual <- load_snapshot(paste0(snapshot, ".new.json"))
  expected <- load_snapshot(paste0(snapshot, ".json"))
  testthat::expect_identical(actual, expected)
}

get_inputs_for_tests <- function(app) {
  inputs <- names(app$get_values(input = TRUE)$input)
  inputs <- inputs[!stringr::str_detect(inputs, "shinyscreenshot")]
  return(inputs)
}
