accept_new_snap <- function(snapshot = "pipeline-001") {
  snapshot_dir = "tests/testthat/_snaps/shinytest2"
  
  new <- file.path(snapshot_dir, paste0(snapshot, ".new.json"))
  old <- file.path(snapshot_dir, paste0(snapshot, ".json"))

  file.rename(new, old)
}

# Load a json snapshot
load_snap <- function(
    snapshot,
    snapshot_dir = "tests/testthat/_snaps/shinytest2") {
  
  if (!file.exists(file.path(snapshot_dir, snapshot))) {
    stop("Snapshot does not exist")
  }
  
  json_data <- jsonlite::fromJSON(file.path(snapshot_dir, snapshot))
  return(json_data)
}

compare_new_old_snaps <- function(snapshot = "pipeline-001") {
  actual <- load_snap(paste0(snapshot, ".new.json"))
  expected <- load_snap(paste0(snapshot, ".json"))
  
  testthat::expect_identical(actual, expected)
}

get_inputs_for_tests <- function(app) {
  inputs <- names(app$get_values(input = TRUE)$input)
  inputs <- inputs[!stringr::str_detect(inputs, "shinyscreenshot")]
  return(inputs)
}
