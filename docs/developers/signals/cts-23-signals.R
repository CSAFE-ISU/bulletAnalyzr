source("docs/developers/bullet-codes.R")
source("docs/developers/view-pipeline.R")
source("docs/developers/comparisons/align-signals.R")
source("docs/developers/comparisons/comparison-utils.R")
source("inst/scripts/manual_groove_selection.R")


study_dir <- get_study_path("CTS Forensic Testing Program")

# Get bullets cts.23.1.A-C, cts.23.2.A, cts.23.3.A, and cts.23.5.A
years <- list.dirs(study_dir, recursive = FALSE)
year <- years[5]
items <- list.dirs(year, recursive = FALSE)
bullets <- unlist(lapply(items, function(i) list.dirs(i, recursive = FALSE)))
bullets <- bullets[-6]

# # Find grooves ----
# for (bullet in bullets) {
#   process_directory(bullet, file.path(bullet, "grooves.csv"))
# }

# Plot signals ----
lands <- unlist(lapply(bullets, function(b) list.files(b, pattern = "\\.x3p", full.names = TRUE)))
for (land in lands) {
  view_pipeline(
    filepath = land, 
    grooves_csv = file.path(dirname(land), "grooves.csv"),
    view_land = FALSE, 
    view_signal = TRUE, 
    save_signal_plot = TRUE
  )
}

