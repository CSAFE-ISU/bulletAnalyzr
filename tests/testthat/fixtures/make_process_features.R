# Run the app with save_diagnostics = TRUE to save intermediate files to the
# temporary directory.

# Copy rds files to fixtures folder
files <- c("bullet_scores_pre_ss.rds", "bullet_scores_post_ss.rds", 
           "features.rds", "bulldata.rds", "filtered_data_for_pt.rds")

for (f in files) {
  file.copy(
    file.path(tempdir(), f), 
    testthat::test_path("fixtures", f)
  )
}

