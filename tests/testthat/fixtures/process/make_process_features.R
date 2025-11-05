# Run the app with save_diagnostics = TRUE to save intermediate files to the
# temporary directory.

# Copy rds files to fixtures > process folder
files <- c("bullet_scores_pre_ss.rds", "bullet_scores_post_ss.rds", 
           "features.rds", "bulldata.rds")

for (f in files) {
  file.rename(
    file.path(tempdir(), f), 
    testthat::test_path("fixtures", "process", f)
  )
}

# copy file to fixtures > phase_test folder
file.rename(
  file.path(tempdir(), "filtered_data_for_pt.rds"), 
  testthat::test_path("fixtures", "process", "filtered_data_for_pt.rds")
)

