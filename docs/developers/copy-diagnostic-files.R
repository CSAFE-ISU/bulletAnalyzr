files <- c("bulldata.rds", "bullet_scores_pre_ss.rds", "bullet_scores_post_ss.rds",
           "features.rds", "resolution.rds", "filtered_data_for_pt.rds")

for (f in files) {
  file.copy(file.path(tempdir(), f), 
            file.path("C:\\Users\\reind\\OneDrive\\Documents\\bulletAnalyzr\\docs\\developers\\diagnostics", f))
}
