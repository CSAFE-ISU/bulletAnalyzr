files <- c("bulldata.rds", "bullet_scores.rds", "features.rds", "resolution.rds", 
           "filtered_data_for_pt.rds")

for (f in files) {
  file.copy(file.path(tempdir(), f), 
            file.path("C:\\Users\\reind\\OneDrive\\Documents\\bulletAnalyzr\\docs\\developers\\diagnostics", f))
}
