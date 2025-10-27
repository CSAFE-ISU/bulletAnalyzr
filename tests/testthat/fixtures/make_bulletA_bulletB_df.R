report <- readRDS(testthat::test_path("fixtures", "report.rds"))
df <- report$comparison_export$features_scaled
# Add dummy bullets
df2 <- df
df2$bulletA <- ifelse(df2$bulletA == unique(df$bulletA)[1], "b_new1", "b_new2")
df2$bulletB <- ifelse(df2$bulletB == unique(df$bulletA)[1], "b_new1", "b_new2")
df <- rbind(df, df2)
saveRDS(df, testthat::test_path("fixtures", "bulA_bulB_df.rds"))

# create a nested version
df <- df %>%
  dplyr::group_by(bulletA, bulletB) %>%
  tidyr::nest()
saveRDS(df, testthat::test_path("fixtures", "bulA_bulB_nested_df.rds"))
