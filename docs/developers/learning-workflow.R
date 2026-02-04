library(dplyr)

compare_dfs <- function(df1, df2) {
  diff_cols1 <- get_setdiff_cols(df1, df2)
  print(paste("Columns in df1 not in df2:", paste(diff_cols1, collapse = ", ")))

  diff_cols2 <- get_setdiff_cols(df2, df1)
  print(paste("Columns in df2 not in df1:", paste(diff_cols2, collapse = ", ")))

  shared_cols <- get_same_cols(df1, df2)
  testthat::expect_identical(
    df1[shared_cols],
    df2[shared_cols]
  )
}

# Get the names of columns in df1 that are not in df2
get_setdiff_cols <- function(df1, df2) {
  return(setdiff(colnames(df1), colnames(df2)))
}

# Get the names of columns that are in both data frames
get_same_cols <- function(df1, df2) {
  return(intersect(colnames(df1), colnames(df2)))
}

load_snapshot <- function(
    snapshot,
    snapshot_dir = "app/tests/testthat/_snaps/shinytest2") {
  json_data <- jsonlite::fromJSON(file.path(snapshot_dir, snapshot))
  return(json_data)
}


app7 <- load_snapshot("pipeline-007.json")$export
app13 <- load_snapshot("pipeline-013.json")$export

# Each row is a single land
cbull <- as.data.frame(app13$cbull_export) # (6 x 6)
allbull <- as.data.frame(app13$allbull_export) # (12 x 8)
preCC <- as.data.frame(app7$preCC_export) # (12 x 9)
postCC <- as.data.frame(app13$postCC_export) # (12 x 13)
bullets <- as.data.frame(app13$comparison_export$bullets) # (12 x 21)

# Each row is a pair of lands
comparisons <- as.data.frame(app13$comparison_export$comparisons) # (144 x 38)
features_scaled <- as.data.frame(app13$comparison_export$features_scaled) # (144 x 30)

# Each row is a bullet
bullet_scores <- as.data.frame(app13$comparison_export$bullet_scores) # (4 x 32)

bscores <- bullet_scores %>% tidyr::unnest(c(
  data.land1, data.land2, data.ccf0, data.landA, data.landB, data.D, data.ccf,
  data.cms, data.cms2, data.cms2_per_mm, data.cms_per_mm, data.lag, data.lag_mm, data.left_cms,
  data.length, data.length_mm, data.matches, data.matches_per_mm, data.mismatches, data.mismatches_per_mm,
  data.non_cms, data.non_cms_per_mm, data.overlap, data.right_cms, data.rough_cor, data.sd_D,
  data.sum_peaks, data.rfscore, data.samesource
))
bscores <- as.data.frame(bscores)  # bullet_scores is a tibble
colnames(bscores) <- stringr::str_replace(colnames(bscores), "data.", "")
# bscores is sorted by bulletB then bulletA, but features_scaled is sorted by land2 then land1
bscores <- bscores %>% arrange(land2, land1)


# Check if values in shared columns are identical across data frames ----

compare_dfs(cbull, allbull %>% filter(bullet_name == "Barrel_1.Bullet_2.Land_.x3p"))

compare_dfs(allbull, preCC)

compare_dfs(preCC, postCC)

compare_dfs(postCC, bullets)

compare_dfs(features_scaled, comparisons)

compare_dfs(bscores, features_scaled)

# Diff
# 1. Allbull is cbull for one or more bullets plus: bullet, land
# 2. preCC is allbull plus: crosscut
# 3. postCC is preCC plus: ccdata.x, ccdata.y, ccdata.value, groove
# 4. bullets is postCC plus: sigs.x, sigs.y, sigs.value, sigs.raw_sig, sigs.se, sigs.sig, bulletland, x3pimg
# 5. bullet_scores is a nested version of features_scaled plus: samesource, bulllet_score. Also, after unnesting bullet_scores for comparison with features_scaled, bullet_scores is sorted by bulletB then bulletA while features_scaled is sorted by land2 then land1

# Workflow
# 1. Upload lands to cbull
# 2. Cancel or push cbull to allbull. Clear cbull data frame, but keep cbull name?
# 3. Add crosscut locations to allbull and change stage. Don't create preCC.
# 4. Add ccdata.x, ccdata.y, ccdata.value, groove to allbull from step 3 and change stage. Don't create postCC.
# 5. Add sigs.x, sigs.y, sigs.value, sigs.raw_sig, sigs.se, sigs.sig, bulletland, x3pimg to allbull from step 4 and change stage. Don't create bullets
