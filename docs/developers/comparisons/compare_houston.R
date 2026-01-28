source("docs/developers/manual_bullet_comparison_pipeline.R")
source("docs/developers/parse_filenames.R")

make_pairs_df <- function(bullets){
  pairs <- combn(bullets, 2)
  df <- data.frame(bullet1 = pairs[1, ], bullet2 = pairs[2, ])
  df$bullet1_name <- sapply(df$bullet1, parse_filepath)
  df$bullet2_name <- sapply(df$bullet2, parse_filepath)
  return(df)
}

make_outfile <- function(outdir, bullet1_name, bullet2_name) {
  return(file.path(outdir, paste0(bullet1_name, "_", bullet2_name, ".rds")))
}

houston_dir <- "/Volumes/research/csafe-firearms/bullet-scans/Houston Set Final"
groups <- list.dirs(houston_dir, recursive = FALSE)
barrels <- list.dirs(groups[1], recursive = FALSE)
bullets <- list.dirs(barrels, recursive = FALSE)
pairs <- make_pairs_df(bullets)

for (i in 1:nrow(pairs)) {
  compare_bullets(
    bullet1_dir = pairs$bullet1[i],
    bullet2_dir = pairs$bullet2[i],
    outfile = make_outfile("docs/developers/comparisons", pairs$bullet1_name[i], pairs$bullet2_name[i])
  )
}

