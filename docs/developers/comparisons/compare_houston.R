source("docs/developers/manual_bullet_comparison_pipeline.R")
source("docs/developers/parse_filenames.R")

houston_dir <- "/Volumes/lss/research/csafe-firearms/bullet-scans/Houston Set Final"
groups <- list.dirs(houston_dir, recursive = FALSE)
barrels <- list.dirs(groups[1], recursive = FALSE)
bullets <- list.dirs(barrels[2], recursive = FALSE)

parse_filepath(bullets[4], show_format = TRUE)

pairs <- combn(bullets, 2)
df <- data.frame(item1 = pairs[1, ], item2 = pairs[2, ])

