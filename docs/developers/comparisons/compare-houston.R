# Compare Houston Set Final
#
# Batch comparison script for the Houston Set Final bullet dataset. Generates
# all pairwise bullet combinations (known and unknown), then runs the manual
# comparison pipeline on each pair using parallel processing (4 cores).
# Results are saved as individual RDS files in the dataset's comparisons/
# directory.
#
# Requires the Houston Set Final dataset on an external drive or LSS.
#
# Usage:
#   source("docs/developers/comparisons/compare-houston.R")

source("docs/developers/comparisons/comparison-utils.R")
source("docs/developers/comparisons/manual-bullet-comparison-pipeline.R")
source("docs/developers/bullet-codes.R")


# Helper Functions --------------------------------------------------------

get_known_bullets <- function(groups) {
  # List distinct known barrels - KA, KB,..., KJ - from all groups
  kbarrels1 <- list.dirs(groups[1], recursive = FALSE)
  kbarrels1 <- kbarrels1[startsWith(basename(kbarrels1), "K")]
  kbarrels3 <- list.dirs(groups[3], recursive = FALSE)
  kbarrels3 <- kbarrels3[startsWith(basename(kbarrels3), "K")]
  kbarrels <- c(kbarrels1, kbarrels3)

  kbullets <- list.dirs(kbarrels, recursive = FALSE)

  return(kbullets)
}

get_unknown_bullets <- function(groups) {
  # List unknown barrels from all groups
  ubarrel1 <- list.dirs(groups[1], recursive = FALSE)
  ubarrel1 <- ubarrel1[!startsWith(basename(ubarrel1), "K")]
  ubarrel2 <- list.dirs(groups[2], recursive = FALSE)
  ubarrel2 <- ubarrel2[!startsWith(basename(ubarrel2), "K")]
  ubarrel3 <- list.dirs(groups[3], recursive = FALSE)
  ubarrel3 <- ubarrel3[!startsWith(basename(ubarrel3), "K")]
  ubarrels <- c(ubarrel1, ubarrel2, ubarrel3)

  ubullets <- list.dirs(ubarrels, recursive = FALSE)

  # Remove duplicate bullet - U36 is in group 1 and group 3
  ubullets <- ubullets[!duplicated(basename(ubullets))]

  # Sort by bullet number across all groups
  ubullets <- ubullets[order(basename(ubullets))]

  return(ubullets)
}

get_unknown_barrels <- function(groups) {
  # List unknown barrels - KA, KB,..., KJ - from all groups
}

list_bullets <- function(main_dir) {
  # List bullet directories
  groups <- list.dirs(main_dir, recursive = FALSE)
  groups <- groups[basename(groups) != "comparisons"]
  kbullets <- get_known_bullets(groups)
  ubullets <- get_unknown_bullets(groups)
  bullets <- c(kbullets, ubullets)
  return(bullets)
}


# Compare Bullets ---------------------------------------------------------

if (dir.exists("/Volumes/T7_Shield/CSAFE/datasets/bullet_datasets/Houston Set Final")) {
  houston_dir <- "/Volumes/T7_Shield/CSAFE/datasets/bullet_datasets/Houston Set Final"
} else if (dir.exists("/Volumes/research/csafe-firearms/bullet-scans/Houston Set Final")) {
  houston_dir <- "/Volumes/research/csafe-firearms/bullet-scans/Houston Set Final"
} else if (dir.exists("/Volumes/lss/research/csafe-firearms/bullet-scans/Houston Set Final")) {
  houston_dir <- "/Volumes/lss/research/csafe-firearms/bullet-scans/Houston Set Final"
} else {
  stop("Are you connected to LSS?")
}

bullets <- list_bullets(main_dir = houston_dir)
pairs <- make_pairs_df(bullets)

for (i in 1:nrow(pairs)) {
  compare_bullets(
    bullet1_dir = pairs$bullet1[i],
    bullet2_dir = pairs$bullet2[i],
    outfile = make_outfile(file.path(houston_dir, "comparisons"), pairs$bullet1_name[i], pairs$bullet2_name[i]),
    cores = 4
  )
}
