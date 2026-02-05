# Compare Phoenix Test
#
# Batch comparison script for the Phoenix Test bullet dataset. Generates
# all pairwise bullet combinations (known and unknown), then runs the manual
# comparison pipeline on each pair using parallel processing (4 cores).
# Results are saved as individual RDS files in the dataset's comparisons/
# directory.
#
# Requires the Phoenix Test dataset on an external drive or LSS.
#
# Usage:
#   source("docs/developers/comparisons/compare-phoenix.R")

source("inst/scripts/manual_groove_selection.R")
source("docs/developers/comparisons/comparison-utils.R")
source("docs/developers/comparisons/manual-bullet-comparison-pipeline.R")
source("docs/developers/bullet-codes.R")


# Helper Functions --------------------------------------------------------

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

list_bullets <- function(main_dir) {
  # List bullet directories
  dirs <- list.dirs(main_dir, recursive = FALSE)

  # Get known bullets
  kbarrels <- dirs[startsWith(basename(dirs), "Gun")]
  kbullets <- unlist(lapply(kbarrels, function(d) list.dirs(d, recursive = FALSE)))

  # Get unknown bullets
  ubullets <- dirs[startsWith(basename(dirs), "Unknown")]

  bullets <- c(kbullets, ubullets)

  return(bullets)
}


# Setup -------------------------------------------------------------------

if (dir.exists("/Volumes/T7_Shield/CSAFE/datasets/bullet_datasets/Phoenix Test")) {
  phoenix_dir <- "/Volumes/T7_Shield/CSAFE/datasets/bullet_datasets/Phoenix Test"
} else if (dir.exists("/Volumes/research/csafe-firearms/bullet-scans/Phoenix Test")) {
  phoenix_dir <- "/Volumes/research/csafe-firearms/bullet-scans/Phoenix Test"
} else if (dir.exists("/Volumes/lss/research/csafe-firearms/bullet-scans/Phoenix Test")) {
  phoenix_dir <- "/Volumes/lss/research/csafe-firearms/bullet-scans/Phoenix Test"
} else {
  stop("Are you connected to LSS?")
}

bullets <- list_bullets(main_dir = phoenix_dir)


# Manually Detect Grooves -------------------------------------------------

for (bullet in bullets[32:34]) {
  process_directory(
    bullet,
    file.path(bullet, "grooves.csv")
  )
}

# Compare Bullets ---------------------------------------------------------

pairs <- make_pairs_df(bullets)

for (i in 1:nrow(pairs)) {
  compare_bullets(
    bullet1_dir = pairs$bullet1[i],
    bullet2_dir = pairs$bullet2[i],
    outfile = make_outfile(file.path(phoenix_dir, "comparisons"), pairs$bullet1_name[i], pairs$bullet2_name[i]),
    cores = 4
  )
}
