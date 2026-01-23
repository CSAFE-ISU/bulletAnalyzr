#!/usr/bin/env Rscript List Bullet Scans on LSS Script
#
#This script lists bullets scans saved as x3p files in LSS > csafe-firearms >
#bullet-scans. The script ignores the Archived and Other folders.
#
#Usage: 
#   1) Mount LSS as a folder on your computer. 
#   2) Change main_dir as necessary.
#   3) Run the script interactively in R or type `Rscript
#      docs/developers/list_bullet_scans.R` in the terminal

list_scans_by_study <- function(directory) {
  study <- basename(directory)
  print(paste("Listing files in ", study, "..."))
  
  df <- data.frame(
    study = study,
    filename = list.files(directory, recursive = TRUE, full.names = FALSE, pattern = "\\.x3p")
  )
  outfile <- file.path("docs", "developers", "bullet-studies", paste(study, "scans.csv"))
  write.csv(df, outfile, row.names = FALSE)
  print(paste("Saving files in ", study, "..."))
  
}

list_scans_by_bullet <- function(study, bullet_dir) {
  df <- data.frame(
    study = study,
    filename = list.files(bullet_dir, recursive = TRUE, full.names = FALSE, pattern = "\\.x3p")
  )
  return(df)
}

main_dir <- "/Volumes/research/csafe-firearms/bullet-scans"

dirs <- list.dirs(main_dir, recursive = FALSE)

# Drop Archived, Other, CSAFE Persistence studies, and LAPD 
# dirs <- dirs[!(basename(dirs) %in% c("Archived", "Other", "CSAFE Persistence", 
#                                      "CSAFE Persistence Rescans 2019", "CSAFE Persistence Rescans 2020", "LAPD"))]
# for (i in 1:length(dirs)) {
#   list_scans(directory = dirs[i])
# }

# Subset the DCI scans
dirs <- dirs[basename(dirs) == "CSAFE Persistence"]
dci <- list.dirs(dirs, recursive = FALSE)[1]
# Keep the first two barrels
barrel <- unlist(lapply(dci, function(d) list.dirs(d, recursive = FALSE)[1:2]))
# Keep both sets
sets <- unlist(lapply(barrel, function(d) list.dirs(d, recursive = FALSE)))
# Keep the first two bullets
bullet <- unlist(lapply(sets, function(d) list.dirs(d, recursive = FALSE)[1:2]))
files <- lapply(bullet, function(b) list_scans_by_bullet("CSAFE Persistence/DCI", b))
df <- do.call(rbind, files)
outfile <- file.path("docs", "developers", "bullet-studies", "CSAFE Persistence DCI subset scans.csv")
write.csv(df, outfile, row.names = FALSE)


# SW scans
sw <- list.dirs(dirs, recursive = FALSE)[2]
barrel <- unlist(lapply(sw, function(d) list.dirs(d, recursive = FALSE)))
bullet <- unlist(lapply(barrel, function(d) list.dirs(d, recursive = FALSE)))
files <- lapply(bullet, function(b) list_scans_by_bullet("CSAFE Persistence/SW", b))
df <- do.call(rbind, files)
outfile <- file.path("docs", "developers", "bullet-studies", "CSAFE Persistence SW scans.csv")
write.csv(df, outfile, row.names = FALSE)

# Subset LAPD scans
lapd <- dirs[basename(dirs) == "LAPD"]
# Keep the first 2 barrels
barrel <- unlist(lapply(lapd, function(d) list.dirs(d, recursive = FALSE)[1:2]))
bullet <- unlist(lapply(barrel, function(d) list.dirs(d, recursive = FALSE)))
files <- lapply(bullet, function(b) list_scans_by_bullet("LAPD", b))
df <- do.call(rbind, files)
outfile <- file.path("docs", "developers", "bullet-studies", "LAPD scans.csv")
write.csv(df, outfile, row.names = FALSE)
