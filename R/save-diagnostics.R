#' Save an item to an RDS file
#'
#' This function saves an R object to an RDS file in a specified output
#' directory. The filename can optionally include bullet names as a prefix, and
#' x3p columns can be dropped from data frames before saving to reduce file
#' size.
#'
#' @param item An R object to save. Typically a data frame or list.
#' @param outfile A character string specifying the base name for the output
#'   file, including the .rds extension.
#' @param outdir A character string specifying the path to the output directory.
#'   The directory must exist or the function will throw an error.
#' @param bullet_names An optional character vector of length 2 containing
#'   bullet names to prepend to the filename. If provided, the output filename
#'   will be formatted as "bullet1_bullet2_outfile.rds". Default is NULL.
#' @param drop_x3p Logical indicating whether to drop x3p columns from the item
#'   before saving (to save space). If TRUE, calls \code{make_export_df()} on
#'   the item. Default is TRUE.
#'
#' @return Null, called for side-effects
#'
#' @noRd
save_to_file <- function(item, outfile, outdir, bullet_names = NULL, drop_x3p = TRUE) {
  if (!dir.exists(outdir)) {
    stop(paste("Folder", outdir, "does not exist."))
  }
  
  if (!is.null(bullet_names)) {
    outfile <- file.path(outdir, paste0(bullet_names[1], "_", bullet_names[2], "_", outfile))
  } else {
    outfile <- file.path(outdir, outfile)
  }
  
  # Drop x3p column from data frame to save space
  if (drop_x3p) {
    item <- make_export_df(df = item)
  }
  
  saveRDS(item, outfile)
}
