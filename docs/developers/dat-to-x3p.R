#' Convert .dat Files to x3p Objects
#'
#' Reads all space-delimited \code{.dat} files in a directory and converts each
#' one into an x3p object. Each \code{.dat} file is expected to have three
#' unnamed columns: \code{x}, \code{y}, and \code{value} (height in microns).
#' Non-numeric height values (e.g. \code{"1.#QNAN0"}) are coerced to \code{NA}.
#'
#' @param main_dir Path to a directory containing \code{.dat} files.
#' @returns A data frame with columns \code{source} (file paths) and \code{x3p}
#'   (a list column of x3p objects).
#' @examples
#' dat_to_x3p("path/to/dat_dir")
dat_to_x3p <- function(main_dir) {
  files <- list.files(main_dir, pattern = ".dat", full.names = TRUE)
  x3ps <- lapply(files, function(f) {
    df <- readr::read_delim(
      f,
      delim = " ",
      col_names= c("x", "y", "value"),
      na = c("", "NA", "1.#QNAN0"),
      progress = FALSE
    ) # There are no actual Nas created – probably because of the # in the name
    
    # Should create a considerable number of NAs
    df$value <- as.numeric(df$value)
    
    # Convert data frame into x3p. Value is measured in microns
    x3p <- x3ptools::df_to_x3p(df)
  })
  df <- data.frame(source = files)
  df$x3p <- x3ps
  return(df)
}
