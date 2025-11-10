preprocess_hamby44 <- function(filepath) {
  df <- readr::read_delim(
    filepath,
    delim = " ",
    col_names= c("x", "y", "value"),
    na = c("", "NA", "1.#QNAN0"),
    progress = FALSE
  ) # There are no actual Nas created – probably because of the # in the name
  
  # Should create a considerable number of NAs
  df$value <- as.numeric(df$value)
  
  # Convert data frame into x3p. Value is measured in microns
  x3p <- x3ptools::df_to_x3p(df)
  
  bullet <- list()
  bullet$x3p <- x3p
  bullet <- bullet %>% dplyr::mutate(
    x3p = x3p %>% purrr::map(.f = function(x) x %>%
                               x3ptools::y_flip_x3p())
  )
}

