library(dplyr)
library(bulletxtrctr)
library(x3ptools)
library(rgl)
library(randomForest)
library(ggplot2)
library(readr)
library(nbtrd)

load_from_dat <- function(main_dir) {
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

main_dir <- "/Users/stephanie/Downloads/phoenix_example"

# Load dat and convert to x3p to view
b1 <- load_from_dat(main_dir)
x3p_image(b1$x3p[[1]])

# Load from x3p to view
b1 <- read_bullet(main_dir)
x3p_image(b1$x3p[[1]])
