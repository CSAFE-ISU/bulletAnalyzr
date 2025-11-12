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

# main_dir <- "/Volumes/T7_Shield/CSAFE/datasets/bullet_datasets/Hamby Set 44 Rescan/Barrel 1/Bullet 2"
# main_dir <- "/Volumes/T7_Shield/CSAFE/datasets/bullet_datasets/cts/test_22-5261_sample_F1/Item 1/Bullet B"
main_dir <- testthat::test_path("fixtures", "hamby44", "barrel1", "bullet1")
# Load dat and convert to x3p to view
d1 <- load_from_dat(main_dir)
x3p_image(d1$x3p[[3]])

# Load from x3p to view
b1 <- read_bullet(main_dir)
x3p_image(b1$x3p[[1]])
x3p_get_scale(b1$x3p[[6]])

b2 <- read_bullet(main_dir)
x3p_image(b2$x3p[[1]])
x3p_get_scale(b2$x3p[[6]])
