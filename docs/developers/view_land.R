library(dplyr)
library(bulletxtrctr)
library(x3ptools)
library(rgl)
library(randomForest)
library(ggplot2)
library(readr)
library(nbtrd)


# cts <- read_bullet("/Users/stephanie/Documents/cts_set/scans/test_19-527_sample_F2/Item 1/Bullet A")
# image_x3p(cts$x3p[[1]])
# snapshot3d(filename ="~/Documents/CTS19.1.A.png")
# 
# hamby <- read_bullet("/Users/stephanie/Documents/Hamby Set 44 Rescan Preprocessed/Barrel 1/Bullet 1")
# image_x3p(hamby$x3p[[1]])
# snapshot3d(filename ="~/Documents/hamby44.1.2.png")

main_dir <- "/Users/stephanie/Documents/Bullet_scans/St Louis"

files <- list.files(main_dir, pattern = ".dat", full.names = TRUE)
df <- readr::read_delim(
  files[2],
  delim = " ",
  col_names= c("x", "y", "value"),
  na = c("", "NA", "1.#QNAN0"),
  progress = FALSE
) # There are no actual Nas created – probably because of the # in the name

# Should create a considerable number of NAs
df$value <- as.numeric(df$value)

# Convert data frame into x3p. Value is measured in microns
x3p <- x3ptools::df_to_x3p(df)
x3p_image(x3p)

# b1 <- read_bullet(main_dir)
# x3p_image(b1$x3p[[1]])
