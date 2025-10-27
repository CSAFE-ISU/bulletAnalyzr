library(x3ptools)
library(bulletxtrctr)

input_dir <- "examples/Hamby-44/Barrel 1"
output_dir <- "app/tests/testthat/fixtures/hamby44/barrel1"

b1 <- read_bullet(file.path(input_dir, "bullet1"))
b2 <- read_bullet(file.path(input_dir, "bullet2"))

bullets <- rbind(b1, b2)
bullets$x3p <- lapply(bullets$x3p, x3p_sample, m = 5)
bullets$new_file <- file.path(output_dir, basename(dirname(bullets$source)), basename(bullets$source))
mapply(
  x3p_write, 
  bullets$x3p, 
  file = bullets$new_file,
  MoreArgs = list(size = 4)
)
