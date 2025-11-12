library(x3ptools)
library(bulletxtrctr)


input_dir <- testthat::test_path("fixtures", "hamby44_in_microns", "barrel1", "bullet2")

bullet <- read_bullet(input_dir)
bullet$x3p[[1]] %>% x3p_get_scale()

bullet <- bullet %>% mutate(
  x3p = x3p %>% purrr::map(.f = x3p_m_to_mum)
)
bullet$x3p[[1]] %>% x3p_get_scale()

lapply(1:6, function(i) {
  x3p_write(bullet$x3p[[i]], file = bullet$source[i])
})
