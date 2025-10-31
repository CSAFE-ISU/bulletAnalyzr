testthat::test_that("get bullet scores wrapper works", {
  input <- readRDS(testthat::test_path("fixtures", "features.rds"))
  actual <- get_bullet_scores_wrapper(features = input)
  
  expected <- readRDS(testthat::test_path("fixtures", "bullet_scores_pre_ss.rds"))
  
  testthat::expect_equal(actual, expected)
})

testthat::test_that("get bullet to land wrapper works", {
  input <- readRDS(testthat::test_path("fixtures", "bullet_scores_pre_ss.rds"))
  actual <- get_bullet_to_land_wrapper(bullet_scores = input)
  
  expected <- readRDS(testthat::test_path("fixtures", "bullet_scores_post_ss.rds"))
  
  testthat::expect_equal(actual, expected)
})
