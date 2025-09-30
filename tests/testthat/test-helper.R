testthat::test_that("is stage works", {
  # Expect TRUE when strict is TRUE - current is last item in stages
  testthat::expect_true(is_stage(current = "upload", stages = c("upload"), strict = TRUE))
  testthat::expect_true(is_stage(current = "crosscut", stages = c("upload", "crosscut"), strict = TRUE))
  testthat::expect_true(is_stage(current = "report", stages = c("upload", "crosscut", "groove", "report"), strict = TRUE))
  
  # Expect FALSE when strict is TRUE - current item is not last item in stages
  testthat::expect_false(is_stage(current = "upload", stages = c("upload", "crosscut"), strict = TRUE))
  testthat::expect_false(is_stage(current = "crosscut", stages = c("upload"), strict = TRUE))
  testthat::expect_false(is_stage(current = "crosscut", stages = c("upload", "crosscut", "groove", "report"), strict = TRUE))
  
  # Expect TRUE when strict is FALSE - current is somewhere in stages
  testthat::expect_true(is_stage(current = "upload", stages = c("upload"), strict = FALSE))
  testthat::expect_true(is_stage(current = "upload", stages = c("upload", "crosscut"), strict = FALSE))
  testthat::expect_true(is_stage(current = "crosscut", stages = c("upload", "crosscut"), strict = FALSE))
  testthat::expect_true(is_stage(current = "crosscut", stages = c("upload", "crosscut", "groove", "report"), strict = FALSE))
  testthat::expect_true(is_stage(current = "report", stages = c("upload", "crosscut", "groove", "report"), strict = FALSE))
  
  # Expect FALSE when strict is FALSE - current isn't anywhere in stages
  testthat::expect_false(is_stage(current = "crosscut", stages = c("upload"), strict = FALSE))
  testthat::expect_false(is_stage(current = "groove", stages = c("upload", "crosscut"), strict = FALSE))
  
})

testthat::test_that("is upload (stage) works", {
  # Expect TRUE
  testthat::expect_true(is_upload(stages = c("upload"), strict = TRUE))
  testthat::expect_true(is_upload(stages = c("upload"), strict = FALSE))
  testthat::expect_true(is_upload(stages = c("upload", "crosscut"), strict = FALSE))
  
  # Expect FALSE
  testthat::expect_false(is_upload(stages = c("crosscut"), strict = FALSE))
  testthat::expect_false(is_upload(stages = c("upload", "crosscut"), strict = TRUE))
  testthat::expect_false(is_upload(stages = c("crosscut", "grooves"), strict = TRUE))
})
