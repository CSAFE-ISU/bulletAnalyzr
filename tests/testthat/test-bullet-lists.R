testthat::test_that("filter bulletA and bulletB columns works", {

  df <- readRDS(testthat::test_path("fixtures", "bulA_bulB_df.rds"))
  actual <- filter_bulletA_bulletB_cols(
    df = df,
    selected1 = "b1",
    selected2 = "b2",
    unnest_data = NULL
  )
  
  testthat::expect_equal(nrow(actual), 36)
  testthat::expect_equal(actual$bulletA, rep("b1", 36))
  testthat::expect_equal(actual$bulletB, rep("b2", 36))
  
})

testthat::test_that("filter bulletA and bulletB columns works on nested data", {
  # Load example data frame with bulletA and bulletB columns and nested column data
  df <- readRDS(testthat::test_path("fixtures", "bulA_bulB_nested_df.rds"))
  
  # Run function without unnesting
  actual1 <- filter_bulletA_bulletB_cols(
    df = df,
    selected1 = "b1",
    selected2 = "b2",
    unnest_data = NULL
  )
  
  # Run function with unnesting
  actual2 <- filter_bulletA_bulletB_cols(
    df = df,
    selected1 = "b1",
    selected2 = "b2",
    unnest_data = "data"
  )
  
  testthat::expect_equal(nrow(actual1), 1)
  testthat::expect_equal(actual1$bulletA, c("b1"))
  testthat::expect_equal(actual1$bulletB, c("b2"))
  
  testthat::expect_equal(nrow(actual2), 36)
  testthat::expect_equal(actual2$bulletA, rep("b1", 36))
  testthat::expect_equal(actual2$bulletB, rep("b2", 36))
  
})
