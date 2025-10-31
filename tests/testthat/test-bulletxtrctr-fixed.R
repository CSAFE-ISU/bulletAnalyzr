testthat::test_that("bullet to land predict fixed works", {
  input <- readRDS(testthat::test_path("fixtures", "bullet_scores_pre_ss.rds"))
  input <- input$data[[2]]
  actual <- bullet_to_land_predict_fixed(
    land1 = input$landA,
    land2 = input$landB,
    scores = input$rfscore
  )
  
  expected <- rep(FALSE, 36)
  expected[c(5, 12, 13, 20, 27, 34)] <- TRUE
  
  testthat::expect_identical(actual, expected)
  
})

testthat::test_that("compute average scores fixed works", {
  input <- readRDS(testthat::test_path("fixtures", "features.rds"))
  input <- input %>% dplyr::group_by(bulletA, bulletB) %>% tidyr::nest()
  input <- input$data[[1]]
  actual <- compute_average_scores_fixed(
    land1 = input$landA, 
    land2 = input$landB, 
    score = input$rfscore
  )
  
  expected <- c(0.917777777777778, 0.137222222222222, 0.292777777777778, 
                0.186666666666667, 0.292777777777778, 0.137222222222222)
  
  testthat::expect_equal(actual, expected)
})
