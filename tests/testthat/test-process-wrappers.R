test_that("get bullet scores wrapper works", {
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
  
  expect_equal(actual, expected)
})
