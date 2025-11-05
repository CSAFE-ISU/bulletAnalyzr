testthat::test_that("bullet to land predict fixed works", {
  input <- readRDS(testthat::test_path("fixtures", "process", "bullet_scores_pre_ss.rds"))
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
  input <- readRDS(testthat::test_path("fixtures", "process", "features.rds"))
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

testthat::test_that("relabel phases works", {

  input <- readRDS(testthat::test_path("fixtures", "phase_test", "filtered_data_for_pt.rds"))
  land1 <- input$landA
  land2 <- input$landB
  score <- input$ccf
  input <- data.frame(land1, land2, score)
  actual <- relabel_phases(df = input)
  
  avgs <- actual$avgs
  # Check that the ordered column is 1, 2, ..., n
  testthat::expect_identical(avgs$ordered, 1:nrow(avgs))
  # Check that the max mean is in the last row
  testthat::expect_equal(avgs$means[nrow(avgs)], max(avgs$means))
  # Check that the max mean is 0.865379639
  testthat::expect_equal(avgs$means[nrow(avgs)], 0.865379639)
  
  # Recalculate the means from the data frame and check that the highest
  # numbered phase corresponds to the highest mean
  avgs_new <- actual$df %>%
    dplyr::group_by(phase) %>%
    dplyr::summarize(means = mean(score))
  testthat::expect_equal(avgs_new$means[max(avgs_new$phase)], max(avgs_new$means))
  
  # Check that the output is identical to the saved fixture
  expected <- readRDS(testthat::test_path("fixtures", "phase_test", "relabeled_phases.rds"))
  testthat::expect_identical(actual, expected)
})

testthat::test_that("phase test fixed works", {
  input <- readRDS(testthat::test_path("fixtures", "phase_test", "filtered_data_for_pt.rds"))
  actual <- phase_test_fixed(
    land1 = input$landA,
    land2 = input$landB,
    score = input$ccf
  )
  
  expected <- readRDS(testthat::test_path("fixtures", "phase_test", "phase_test.rds"))
  
  testthat::expect_identical(actual, expected)
})
