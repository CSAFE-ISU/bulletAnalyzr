testthat::test_that("New report is identical to old report", {
  # I can't get shinytest2 to run the report module. As a (cumbersome)
  # work-around: (1) run bulletAnalyzrApp(run_interactive = FALSE) (2) upload the hamby44 bullets in the tests > testthat > fixtures folder and click Compare Bullets (3) download the report data (4) rename the report data "new-report.rds" and move it to the fixtures folder.
  
  old <- readRDS(testthat::test_path("fixtures", "report.rds"))
  new <- readRDS(testthat::test_path("fixtures", "new-report.rds"))
  
  testthat::expect_identical(new, old)
  
})
