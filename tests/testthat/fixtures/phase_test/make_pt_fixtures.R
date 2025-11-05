devtools::load_all()

# Make fixtures for relabel_phases()
input <- readRDS(testthat::test_path("fixtures", "phase_test", "filtered_data_for_pt.rds"))
land1 <- input$landA
land2 <- input$landB
score <- input$ccf
input <- data.frame(land1, land2, score)
actual <- relabel_phases(df = input)
saveRDS(actual, testthat::test_path("fixtures", "phase_test", "relabeled_phases.rds"))

# Make fixtures for phase test
input <- readRDS(testthat::test_path("fixtures", "phase_test", "filtered_data_for_pt.rds"))
actual <- phase_test_fixed(land1 = input$landA, land2 = input$landB, score = input$ccf)
saveRDS(actual, testthat::test_path("fixtures", "phase_test", "phase_test.rds"))
