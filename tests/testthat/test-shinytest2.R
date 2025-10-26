library(shinytest2)

shinytest2::load_app_env()

options(rgl.useNULL = TRUE)

test_that("Test app", {
  
  shiny_app <- bulletAnalyzrApp()

  app <- AppDriver$new(
    shiny_app,
    name = "pipeline", 
    height = 674, 
    width = 1139,
    timeout = 240000,
    expect_values_screenshot_args = FALSE  # don't take debug snapshots with expect_values()
  )
  
  # Make list of input names to test. Drops "__shinyscreenshot..." from the list
  inputs <- get_inputs_for_tests(app = app)
  
  app$expect_values(export = TRUE, input = inputs, output = TRUE)  # 1
  
  # Begin button ----
  app$click("begin_button")
  app$wait_for_idle()
  app$expect_values(export = TRUE, input = inputs, output = TRUE)  # 2
  
  # Select Bullet 1 Land x3p Files button ----
  files1 <- list.files(testthat::test_path("fixtures", "hamby44", "barrel1", "bullet2"), full.names = TRUE, pattern = ".x3p")
  app$upload_file(upload_button = files1)
  app$wait_for_idle()
  app$expect_values(export = TRUE, input = inputs, output = TRUE)  # 3
  
  # Name Bullet 1 ----
  app$set_inputs(bul_x3p_name = "Bullet 1")
  app$click("add_to_list_button")
  app$wait_for_idle()
  app$expect_values(export = TRUE, input = inputs, output = TRUE)  # 4
  
  # Select Bullet 2 Land x3p Files button ----
  files2 <- list.files(testthat::test_path("fixtures", "hamby44", "barrel1", "bullet2"), full.names = TRUE, pattern = ".x3p")
  app$upload_file(upload_button = files2)
  app$wait_for_idle()
  app$expect_values(export = TRUE, input = inputs, output = TRUE)  # 5
  
  # Name Bullet 2 ----
  app$set_inputs(bul_x3p_name = "Bullet 2")
  app$click("add_to_list_button")
  app$wait_for_idle()
  app$expect_values(export = TRUE, input = inputs, output = TRUE)  # 6
  
  # Compare Bullets (Upload Bullet tab) ----
  # Finds and displays "optimal" crosscuts on Comparison Report tab
  app$click("doprocess")
  app$wait_for_value(output = "CCBull1")
  app$wait_for_value(input = "cc_bulsel")
  app$set_inputs(cc_bulsel = "Bullet 1")
  app$wait_for_idle()
  app$expect_values(export = TRUE, input = inputs, output = TRUE)  # 7
  
  # Change Bullet 1 Land 4 Crosscut Location ----
  app$set_inputs(CCsl4 = 507)
  app$click("saveCC")
  app$wait_for_idle()
  app$expect_values(export = TRUE, input = inputs, output = TRUE)  # 8
  
  app$set_inputs(cc_bulsel = "Bullet 2")
  app$set_inputs(CCsl1 = 396)
  app$click("saveCC")
  app$wait_for_idle()
  app$expect_values(export = TRUE, input = inputs, output = TRUE)  # 9
  
  # Click Compare Bullets ----
  app$click("doprocessCC")
  app$wait_for_idle()
  app$expect_values(export = TRUE, input = inputs, output = TRUE)  # 10
  
  # Adjust left and right grooves of Bullet 1 Land 1 ----
  app$set_inputs(grooveL = 278)
  app$wait_for_value(output = "grooveSlidersUI")
  app$set_inputs(grooveR = 2230)
  app$wait_for_value(output = "grooveSlidersUI")
  app$click("save_grooves_button")
  app$wait_for_idle()
  app$expect_values(export = TRUE, input = inputs, output = TRUE)  # 11
  
  # Adjust left and right grooves of Bullet 1 Land 4 ----
  app$set_inputs(groove_landsel = "4")
  app$set_inputs(grooveL = 224)
  app$wait_for_value(output = "grooveSlidersUI")
  app$set_inputs(grooveR = 2149)
  app$wait_for_value(output = "grooveSlidersUI")
  app$click("save_grooves_button")
  app$wait_for_idle()
  app$expect_values(export = TRUE, input = inputs, output = TRUE) # 12
  
  # Adjust left and right grooves of Bullet 2 Land 1 and save ----
  app$set_inputs(groove_bulsel = "Bullet 2")
  app$set_inputs(groove_landsel = "1")
  app$set_inputs(grooveL = 212)
  app$wait_for_value(output = "grooveSlidersUI")
  app$set_inputs(grooveR = 2020)
  app$wait_for_value(output = "grooveSlidersUI")
  app$click("save_grooves_button")
  app$wait_for_idle()
  app$expect_values(export = TRUE, input = inputs, output = TRUE)  # 13
  
  # Adjust left and right grooves of Bullet 2 Land 5 and save ----
  app$set_inputs(groove_landsel = "5")
  app$set_inputs(grooveL = 235)
  app$wait_for_value(output = "grooveSlidersUI")
  app$set_inputs(grooveR = 1991)
  app$wait_for_value(output = "grooveSlidersUI")
  app$click("save_grooves_button")
  app$wait_for_idle()
  app$expect_values(export = TRUE, input = inputs, output = TRUE)  # 14
  
  # Click Next Step on grooves page ----
  message("Starting heavy computation - may take 1-2 minutes...")
  app$click("grooves_next_button", wait_ = FALSE)
  log <- app$get_logs()
  print(log)
  saveRDS(log, testthat::test_path("logs", "app_log.rds"))
  
  # Then manually wait for what you need
  app$wait_for_value(input = "comp_bul1")
  app$wait_for_idle()
  
  app$expect_values(export = TRUE, input = inputs)  # 15
  
})
