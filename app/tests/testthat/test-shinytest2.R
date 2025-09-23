library(shinytest2)


test_that("{shinytest2} recording: app", {
  app <- AppDriver$new(
    name = "app", 
    height = 711, 
    width = 1299,
    timeout = 240000,
    expect_values_screenshot_args = FALSE  # don't take debug snapshots with expect_values()
  )
  
  # Begin button ----
  app$click("confirm_autonomous")
  app$expect_values(export = TRUE)
  
  # Select Bullet Land x3p Files button ----
  files1 <- list.files(file.path("fixtures", "Hamby-44", "Barrel 1", "Bullet 1"), full.names = TRUE, pattern = ".x3p")
  app$upload_file(bul_x3p = files1)
  app$set_window_size(width = 1299, height = 711)
  
  # Bullet Name text input ----
  app$set_inputs(bul_x3p_name = "Bullet 1")
  app$click("up_bull")
  app$set_window_size(width = 1299, height = 711)
  app$expect_values(export = TRUE)
  
  # Select Bullet Land x3p Files button ----
  files2 <- list.files(file.path("fixtures", "Hamby-44", "Barrel 1", "Bullet 2"), full.names = TRUE, pattern = ".x3p")
  app$upload_file(bul_x3p = files2)
  app$set_window_size(width = 1299, height = 711)
  
  # Bullet Name text input ----
  app$set_inputs(bul_x3p_name = "Bullet 2")
  app$click("up_bull")
  app$set_window_size(width = 1299, height = 711)
  app$expect_values(export = TRUE)
  
  # Compare Bullets (Upload Bullet tab) ----
  # Finds and displays "optimal" crosscuts on Comparison Report tab
  app$click("doprocess")
  app$wait_for_value(output = "CCBull1")
  app$set_inputs(cc_bulsel = "Bullet 1")
  app$set_window_size(width = 1299, height = 711)
  app$expect_values(export = TRUE)
  
  app$wait_for_value(output = "CCBull2")
  app$click("saveCC")
  app$click("doprocessCC")
  app$set_window_size(width = 1299, height = 711)
  app$expect_values(export = TRUE)
})
