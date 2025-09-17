library(shinytest2)

# test_that("{shinytest2} recording: compare_downsampled", {
#   app <- AppDriver$new(
#     name = "compare_downsampled", 
#     height = 711, 
#     width = 1299,
#     timeout = 240000,
#     expect_values_screenshot_args = FALSE  # don't take debug snapshots with expect_values()
#   )
#   
#   # Begin button ----
#   app$click("confirm_autonomous")  
#   app$expect_values(export = "allbull_export")
#   
#   # Select Bullet Land x3p Files button ----
#   files1 <- list.files(file.path("fixtures", "Hamby-44", "Barrel 1", "Bullet 1"), full.names = TRUE, pattern = ".x3p")
#   app$upload_file(bul_x3p = files1)  
#   app$set_window_size(width = 1299, height = 711)
#   
#   # Bullet Name text input ----
#   app$set_inputs(bul_x3p_name = "bullet 1")
#   
#   # Add Bullet to Comparison List Button  ----
#   app$click("up_bull")  
#   app$set_window_size(width = 1299, height = 711)
#   app$expect_values(export = "allbull_export")
#   
#   # Select Bullet Land x3p Files button ----
#   files2 <- list.files(file.path("fixtures", "Hamby-44", "Barrel 1", "Bullet 2"), full.names = TRUE, pattern = ".x3p")
#   app$upload_file(bul_x3p = files2)
#   app$set_window_size(width = 1299, height = 711)
#   
#   # Bullet Name text input ----
#   app$set_inputs(bul_x3p_name = "bullet 2")
#   
#   # Add Bullet to Comparison List Button  ----
#   app$click("up_bull")
#   app$set_window_size(width = 1299, height = 711)
#   app$expect_values(export = "allbull_export")
#   
#   # Compare Bullets (On ) button
#   app$click("doprocess")
#   # cc_bulsel exists in output$CCBull1, which isn't rendered until doprocess
#   # creates bulldata$preCC
#   app$wait_for_value(output = "CCBull1")
#   app$set_inputs(cc_bulsel = "bullet 1")
#   app$set_window_size(width = 1299, height = 711)
#   app$expect_values(export = "allbull_export")
#   
#   app$click("saveCC")  # Finalize Crosscut button
#   app$expect_values(export = "allbull_export")
#   app$click("doprocessCC")
#   app$expect_values(export = "allbull_export")
#   # Crosscut sliders exist in output$CCBull2
#   app$wait_for_value(output = "CCBull2")
#   app$set_inputs(`CCsl 1` = 156)
#   app$set_inputs(`CCsl 2` = 156)
#   app$set_inputs(`CCsl 3` = 156)
#   app$set_inputs(`CCsl 4` = 156)
#   app$set_inputs(`CCsl 5` = 206)
#   app$set_inputs(`CCsl 6` = 331)
#   app$set_window_size(width = 1299, height = 711)
#   # app$set_inputs(cc_bulsel = "bullet 2")
#   # app$set_window_size(width = 1299, height = 711)
#   # app$click("saveCC")
#   # app$set_window_size(width = 1299, height = 711)
#   # app$click("doprocessCC")
#   # app$set_window_size(width = 1299, height = 711)
#   # app$expect_values(export = "allbull_export")
# })


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
  app$expect_values(export = "allbull_export")
  
  # Select Bullet Land x3p Files button ----
  files1 <- list.files(file.path("fixtures", "Hamby-44", "Barrel 1", "Bullet 1"), full.names = TRUE, pattern = ".x3p")
  app$upload_file(bul_x3p = files1)
  app$set_window_size(width = 1299, height = 711)
  
  # Bullet Name text input ----
  app$set_inputs(bul_x3p_name = "Bullet 1")
  app$click("up_bull")
  app$set_window_size(width = 1299, height = 711)
  app$expect_values(export = "allbull_export")
  
  # Select Bullet Land x3p Files button ----
  files2 <- list.files(file.path("fixtures", "Hamby-44", "Barrel 1", "Bullet 2"), full.names = TRUE, pattern = ".x3p")
  app$upload_file(bul_x3p = files2)
  app$set_window_size(width = 1299, height = 711)
  
  # Bullet Name text input ----
  app$set_inputs(bul_x3p_name = "Bullet 2")
  app$click("up_bull")
  app$set_window_size(width = 1299, height = 711)
  app$expect_values(export = "allbull_export")
  
  # Compare Bullets (Upload Bullet tab) ----
  # Finds and displays "optimal" crosscuts on Comparison Report tab
  app$click("doprocess")
  app$wait_for_value(output = "CCBull1")
  app$set_inputs(cc_bulsel = "Bullet 1")
  app$set_window_size(width = 1299, height = 711)
  app$expect_values(export = "allbull_export")
  
  app$wait_for_value(output = "CCBull2")
  app$click("saveCC")
  app$click("doprocessCC")
  app$set_window_size(width = 1299, height = 711)
  app$expect_values(export = "allbull_export")
})
