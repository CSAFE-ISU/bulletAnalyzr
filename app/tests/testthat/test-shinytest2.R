library(shinytest2)

test_that("{shinytest2} recording: upload", {
  app <- AppDriver$new(
    variant = platform_variant(),
    name = "upload",
    height = 711,
    width = 1299
  )
  app$expect_values(export = "show_alert")
  app$expect_values(export = "allbull")

  # Click begin ----
  app$click("confirm_autonomous")

  # Upload bullet 1 ----
  files <- list.files(
    file.path("fixtures", "Hamby-44", "barrel 1", "Bullet 1"), full.names = TRUE
  )
  app$upload_file(bul_x3p = files)
  app$set_window_size(width = 1299, height = 711)
  app$expect_values(export = "show_alert")
  app$expect_values(export = "allbull")

  # Set bullet 1 name ----
  app$set_inputs(bul_x3p_name = "Bullet 1")
  app$expect_values(export = "show_alert")
  app$expect_values(export = "allbull")

  # Add bullet 1 to comparison list ----
  app$click("up_bull")
  app$set_window_size(width = 1299, height = 711)
  app$expect_values(export = "show_alert")
  app$expect_values(export = "allbull")
})
