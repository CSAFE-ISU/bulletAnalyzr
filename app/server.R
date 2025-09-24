# Load Libraries ----
library(shiny)
library(shinyjs)
library(shinyBS)
library(ggplot2)
library(bslib)
library(bsicons)
library(shinycssloaders)
library(randomForest)
library(dplyr)
library(DT)

# Load Bullet Libraries ----
library(rgl)
library(x3ptools) # remotes::install_github("heike/x3ptools")
library(bulletxtrctr) # remotes::install_github("heike/bulletxtrctr")

# Force use of chromote ----
library(pagedown)
library(curl) # for webshot

# Config ----
options(rgl.useNULL = TRUE)
options(shiny.maxRequestSize = 150*1024^2)
addResourcePath("images", "images")
theme_set(theme_bw())
theme_update(
  text = element_text(size = 22), 
  plot.title = element_text(size = 22, face = "bold")
)
interactive_cc = TRUE

# Helper Functions ----
source("R/bullet-lists.R")
source("R/helper.R")
source("R/plot.R")
source("R/preprocess.R")
source("R/process-wrappers.R")
source("R/render.R")
source("R/report-module.R")


# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  
  # OUTPUT - Session Info - Report versions of packages used ----
  output$sessionInfo <- render_session_info(session)
  
  # REACTIVE VALUES - Switch alerts on/off ----
  values <- reactiveValues(show_alert = TRUE)
  
  # REACTIVE VALUES - Bullet and comparison data ----
  bulldata <- reactiveValues(
    allbull = data.frame(),
    allbull_export = data.frame(),
    cbull = data.frame(),
    cbull_export = data.frame(),
    preCC = NULL,
    preCC_export = NULL,
    postCC = NULL, 
    postCC_export = NULL,
    comparison = NULL,
    comparison_export = NULL
  )
  
  # REACTIVE VALUES - Phase test results ----
  phase <- reactiveValues(
    test_results = NULL
  )
  
  # EXPORT VALUES - For shinytest2 tests ----
  exportTestValues(
    allbull_export = bulldata$allbull_export,
    cbull_export = bulldata$cbull_export,
    comparison_export = bulldata$comparison_export,
    phase_test_export = phase$test_results,
    postCC_export = bulldata$postCC_export,
    preCC_export = bulldata$preCC_export,
    show_alert_export = values$show_alert
  )
  
  # BUTTON - Begin ----
  observeEvent(input$begin_button, {
    updateTabsetPanel(session, "prevreport", selected = "Upload Bullet")
  })
  
  
  # SECTION: UPLOAD BULLET TAB -----------------------------------------------
  
  # OUTPUT UI - Select bullet lands sidebar ----
  output$bul_x3pui <- renderUI({
    # Button - Bullet Land x3p Files ----
    fileInput("upload_button", "Select Bullet Land x3p files", accept = ".x3p", multiple = TRUE)
  })
  
  # OBSERVE EVENT - Bullet Land x3p Files button ----
  observeEvent(input$upload_button, {
    
    disable("add_to_list_button")
    
    progress <- shiny::Progress$new(); on.exit(progress$close())
    
    # Get default bullet name ----
    bullet_name <- identify_bullet(input$upload_button$name)
    updateTextInput(session, "bul_x3p_name", value = bullet_name)
    
    # Switch alert on ----
    values$show_alert <- TRUE
    
    temp_refresh <- input$prevreport
    
    # Create Temporary Directory and save bullets in it ----
    temp_dir <- copy_to_tempdir(
      filepath = input$upload_button$datapath,
      filename = input$upload_button$name
    )
    
    # Read bullet from temp directory ----
    progress$set(message = "Reading Bullet", value = .25)
    bull <- read_bullet(temp_dir)
    
    # Rotate bullet (optional) ----
    rotate_results <- rotate_bullet(
      bullet = bull, 
      show_alert = values$show_alert, 
      session = session
    )
    bull <- rotate_results$bullet
    values$show_alert <- rotate_results$show_alert
    
    # Down-sample bullet (optional) ----
    downsample_results <- downsample_bullet(
      allbull = bulldata$allbull,
      bullet = bull,
      show_alert = values$show_alert,
      session = session
    )
    bulldata$allbull <- downsample_results$allbull
    values$show_alert <- downsample_results$show_alert
    
    # Convert to microns (optional) ----
    bull$x3p <- lapply(bull$x3p, cond_x3p_m_to_mum)
    
    # Get hash ----
    bull$md5sum <- tools::md5sum(bull$source)
    
    # Get names ----
    bull$filename <- basename(bull$source)
    bull$land_names <- identify_lands(bull$filename)
    bull$bullet_name <- identify_bullet(bull$filename)
    
    # Store current bullet ----
    bulldata$cbull <- bull
    bulldata$cbull_export <- make_export_df(df = bulldata$cbull)
    
  })
  
  # OBSERVE EVENT - Add Bullet to Comparison List button ----
  # Push current bullet data to all bullet data object
  observeEvent(input$add_to_list_button, {
    req(nrow(bulldata$cbull) > 0)
    
    bulldata$allbull <- add_cbull_to_allbull(
      cbull = bulldata$cbull,
      bul_x3p_name = input$bul_x3p_name,
      allbull = bulldata$allbull
    )
    bulldata$allbull_export <- make_export_df(df = bulldata$allbull)
    
    # Switch upload button off ----
    disable("add_to_list_button")
  })
  
  
  # SECTION: BULLET PREVIEWS ON UPLOAD TAB --------------------------------
  
  # OUTPUT UI - Upload Bullet tab panel ----
  output$lpupload <- renderUI({
    req(nrow(bulldata$cbull) > 0)
    
    progress <- shiny::Progress$new(); on.exit(progress$close())
    
    # Render bullet ----
    progress$set(message = "Rendering Previews", value = .75)
    for(idx in 1:nrow(bulldata$cbull)) {
      local({
        cidx <- idx
        # OUTPUT RGL - Bullet ----
        output[[paste0("x3prgl",idx)]] <- renderRglwidget({
          render_land(
            x3p = bulldata$cbull$x3p[[cidx]], 
            ccut = NULL,
            sample_m = 5,
            rotate = TRUE,
            img_size = 500,
            img_zoom = 0.4
          )
          rglwidget()
        })
      })
    }
    
    # Enable upload button ----
    enable("add_to_list_button")
    
    # Display bullet ----
    layout_column_wrap(
      width = 1/6,
      !!!lapply(1:nrow(bulldata$cbull), FUN = function(x) parse_rglui(x, name = "x3prgl", land_name = bulldata$cbull$land_names[x]))
    )
  })
  
  # SECTION: PREVIEW BULLET TAB --------------------------------------------
  
  # OUTPUT UI - Preview Bullet sidebar ----
  output$prevSelUI <- renderUI({
    req(nrow(bulldata$allbull) > 0)
    
    # Store allbul ----
    allbull <- bulldata$allbull
    
    # Drop-down - Preview Bullet ----
    selectInput(
      "prev_bul", 
      "Preview Bullet", 
      choices = unique(allbull$bullet), 
      selected = NULL, 
      multiple = FALSE
    )
  })
  
  # OUTPUT UI - Preview Bullet tab panel ----
  output$lpreview <- renderUI({
    req(nrow(bulldata$allbull) > 0)
    req(length(input$prev_bul) > 0)
    
    progress <- shiny::Progress$new(); on.exit(progress$close())
    
    # Refresh on tab change ----
    temp_refresh <- input$prevreport
    
    # Get selected bullet ----
    bull <- fiter_preview_bullet(
      allbull = bulldata$allbull,
      preview_bull_name = input$prev_bul
    )
    
    # Render selected bullet ----
    progress$set(message = "Rendering Previews", value = .75)
    for(idx in 1:nrow(bull)) {
      local({
        cidx <- idx
        # OUTPUT RGL - Bullet ----
        output[[paste0("x3prglprev",idx)]] <- renderRglwidget({
          render_land(
            x3p = bull$x3p[[cidx]], 
            ccut = NULL,
            sample_m = 5,
            rotate = TRUE,
            img_size = 500,
            img_zoom = 0.4
          )
          rglwidget()
        })
      })
    }
    
    # Display selected bullet ----
    layout_column_wrap(
      width = 1/6, 
      !!!lapply(1:nrow(bull), FUN = function(x) parse_rglui(x, name = "x3prglprev", land_name = bull$land_names[x]))
    )
  })
  
  
  # SECTION: SELECT BULLETS FOR COMPARISON --------------------------------
  
  # OUTPUT UI - Select Bullets to Compare sidebar ----
  output$bullSelCheckboxUI <- renderUI({
    req(nrow(bulldata$allbull) > 0)
    
    # Store allbull ----
    allbull <- bulldata$allbull
    
    # CHECK BOX - Select Bullets to Compare ----
    checkboxGroupInput(
      "bull_sel_checkbox",
      label = "Select Bullets to Compare", 
      choices = unique(bulldata$allbull$bullet),
      selected = unique(bulldata$allbull$bullet)
    )
  })
  
  # OBSERVE EVENT - Compare Bullets button (Upload Bullet Tab) ----
  observeEvent(input$doprocess, {
    req(length(input$bull_sel_checkbox) > 0)
    
    values$show_alert <- FALSE
    progress <- shiny::Progress$new(); on.exit(progress$close())
    
    bullets <- bulldata$allbull
    
    # Find optimal crosscuts ----
    progress$set(message = "Get suitable Cross Sections", value = 0)
    # If interactive_cc = TRUE, crosscut results added to preCC and postCC is
    # NULL. If interactive_cc = FALSE, crosscut results added to postCC and preCC is NULL.
    crosscut_results <- get_default_cc_wrapper(
      bullets = bullets,
      interactive_cc = interactive_cc,
      ylimits = c(150, NA)
    )
    bulldata$preCC <- crosscut_results$preCC
    bulldata$preCC_export <- make_export_df(df = bulldata$preCC)
    bulldata$postCC <- crosscut_results$postCC
    bulldata$postCC_export <- make_export_df(df = bulldata$postCC)
    
    # Switch to Comparison Report tab panel ----
    updateTabsetPanel(session, "prevreport", selected = "Comparison Report")
  })
  
  # OBSERVE EVENT - bulldata$postCC - Get crosscut data, grooves, signal, features, and random forest score ---- 
  
  # If interactive_cc = TRUE, bulldata$postCC is populated when the Compare
  # Bullets button (doprocessCC) on the Comparison Report tab panel is clicked.
  # If interactive_cc = FALSE, bulldata$postCC is populated when (doprocess) is
  # clicked
  observeEvent(bulldata$postCC, {
    req(bulldata$postCC)
    
    progress <- shiny::Progress$new(); on.exit(progress$close())
    
    # Extract crosscut data ----
    bullets <- get_ccdata_wrapper(postCC = bulldata$postCC, progress = progress)
    
    # Find the optimal groove locations ----
    bullets <- get_grooves_wrapper(bullets = bullets, progress = progress)
    
    # Extract the signals ----
    bullets <- get_signals_wrapper(bullets = bullets, progress = progress)
    
    # Align the signals ----
    signals_results <- get_aligned_signals_wrapper(bullets = bullets, progress = progress)
    bullets <- signals_results$bullets
    comparisons <- signals_results$comparisons
    
    # Get Resolution ----
    resolution <- x3p_get_scale(bullets$x3p[[1]])
    
    # Get Features ----
    features_results <- get_features_wrapper(comparisons = comparisons, resolution = resolution, progress = progress)
    comparisons <- features_results$comparisons
    features <- features_results$features
    
    # Predict random forest scores ----
    progress$set(message = "Predicting RandomForest Scores", value = .45)
    features$rfscore <- predict(rtrees, newdata = features, type = "prob")[,2]
    
    # Calculate bullet scores ----
    bullet_scores <- get_bullet_scores_wrapper(features = features, progress = progress)
    
    # Denote same source ----
    # just get the 'best phase' not just ones that are 'matches'
    bullet_scores$data <- lapply(
      bullet_scores$data,
      function(d) cbind(d, samesource = bullet_to_land_predict(land1 = d$landA, land2 = d$landB, d$rfscore, alpha = .9, difference = 0.01))
    )
    
    # Render lands with crosscuts snapshot ----
    bullets$x3pimg <- NA
    
    # Store comparison report data ----
    report_results <- get_report_data_wrapper(
      bullets = bullets,
      comparisons = comparisons,
      features = features,
      bullet_scores = bullet_scores,
      progress = progress
    )
    bulldata$comparison <- report_results$comparison
    bulldata$comparison_export <- report_results$comparison_export
  })
  
  
  # SECTION: CROSSCUT INTERACTIVITY ---------------------------------------
  
  # OUTPUT UI - Report Crosscut sidebar 1 ----
  output$CCBull1 <- renderUI({
    req(bulldata$preCC)
    
    # DROP-DOWN - Report Select Bullet ----
    bullets <- bulldata$preCC
    selectInput("cc_bulsel", "Select Bullet", choices = unique(bullets$bullet), selected = NULL, multiple = FALSE)
  })
  
  # OUTPUT UI - Report Crosscut sidebar 2 ----
  output$CCBull2 <- renderUI({
    req(bulldata$preCC)
    req(input$cc_bulsel)
    
    # Filter selected bullet ----
    bullets <- filter_selected_bullet(bullets = bulldata$preCC, selected = input$cc_bulsel)
    
    # Calculate Y coordinate ranges for each bullet land in microns
    bullet_y_ranges <- get_max_microns(bullets = bullets)
    
    # Render crosscut sliders and Finalize Crosscut and Compare Bullets buttons ----
    list(
      # Render crosscut sliders ----
      mapply(render_ccsl, id = 1:nrow(bullets), ymin = 0, ymax = bullet_y_ranges, yset = bullets$crosscut, SIMPLIFY = FALSE),
      # BUTTON - Finalize Crosscut ----
      fluidRow(column(12, actionButton("saveCC", label = "Finalise CrossCut"), align="center")),
      hr(),
      # BUTTON - Compare Bullets ----
      fluidRow(column(12, actionButton("doprocessCC", label = "Compare Bullets"), align="center"))
    )
  })
  
  # OBSERVE EVENT - Finalize Crosscut button ----
  
  observeEvent(input$saveCC,{
    req(bulldata$preCC)
    
    bullets <- bulldata$preCC
    
    # Update crosscut column in bullets data frame with crosscut sliders ----
    bullets <- update_cc_from_slider_wrapper(
      bullets = bullets, 
      selected = input$cc_bulsel,
      all_inputs = reactiveValuesToList(input)
    )
    
    # Store bullets with crosscut locations ----
    bulldata$preCC <- bullets
    bulldata$preCC_export <- make_export_df(df = bullets)
  })
  
  # OBSERVE EVENT - Compare Bullets button ----
  observeEvent(input$doprocessCC,{
    req(bulldata$preCC)
    
    # Push preCC data frame to postCC ----
    bullets <- bulldata$preCC
    bulldata$postCC <- bullets
    bulldata$postCC_export <- make_export_df(bullets)
    
    # Reset preCC to NULL ----
    bulldata$preCC <- NULL
    bulldata$preCC_export <- NULL
  })
  
  # OUTPUT UI - Crosscuts on Report tab panel ----
  output$CCBullLand <- 	renderUI({
    req(bulldata$preCC)
    req(input$cc_bulsel)
    
    # Filter selected bullet ----
    bullets <- filter_selected_bullet(bullets = bulldata$preCC, selected = input$cc_bulsel)
    
    # Refresh tab on change ----
    temp_refresh <- input$prevreport
    
    # Render lands with crosscuts ---- 
    for(idx in 1:nrow(bullets)) {
      local({
        cidx <- idx
        # OUTPUT RGL - Render lands with crosscuts ----
        output[[paste0("CC_Sel_",idx)]] <- renderRglwidget({
          render_land(
            x3p = bullets$x3p[[cidx]],
            ccut = input[[paste("CCsl",cidx)]],
            rotate = TRUE,
            sample_m = 5,
            img_size = 500,
            img_zoom = 0.4
          )
          rglwidget()
        })
      })
    }
    
    # Display lands with crosscuts ---- 
    layout_column_wrap(
      width = 1/6, 
      !!!lapply(1:nrow(bullets), FUN = function(x) parse_rglui(x, name = "CC_Sel_", land_name = NULL))
    )
  })
  
  
  # SECTION: GENERATE REPORT ----------------------------------------------
  
  # OUTPUT UI - Report Comparison sidebar ----
  output$reportSelUI <- renderUI({
    req(is.null(bulldata$preCC))
    req(bulldata$comparison)
    
    all_bullets <- unique(bulldata$comparison$bullet_scores$bulletA)
    list(
      # DROP-DOWN - Compare Bullet ----
      selectInput("comp_bul1", "Compare Bullet", choices = all_bullets, selected = all_bullets[1]),
      # DROP-DOWN - With Bullet ----
      selectInput("comp_bul2", "With Bullet", choices = all_bullets, selected = all_bullets[2]),
      hr()
    )
  })
  
  # MODULE - reportServer ----
  reportServer(
    "report1", 
    bullet_data = bulldata, 
    comp_bul1 = reactive(input$comp_bul1), 
    comp_bul2 = reactive(input$comp_bul2),
    phase_test_results = phase$test_results
  )
  
  # SECTION: PHASE TEST ---------------------------------------------------
  
  observe({
    req(bulldata$comparison)
    req(bulldata$comparison$bullet_scores)
    req(input$comp_bul1)
    req(input$comp_bul2)
    
    bullet_scores <- bulldata$comparison$bullet_scores
    bullet_scores$selsource <- FALSE
    bullet_scores$selsource[bullet_scores$bulletA == input$comp_bul1 & bullet_scores$bulletB == input$comp_bul2] <- TRUE
    d <- bullet_scores %>% filter(selsource) %>% tidyr::unnest(data)
    
    tryCatch({
      phase$test_results <- bulletxtrctr:::phase_test(land1 = d$landA, land2 = d$landB, d$ccf)
    }, error = function(e) {
      return(d)
    })
  })
  
}
