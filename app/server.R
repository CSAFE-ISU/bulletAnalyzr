# Load Libraries
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

# Load Bullet Libraries
library(rgl)
library(x3ptools) # remotes::install_github("heike/x3ptools")
library(bulletxtrctr) # remotes::install_github("heike/bulletxtrctr")

# Force use of chromote
library(pagedown)
library(curl) # for webshot

# Config
options(rgl.useNULL = TRUE)
options(shiny.maxRequestSize = 150*1024^2)
addResourcePath("images", "images")
theme_set(theme_bw())
theme_update(
  text = element_text(size = 22), 
  plot.title = element_text(size = 22, face = "bold")
)
interactive_cc = TRUE
sample_m = 10

# Helper Functions
source("R/bullet-lists.R")
source("R/helper.R")
source("R/plot.R")
source("R/preprocess.R")
source("R/process-wrappers.R")
source("R/render.R")
source("R/report-module.R")


# Server--------------------------------------------------------------
server <- function(input, output, session) {
  
  # OUTPUT - Session Info - Report versions of packages used
  output$sessionInfo <- render_session_info(session)
  
  # REACTIVE VALUES - Switch alerts on/off
  values <- reactiveValues(show_alert = TRUE)
  
  # REACTIVE VALUES - Bullet and comparison data
  bulldata <- reactiveValues(
    stage = NULL,
    allbull = data.frame(),
    allbull_export = data.frame(),
    cbull = NULL,
    cbull_export = NULL,
    cbull_name = NULL,
    preCC = NULL,
    preCC_export = NULL,
    postCC = NULL, 
    postCC_export = NULL,
    comparison = NULL,
    comparison_export = NULL
  )
  
  # REACTIVE VALUES - Phase test results
  phase <- reactiveValues(
    test_results = NULL
  )
  
  # EXPORT VALUES - For shinytest2 tests
  exportTestValues(
    allbull_export = bulldata$allbull_export,
    cbull_export = bulldata$cbull_export,
    cbull_name_export = bulldata$cbull_name,
    comparison_export = bulldata$comparison_export,
    phase_test_export = phase$test_results,
    postCC_export = bulldata$postCC_export,
    preCC_export = bulldata$preCC_export,
    show_alert_export = values$show_alert,
    stage_export = bulldata$stage
  )
  
  
  # SECTION: WELCOME TAB----------------------------------------------------
  
  # BUTTON - Begin Button
  observeEvent(input$begin_button, {
    bulldata$stage <- c("upload")
    updateTabsetPanel(session, "prevreport", selected = "Upload Bullet")
  })
  

  # SECTION: UPLOAD BULLET TAB - CBULL --------------------------------------
  
  # OUTPUT UI - Upload Land x3p Files Button
  output$bul_x3pui <- renderUI({
    req(is_upload(bulldata$stage))
    
    # Button - Bullet Land x3p Files
    fileInput("upload_button", "Select Bullet Land x3p files", accept = ".x3p", multiple = TRUE)
  })
  
  # OBSERVE EVENT - Bullet Land x3p Files Button
  # Preprocess uploaded x3p files and push to cbull
  observeEvent(input$upload_button, {
    req(is_upload(bulldata$stage))
    
    disable("add_to_list_button")
    
    progress <- shiny::Progress$new(); on.exit(progress$close())
    
    # Get default bullet name
    bullet_name <- identify_bullet(input$upload_button$name)
    updateTextInput(session, "bul_x3p_name", value = bullet_name)
    
    # Switch alert on
    values$show_alert <- TRUE
    
    temp_refresh <- input$prevreport
    
    # Create Temporary Directory and save bullets in it
    temp_dir <- copy_to_tempdir(
      filepath = input$upload_button$datapath,
      filename = input$upload_button$name
    )
    
    # Read bullet from temp directory
    progress$set(message = "Reading Bullet", value = .25)
    cbull <- read_bullet(temp_dir)
    
    preprocess_results <- preprocess_bullet(
      allbull = bulldata$allbull,
      cbull = cbull,
      show_alert = values$show_alert,
      progress = progress,
      session = session
    )
    bulldata$allbull <- preprocess_results$allbull
    bulldata$cbull <- preprocess_results$cbull
    bulldata$cbull_export <- make_export_df(df = bulldata$cbull)
    
  })
  
  # OUTPUT UI - Display Lands on Upload Tab
  output$lpupload <- renderUI({
    req(is_upload(bulldata$stage))
    req(isTruthy(bulldata$cbull) || isTruthy(bulldata$cbull_name))
    
    progress <- shiny::Progress$new(); on.exit(progress$close())
    
    if (!is.null(bulldata$cbull)) {
      cbull <- bulldata$cbull
    } else if (!is.null(bulldata$cbull_name) & (nrow(bulldata$allbull) > 0)) {
      cbull <- filter_selected_bullet(
        bullets = bulldata$allbull,
        selected = bulldata$cbull_name
      )
    } else {
      warning("Current bullet data frame not found for rendering RGLs")
    }
    
    # Render bullet
    progress$set(message = "Rendering Previews", value = .75)
    for(idx in 1:nrow(cbull)) {
      local({
        cidx <- idx
        # OUTPUT RGL - Bullet
        output[[paste0("x3prgl",idx)]] <- renderRglwidget({
          render_land(
            x3p = cbull$x3p[[cidx]], 
            ccut = NULL,
            sample_m = sample_m,
            rotate = TRUE,
            img_size = 500,
            img_zoom = 0.4
          )
          rglwidget()
        })
      })
    }
    
    # Enable upload button
    enable("add_to_list_button")
    
    # Display bullet
    layout_column_wrap(
      width = 1/6,
      !!!lapply(1:nrow(cbull), FUN = function(x) parse_rglui(x, name = "x3prgl", land_name = cbull$land_names[x]))
    )
  })
  

  # SECTION: UPLOAD BULLET TAB - ALLBULL ------------------------------------
  
  # OBSERVE EVENT - Add Bullet to Comparison List button
  # Push current bullet data to all bullet data object
  observeEvent(input$add_to_list_button, {
    req(is_upload(bulldata$stage))
    req(bulldata$cbull)
    
    # Add bullet and land columns to current bullet
    cbull <- bulldata$cbull
    cbull$bullet <- input$bul_x3p_name
    cbull$land <- factor(cbull$land_names, levels = cbull$land_names)
    
    bulldata$allbull <- add_cbull_to_allbull(
      cbull = cbull,
      cbull_name = cbull$bullet[1],
      allbull = bulldata$allbull
    )
    bulldata$allbull_export <- make_export_df(df = bulldata$allbull)
    
    # Store cbull name and reset cbull
    bulldata$cbull_name <- cbull$bullet[1]
    bulldata$cbull <- NULL
    bulldata$cbull_export <- NULL
    
    # Switch upload button off
    disable("add_to_list_button")
  })
  
  # OUTPUT UI - Select Bullets to Compare Checkbox
  output$bullSelCheckboxUI <- renderUI({
    req(is_upload(bulldata$stage))
    req(nrow(bulldata$allbull) > 0)
    
    # Store allbull
    allbull <- bulldata$allbull
    
    # CHECK BOX - Select Bullets to Compare
    checkboxGroupInput(
      "bull_sel_checkbox",
      label = "Select Bullets to Compare", 
      choices = unique(bulldata$allbull$bullet),
      selected = unique(bulldata$allbull$bullet)
    )
  })
  
  # OBSERVE EVENT - Compare Bullets button (Upload Bullet Tab) - Get default
  # crosscuts before starting interactivity
  observeEvent(input$doprocess, {
    req(is_upload(bulldata$stage))
    req(length(input$bull_sel_checkbox) > 0)
    
    values$show_alert <- FALSE
    progress <- shiny::Progress$new(); on.exit(progress$close())
    
    bullets <- bulldata$allbull
    
    # Find optimal crosscuts
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
    
    # Switch to Comparison Report tab panel
    bulldata$stage <- c("upload", "crosscut")
    updateTabsetPanel(session, "prevreport", selected = "Comparison Report")
  })
  
  
  # SECTION: PREVIEW BULLET TAB----------------------------------------
  
  # OUTPUT UI - Preview Bullet sidebar
  output$prevSelUI <- renderUI({
    req(nrow(bulldata$allbull) > 0)
    
    # Store allbul
    allbull <- bulldata$allbull
    
    # Drop-down - Preview Bullet
    selectInput(
      "prev_bul", 
      "Preview Bullet", 
      choices = unique(allbull$bullet), 
      selected = NULL, 
      multiple = FALSE
    )
  })
  
  # OUTPUT UI - Preview Bullet tab panel
  output$lpreview <- renderUI({
    req(nrow(bulldata$allbull) > 0)
    req(length(input$prev_bul) > 0)
    
    progress <- shiny::Progress$new(); on.exit(progress$close())
    
    # Refresh on tab change
    temp_refresh <- input$prevreport
    
    # Get selected bullet
    bull <- fiter_preview_bullet(
      allbull = bulldata$allbull,
      preview_bull_name = input$prev_bul
    )
    
    # Render selected bullet
    progress$set(message = "Rendering Previews", value = .75)
    for(idx in 1:nrow(bull)) {
      local({
        cidx <- idx
        # OUTPUT RGL - Bullet
        output[[paste0("x3prglprev",idx)]] <- renderRglwidget({
          render_land(
            x3p = bull$x3p[[cidx]], 
            ccut = NULL,
            sample_m = sample_m,
            rotate = TRUE,
            img_size = 500,
            img_zoom = 0.4
          )
          rglwidget()
        })
      })
    }
    
    # Display selected bullet
    layout_column_wrap(
      width = 1/6, 
      !!!lapply(1:nrow(bull), FUN = function(x) parse_rglui(x, name = "x3prglprev", land_name = bull$land_names[x]))
    )
  })
  
  
  # SECTION: CROSSCUT INTERACTIVITY-----------------------------------
  
  # OUTPUT UI - Crosscut Select Bullet Drop-down
  output$CCBull1 <- renderUI({
    req(is_crosscut(bulldata$stage))
    req(bulldata$preCC)
    
    # DROP-DOWN - Select Bullet
    bullets <- bulldata$preCC
    selectInput("cc_bulsel", "Select Bullet", choices = unique(bullets$bullet), selected = unique(bullets$bullet)[1], multiple = FALSE)
  })
  
  # OUTPUT UI - Crosscut Sliders, Finalize Button, and Compare Button
  output$CCBull2 <- renderUI({
    req(is_crosscut(bulldata$stage))
    req(bulldata$preCC)
    req(input$cc_bulsel)
    
    # Filter selected bullet
    bullets <- filter_selected_bullet(bullets = bulldata$preCC, selected = input$cc_bulsel)
    
    # Calculate Y coordinate ranges for each bullet land in microns
    bullet_y_ranges <- get_max_microns(bullets = bullets)
    
    # Render crosscut sliders and Finalize Crosscut and Compare Bullets buttons
    list(
      # Render crosscut sliders
      mapply(render_ccsl, id = 1:nrow(bullets), ymin = 0, ymax = bullet_y_ranges, yset = bullets$crosscut, SIMPLIFY = FALSE),
      # BUTTON - Finalize Crosscut
      fluidRow(column(12, actionButton("saveCC", label = "Finalise CrossCut"), align="center")),
      hr(),
      # BUTTON - Compare Bullets
      fluidRow(column(12, actionButton("doprocessCC", label = "Compare Bullets"), align="center"))
    )
  })
  
  # OUTPUT UI - Display Lands with Crosscuts
  output$CCBullLand <- 	renderUI({
    req(is_crosscut(bulldata$stage))
    req(bulldata$preCC)
    req(input$cc_bulsel)
    
    # Filter selected bullet
    bullets <- filter_selected_bullet(bullets = bulldata$preCC, selected = input$cc_bulsel)
    
    # Refresh tab on change
    temp_refresh <- input$prevreport
    
    # Render lands with crosscuts 
    for(idx in 1:nrow(bullets)) {
      local({
        cidx <- idx
        # OUTPUT RGL - Render lands with crosscuts
        output[[paste0("CC_Sel_",idx)]] <- renderRglwidget({
          render_land(
            x3p = bullets$x3p[[cidx]],
            ccut = input[[paste0("CCsl", cidx)]],
            rotate = TRUE,
            sample_m = sample_m,
            img_size = 500,
            img_zoom = 0.4
          )
          rglwidget()
        })
      })
    }
    
    # Display lands with crosscuts 
    layout_column_wrap(
      width = 1/6, 
      !!!lapply(1:nrow(bullets), FUN = function(x) parse_rglui(x, name = "CC_Sel_", land_name = NULL))
    )
  })
  
  # OBSERVE EVENT - Finalize Crosscut button
  # Update crosscut location in data frame with current crosscut slider values
  observeEvent(input$saveCC,{
    req(is_crosscut(bulldata$stage))
    req(bulldata$preCC)
    
    bullets <- bulldata$preCC
    
    # Update crosscut column in bullets data frame with crosscut sliders
    bullets <- update_cc_from_slider_wrapper(
      bullets = bullets, 
      selected = input$cc_bulsel,
      all_inputs = reactiveValuesToList(input)
    )
    
    # Store bullets with crosscut locations
    bulldata$preCC <- bullets
    bulldata$preCC_export <- make_export_df(df = bullets)
  })
  
  # OBSERVE EVENT - Compare Bullets Button
  # Push preCC to postCC and change stage to "grooves"
  observeEvent(input$doprocessCC,{
    req(is_crosscut(bulldata$stage))
    req(bulldata$preCC)
    
    progress <- shiny::Progress$new(); on.exit(progress$close())
    
    # Extract crosscut data
    bullets <- get_ccdata_wrapper(postCC = bulldata$preCC, progress = progress)
    
    # Find the optimal groove locations
    bullets <- get_grooves_wrapper(bullets = bullets, progress = progress)
    
    # Push preCC data frame to postCC
    bulldata$postCC <- bullets
    bulldata$postCC_export <- make_export_df(bullets)
    
    # Reset preCC to NULL
    bulldata$preCC <- NULL
    bulldata$preCC_export <- NULL
    
    bulldata$stage <- c("upload", "crosscut", "groove")
  })
  
  
  # SECTION: GROOVES INTERACTIVITY------------------------------------------
  
  # REACTIVE - Filtered profile for grooves interactivity
  profile_df <- reactive({
    req(is_groove(bulldata$stage, strict = TRUE))
    req(bulldata$postCC)
    req(input$groove_bulsel)
    req(input$groove_landsel)
    
    land <- filter_selected_bullet_land(
      bullets = bulldata$postCC, 
      sel_bullet = input$groove_bulsel,
      sel_land = input$groove_landsel
    ) %>% 
      tidyr::unnest(ccdata)
  })
  
  # OBSERVE EVENT - Save Grooves
  observeEvent(input$save_grooves_button, {
    req(is_groove(bulldata$stage, strict = TRUE))
    req(bulldata$postCC)
    req(input$groove_bulsel)
    req(input$groove_landsel)
    req(profile_df())
    
    progress <- shiny::Progress$new(); on.exit(progress$close())
    
    bullets <- bulldata$postCC
    
    idx <- which(bullets$bullet == input$groove_bulsel & bullets$land == input$groove_landsel)
    bullets$grooves[[idx]]$groove[1] <- input$grooveL
    bullets$grooves[[idx]]$groove[2] <- input$grooveR
    
    bulldata$postCC <- bullets
    bulldata$postCC_export <- make_export_df(bulldata$postCC)
    progress$set(message = "Grooves saved", value = 0)
    
  })
  
  # OUTPUT UI - Groove Select Bullet Drop-down
  output$grooveBullSelUI <- renderUI({
    req(is_groove(bulldata$stage, strict = TRUE))
    req(bulldata$postCC)
    
    # DROP-DOWN - Select Bullet
    bullets <- bulldata$postCC
    selectInput("groove_bulsel", "Select Bullet", choices = unique(bullets$bullet), selected = NULL, multiple = FALSE)
  })
  
  # OUTPUT UI - Land Select Bullet Drop-down
  output$grooveLandSelUI <- renderUI({
    req(is_groove(bulldata$stage, strict = TRUE))
    req(bulldata$postCC)
    
    # DROP-DOWN - Select Land
    bullets <- bulldata$postCC
    selectInput("groove_landsel", "Select Land", choices = unique(bullets$land), selected = NULL, multiple = FALSE)
  })
  
  # OUTPUT UI - Groove Sliders
  output$grooveSlidersUI <- renderUI({
    req(is_groove(bulldata$stage, strict = TRUE))
    req(bulldata$postCC)
    req(input$groove_bulsel)
    req(input$groove_landsel)
    
    # Profile data frame of selected bullet and land
    df <- profile_df()
    
    # Get default groove locations
    grooveL_default <- df$grooves[[1]]$groove[1]
    grooveR_default <- df$grooves[[1]]$groove[2]
    
    # Render crosscut sliders and Finalize Crosscut and Compare Bullets buttons
    list(
      sliderInput(
        inputId = "grooveL",
        label = "Left Groove",
        min = 0,
        max = floor(max(df$x) / 2),
        value = grooveL_default,
        round = TRUE
      ),
      sliderInput(
        inputId = "grooveR",
        label = "Right Groove",
        min = floor(max(df$x) / 2),
        max = floor(max(df$x)),
        value = grooveR_default,
        round = TRUE
      )
    )
  })
  
  # OUTPUT UI - Save Grooves and Next Step Buttons
  output$groovesButtonsUI <- renderUI({
    req(is_groove(bulldata$stage, strict = TRUE))
    req(bulldata$postCC)
    req(input$groove_bulsel)
    req(input$groove_landsel)
    
    list(
      fluidRow(
        column(12, actionButton("save_grooves_button", label = "Save Grooves"), align="center")
      ),
      hr(),
      fluidRow(
        column(12, actionButton("grooves_next_button", label = "Next Step"), align="center")
      )
    )
    
  })
  
  # PLOT OUTPUT - Render profiles with grooves
  output$profile_plot <- renderPlot({
    req(is_groove(bulldata$stage, strict = TRUE))
    req(bulldata$postCC)
    req(input$groove_bulsel)
    req(input$groove_landsel)
    
    df <- profile_df()
    
    df %>%
      ggplot(aes(x = x, y = value)) + 
      geom_line() +
      geom_vline(xintercept = input$grooveL, color = "red") +
      geom_vline(xintercept = input$grooveR, color = "red") +
      facet_grid(bullet~land, labeller="label_both") +
      theme_bw()
  })
  
  # OUTPUT UI - Display Crosscut (Profiles) with Grooves
  output$groovePlotsUI <- 	renderUI({
    req(is_groove(bulldata$stage, strict = TRUE))
    req(bulldata$postCC)
    req(input$groove_bulsel)
    
    plotOutput(session$ns("profile_plot"))
  })
  
  # OBSERVE EVENT - Next Step
  # Get signal, features, and random forest score, and bullet scores
  observeEvent(input$grooves_next_button, {
    req(is_groove(bulldata$stage, strict = TRUE))
    req(bulldata$postCC)
    req(bulldata$postCC$grooves)
    
    bulldata$stage = c("upload", "crosscut", "groove", "report")
    updateTabsetPanel(session, "prevreport", selected = "Comparison Report")
    
    progress <- shiny::Progress$new(); on.exit(progress$close())
    
    # Extract the signals
    bullets <- get_signals_wrapper(bullets = bulldata$postCC, progress = progress)
    
    # Align the signals
    signals_results <- get_aligned_signals_wrapper(bullets = bullets, progress = progress)
    bullets <- signals_results$bullets
    comparisons <- signals_results$comparisons
    
    # Get Resolution
    resolution <- x3p_get_scale(bullets$x3p[[1]])
    
    # Get Features
    features_results <- get_features_wrapper(comparisons = comparisons, resolution = resolution, progress = progress)
    comparisons <- features_results$comparisons
    features <- features_results$features
    
    # Predict random forest scores
    progress$set(message = "Predicting RandomForest Scores", value = .45)
    features$rfscore <- predict(rtrees, newdata = features, type = "prob")[,2]
    
    # Calculate bullet scores
    bullet_scores <- get_bullet_scores_wrapper(features = features, progress = progress)
    
    # Denote same source
    # just get the 'best phase' not just ones that are 'matches'
    bullet_scores$data <- lapply(
      bullet_scores$data,
      function(d) cbind(d, samesource = bullet_to_land_predict(land1 = d$landA, land2 = d$landB, d$rfscore, alpha = .9, difference = 0.01))
    )
    
    # Render lands with crosscuts snapshot
    bullets$x3pimg <- NA
    
    # Store comparison report data
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
  
  
  # SECTION: GENERATE REPORT------------------------------------------
  
  # OUTPUT UI - Report Comparison sidebar
  output$reportSelUI <- renderUI({
    req(is_report(bulldata$stage))
    req(is.null(bulldata$preCC))
    req(bulldata$comparison)
    
    all_bullets <- unique(bulldata$comparison$bullet_scores$bulletA)
    list(
      # DROP-DOWN - Compare Bullet
      selectInput("comp_bul1", "Compare Bullet", choices = all_bullets, selected = all_bullets[1]),
      # DROP-DOWN - With Bullet
      selectInput("comp_bul2", "With Bullet", choices = all_bullets, selected = all_bullets[2]),
      hr()
    )
  })
  
  # MODULE - reportServer
  reportServer(
    "report1", 
    bullet_data = bulldata, 
    comp_bul1 = reactive(input$comp_bul1), 
    comp_bul2 = reactive(input$comp_bul2),
    phase_test_results = phase$test_results
  )
  
  # SECTION: PHASE TEST-----------------------------------------------
  
  observe({
    req(is_report(bulldata$stage))
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
