#' BulletAnalyzr Application
#'
#' Lauch a 'shiny' application for forensic bullet comparisons.
#'
#' @name bulletAnalyzrApp
#' @rdname bulletAnalyzrApp
#' @keywords Shiny
#' 
#' @param run_interactive TRUE allows the user to adjust the crosscut and groove
#'   locations. FALSE uses the crosscut and grooves automatically chosen by the
#'   app.
#' @param ... Other arguments passed on to 'onStart', 'options', 'uiPattern', or
#'   'enableBookmarking' of 'shiny::shinyApp'
#' 
#' @return No return value, called to launch 'shiny' app
#' 
#' @export
#' @importFrom stats predict
#' 
#' @examples
#' \dontrun{
#'   bulletAnalyzrApp()
#' }
#'
#' @return A Shiny app
bulletAnalyzrApp <- function(run_interactive = TRUE, ...){
  
  ## Config
  version <- "0.5.0-beta.1"
  options(rgl.useNULL = TRUE)
  options(shiny.maxRequestSize = 150*1024^2)
  shiny::addResourcePath(
    prefix = "images", 
    directoryPath = system.file(file.path("extdata", "images"), package = "bulletAnalyzr")
  )
  ggplot2::theme_set(ggplot2::theme_bw())
  ggplot2::theme_update(
    text = ggplot2::element_text(size = 22), 
    plot.title = ggplot2::element_text(size = 22, face = "bold")
  )
  sample_m = 10
  
  ui <- shiny::shinyUI({
    shiny::fluidPage(title = "BulletAnalyzr",
                     shinyjs::useShinyjs(),
                     shiny::tags$head(
                       shiny::tags$link(
                         href = "https://fonts.googleapis.com/css?family=Montserrat:400,500,700,900|Ubuntu:400,500,700",
                         rel = "stylesheet",
                         type = "text/css"
                       ),
                       shiny::tags$link(rel = "shortcut icon", href = "favicon.png", type = "image/png"),
                       shiny::tags$link(rel = "icon", href = "favicon.png", type = "image/png")
                     ),
                     shiny::includeCSS(system.file("extdata", "styles.css", package = "bulletAnalyzr")),
                     shiny::tags$div(id="app-container",
                                     shiny::fluidRow(
                                       shiny::column(width = 4, shiny::tags$a(target = "_blank", href="https://forensicstats.org", shiny::tags$img(src = "images/BulletAnalzr-Mark-2.png", width="500px"))),
                                       shiny::column(width = 4, shiny::br()),
                                       shiny::column(width = 4, shiny::tags$a(target = "_blank", href="https://forensicstats.org", shiny::tags$img(src = "images/BulletAnalyzr-Design-2.png", width="500px")), align="right"),
                                     ),
                                     shiny::tags$div(id="main-content",
                                                     # navbarPage(title = div(div(id = "img-id",img(src = "csafe_tools_blue_h.png", alt="Logo", height = "40px"))),
                                                     shiny::navbarPage(NULL,
                                                                       shiny::tabPanel("Home",
                                                                                       shiny::sidebarLayout(shiny::tags$div(id="my-sidebar",
                                                                                                                            shiny::sidebarPanel(width=3,
                                                                                                                                                shiny::fluidPage(
                                                                                                                                                  
                                                                                                                                                  ## Welcome Page
                                                                                                                                                  shiny::conditionalPanel(condition="input.prevreport == 'Welcome'",
                                                                                                                                                                          shiny::div(id = "autonomous",
                                                                                                                                                                                     shiny::tags$h1(class = "responsive-text","GET STARTED"),
                                                                                                                                                                                     shiny::br(),
                                                                                                                                                                                     shiny::helpText("Press the following button to start using the app by uploading the bullet data."),
                                                                                                                                                                                     shiny::br(),
                                                                                                                                                                                     shiny::actionButton("begin_button", "Begin")#, icon = icon("check"))
                                                                                                                                                                          ),
                                                                                                                                                  ),
                                                                                                                                                  
                                                                                                                                                  ## Bullet Select and manipulate Input 
                                                                                                                                                  shiny::conditionalPanel(condition="input.prevreport == 'Upload Bullet'",
                                                                                                                                                                          shiny::fluidRow(shiny::column(12, shiny::uiOutput("bul_x3pui"))),
                                                                                                                                                                          shiny::hr()
                                                                                                                                                  ),
                                                                                                                                                  shiny::conditionalPanel(condition="input.prevreport == 'Preview Bullet'",
                                                                                                                                                                          shiny::uiOutput("prevSelUI"),
                                                                                                                                                  ),
                                                                                                                                                  shiny::conditionalPanel(condition="input.prevreport == 'Comparison Report'",
                                                                                                                                                                          # Crosscuts
                                                                                                                                                                          shiny::uiOutput("CCBull1"),
                                                                                                                                                                          shiny::uiOutput("CCBull2"),
                                                                                                                                                                          
                                                                                                                                                                          # Grooves
                                                                                                                                                                          shiny::uiOutput("grooveBullSelUI"),
                                                                                                                                                                          shiny::uiOutput("grooveLandSelUI"),
                                                                                                                                                                          shiny::uiOutput("grooveSlidersUI"),
                                                                                                                                                                          shiny::uiOutput("groovesButtonsUI"),
                                                                                                                                                                          
                                                                                                                                                                          # Report
                                                                                                                                                                          shiny::uiOutput("reportSelUI"),
                                                                                                                                                  ),
                                                                                                                                                  
                                                                                                                                                  ## Bullet Add to Comparison UI
                                                                                                                                                  shiny::conditionalPanel(condition="input.prevreport == 'Upload Bullet'",
                                                                                                                                                                          shiny::fluidRow(
                                                                                                                                                                            shiny::column(12, shiny::textInput("bul_x3p_name", label="Bullet Name",value="",placeholder="Bullet Name Here ...")),
                                                                                                                                                                            shiny::column(12, shiny::actionButton("add_to_list_button", label = "Add Bullet to Comparison List"),align="center")
                                                                                                                                                                          ),
                                                                                                                                                                          shiny::hr(),
                                                                                                                                                  ),
                                                                                                                                                  
                                                                                                                                                  ## Bullet Comparison UI
                                                                                                                                                  shiny::conditionalPanel(condition="input.prevreport == 'Upload Bullet'",
                                                                                                                                                                          shiny::fluidRow(
                                                                                                                                                                            shiny::column(12,shiny::uiOutput("bullSelCheckboxUI")),
                                                                                                                                                                            shiny::column(12,shiny::actionButton("doprocess", label = "Compare Bullets"),align="center"),
                                                                                                                                                                          ),
                                                                                                                                                                          shiny::div(id = "orientation-note",
                                                                                                                                                                                     shiny::br(),
                                                                                                                                                                                     shiny::helpText("Note that for valid comparisons results, scans have to be oriented with the tip of the bullet pointing left."),
                                                                                                                                                                                     shiny::br()
                                                                                                                                                                          )
                                                                                                                                                  ),
                                                                                                                                                  
                                                                                                                                                  ## Download Report Button
                                                                                                                                                  shiny::conditionalPanel(condition="input.prevreport == 'Comparison Report'",
                                                                                                                                                                          reportSidebarUI("report1"),
                                                                                                                                                  ),
                                                                                                                                                ))),
                                                                                                            shiny::mainPanel(
                                                                                                              shiny::tabsetPanel(id="prevreport",
                                                                                                                          
                                                                                                                          ## Welcome
                                                                                                                          shiny::tabPanel("Welcome",
                                                                                                                                          shiny::h3("WELCOME TO BULLETANALYZR!"),
                                                                                                                                          shiny::p("Our innovation combines 3D imagery and sophisticated algorithms to revolutionize bullet analysis. This prototype demonstrates how our methods can calculate the likelihood of the observed similarity if two bullets originated from the same firearm versus different firearms. It's a work in progress, evolving through feedback from diverse communities."),
                                                                                                                          ),
                                                                                                                          
                                                                                                                          ## Upload Bullet RGL Windows
                                                                                                                          shiny::tabPanel("Upload Bullet", shiny::uiOutput("lpupload")),
                                                                                                                          
                                                                                                                          ## Upload Bullet RGL Windows
                                                                                                                          shiny::tabPanel("Preview Bullet",shiny::uiOutput("lpreview")),
                                                                                                                          
                                                                                                                          ## Comparison Report
                                                                                                                          shiny::tabPanel("Comparison Report", 
                                                                                                                                          shinycssloaders::withSpinner(shiny::uiOutput("CCBullLand")),
                                                                                                                                          shinycssloaders::withSpinner(shiny::uiOutput("groovePlotsUI")),
                                                                                                                                          shinycssloaders::withSpinner(reportMainUI("report1"))
                                                                                                                          )  
                                                                                                              )
                                                                                                            )
                                                                                       )
                                                                       ),
                                                                       shiny::tabPanel( 
                                                                         "About",
                                                                         shiny::h3("BULLETANALYZR"),
                                                                         shiny::h4(paste0("v", version)),
                                                                         shiny::includeHTML(system.file("extdata", "HTML", "welcome.html", package = "bulletAnalyzr")),
                                                                         shiny::verbatimTextOutput(outputId="sessionInfo"),
                                                                         shiny::hr()
                                                                       ),
                                                                       shiny::tabPanel("Instructions",),
                                                                       shiny::tabPanel("Contact",),
                                                     ))),
                     
                     # Footer
                     shiny::tags$div(id="global-footer",
                                     shiny::fluidRow(
                                       shiny::column(width = 4, shiny::tags$img(src="images/csafe_tools_blue_h.png", alt="Logo", height = "40px")),
                                       shiny::column(width = 4, shiny::tags$p("195 Durham Center, 613 Morrill Road, Ames, Iowa, 50011")),
                                       shiny::column(width = 4, shiny::tags$p("(C) 2023-2025 | All Rights Reserved", class="right-float"))
                                     )
                     ))  
  })
  
  
  # Server--------------------------------------------------------------
  server <- function(input, output, session) {
    
    # OUTPUT - Session Info - Report versions of packages used
    output$sessionInfo <- render_session_info(session)
    
    # REACTIVE VALUES - Switch alerts on/off
    values <- shiny::reactiveValues(show_alert = TRUE)
    
    # REACTIVE VALUES - Bullet and comparison data
    bulldata <- shiny::reactiveValues(
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
    
    # REACTIVE VALUES - Store unique bullet and land names for drop-down menus
    grooves <- shiny::reactiveValues(
      bullets = NULL,
      lands = NULL
    )
    
    # REACTIVE VALUES - Phase test results
    phase <- shiny::reactiveValues(
      test_results = NULL
    )
    
    # EXPORT VALUES - For shinytest2 tests
    shiny::exportTestValues(
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
    shiny::observeEvent(input$begin_button, {
      bulldata$stage <- c("upload")
      shiny::updateTabsetPanel(session, "prevreport", selected = "Upload Bullet")
    })
    
    
    # SECTION: UPLOAD BULLET TAB - CBULL --------------------------------------
    
    # OUTPUT UI - Upload Land x3p Files Button
    output$bul_x3pui <- shiny::renderUI({
      shiny::req(is_upload(bulldata$stage))
      
      # Button - Bullet Land x3p Files
      shiny::fileInput("upload_button", "Select Bullet Land x3p files", accept = ".x3p", multiple = TRUE)
    })
    
    # OBSERVE EVENT - Bullet Land x3p Files Button
    # Preprocess uploaded x3p files and push to cbull
    shiny::observeEvent(input$upload_button, {
      shiny::req(is_upload(bulldata$stage))
      
      shinyjs::disable("add_to_list_button")
      
      progress <- shiny::Progress$new(); on.exit(progress$close())
      
      # Get default bullet name
      bullet_name <- identify_bullet(input$upload_button$name)
      shiny::updateTextInput(session, "bul_x3p_name", value = bullet_name)
      
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
      cbull <- bulletxtrctr::read_bullet(temp_dir)
      
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
    output$lpupload <- shiny::renderUI({
      shiny::req(is_upload(bulldata$stage))
      shiny::req(shiny::isTruthy(bulldata$cbull) || shiny::isTruthy(bulldata$cbull_name))
      
      progress <- shiny::Progress$new(); on.exit(progress$close())
      
      if (!is.null(bulldata$cbull)) {
        cbull <- bulldata$cbull
      } else if (!is.null(bulldata$cbull_name) & (nrow(bulldata$allbull) > 0)) {
        cbull <- filter_bullet_col(
          df = bulldata$allbull,
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
          output[[paste0("x3prgl",idx)]] <- rgl::renderRglwidget({
            render_land(
              x3p = cbull$x3p[[cidx]], 
              ccut = NULL,
              sample_m = sample_m,
              rotate = TRUE,
              img_size = 500,
              img_zoom = 0.4
            )
            rgl::rglwidget()
          })
        })
      }
      
      # Enable upload button
      shinyjs::enable("add_to_list_button")
      
      # Display bullet
      bslib::layout_column_wrap(
        width = 1/6,
        !!!lapply(1:nrow(cbull), FUN = function(x) parse_rglui(x, name = "x3prgl", land_name = cbull$land_names[x]))
      )
    })
    
    
    # SECTION: UPLOAD BULLET TAB - ALLBULL ------------------------------------
    
    # OBSERVE EVENT - Add Bullet to Comparison List button
    # Push current bullet data to all bullet data object
    shiny::observeEvent(input$add_to_list_button, {
      shiny::req(is_upload(bulldata$stage))
      shiny::req(bulldata$cbull)
      
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
      shinyjs::disable("add_to_list_button")
    })
    
    # OUTPUT UI - Select Bullets to Compare Checkbox
    output$bullSelCheckboxUI <- shiny::renderUI({
      shiny::req(is_upload(bulldata$stage))
      shiny::req(nrow(bulldata$allbull) > 0)
      
      # Store allbull
      allbull <- bulldata$allbull
      
      # CHECK BOX - Select Bullets to Compare
      shiny::checkboxGroupInput(
        "bull_sel_checkbox",
        label = "Select Bullets to Compare", 
        choices = unique(bulldata$allbull$bullet),
        selected = unique(bulldata$allbull$bullet)
      )
    })
    
    # OBSERVE EVENT - Compare Bullets button (Upload Bullet Tab) - Get default
    # crosscuts before starting interactivity
    shiny::observeEvent(input$doprocess, {
      shiny::req(is_upload(bulldata$stage))
      shiny::req(length(input$bull_sel_checkbox) > 0)
      
      values$show_alert <- FALSE
      progress <- shiny::Progress$new(); on.exit(progress$close())
      
      bullets <- bulldata$allbull
      
      # Find optimal crosscuts
      progress$set(message = "Get suitable Cross Sections", value = 0)
      bullets <- get_default_cc_wrapper(
        bullets = bullets,
        ylimits = c(150, NA)
      )
      
      if (run_interactive) {
        # Interactive mode: Store in preCC and switch to Comparison Report tab
        # where user can adjust crosscuts and grooves
        bulldata$preCC <- bullets
        bulldata$preCC_export <- make_export_df(df = bulldata$preCC)
        bulldata$stage <- c("upload", "crosscut")
        shiny::updateTabsetPanel(session, "prevreport", selected = "Comparison Report")
      } else {
        # Non-interactive mode: Run all processing automatically
        
        # Extract crosscut data
        bullets <- get_ccdata_wrapper(postCC = bullets, progress = progress)
        
        # Find the optimal groove locations
        bullets <- get_grooves_wrapper(bullets = bullets, progress = progress)
        
        # Update postCC with groove data
        bulldata$postCC <- bullets
        bulldata$postCC_export <- make_export_df(bullets)
        
        # Extract the signals
        bullets <- get_signals_wrapper(bullets = bulldata$postCC, progress = progress)
        
        # Align the signals
        signals_results <- get_aligned_signals_wrapper(bullets = bullets, progress = progress)
        bullets <- signals_results$bullets
        comparisons <- signals_results$comparisons
        
        # Get Resolution
        resolution <- x3ptools::x3p_get_scale(bullets$x3p[[1]])
        
        # Get Features
        features_results <- get_features_wrapper(comparisons = comparisons, resolution = resolution, progress = progress)
        comparisons <- features_results$comparisons
        features <- features_results$features
        
        # Predict random forest scores
        progress$set(message = "Predicting RandomForest Scores", value = .45)
        features$rfscore <- predict(bulletxtrctr::rtrees, newdata = features, type = "prob")[,2]
        
        # Calculate bullet scores
        bullet_scores <- get_bullet_scores_wrapper(features = features, progress = progress)
        
        # Denote same source
        bullet_scores$data <- lapply(
          bullet_scores$data,
          function(d) cbind(d, samesource = bulletxtrctr::bullet_to_land_predict(land1 = d$landA, land2 = d$landB, d$rfscore, alpha = .9, difference = 0.01))
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
        
        # Set stage to report
        bulldata$stage <- c("upload", "crosscut", "groove", "report")
        
        # Switch to Comparison Report tab panel
        shiny::updateTabsetPanel(session, "prevreport", selected = "Comparison Report")
      }
    })
    
  
    # SECTION: PREVIEW BULLET TAB----------------------------------------
    
    # OUTPUT UI - Preview Bullet sidebar
    output$prevSelUI <- shiny::renderUI({
      shiny::req(nrow(bulldata$allbull) > 0)
      
      # Store allbul
      allbull <- bulldata$allbull
      
      # Drop-down - Preview Bullet
      shiny::selectInput(
        "prev_bul", 
        "Preview Bullet", 
        choices = unique(allbull$bullet), 
        selected = NULL, 
        multiple = FALSE
      )
    })
    
    # OUTPUT UI - Preview Bullet tab panel
    output$lpreview <- shiny::renderUI({
      shiny::req(nrow(bulldata$allbull) > 0)
      shiny::req(length(input$prev_bul) > 0)
      
      progress <- shiny::Progress$new(); on.exit(progress$close())
      
      # Refresh on tab change
      temp_refresh <- input$prevreport
      
      # Get selected bullet
      bull <- filter_bullet_col(
        df = bulldata$allbull,
        selected = input$prev_bul,
        unnest_data = FALSE
      )
      
      # Render selected bullet
      progress$set(message = "Rendering Previews", value = .75)
      for(idx in 1:nrow(bull)) {
        local({
          cidx <- idx
          # OUTPUT RGL - Bullet
          output[[paste0("x3prglprev",idx)]] <- rgl::renderRglwidget({
            render_land(
              x3p = bull$x3p[[cidx]], 
              ccut = NULL,
              sample_m = sample_m,
              rotate = TRUE,
              img_size = 500,
              img_zoom = 0.4
            )
            rgl::rglwidget()
          })
        })
      }
      
      # Display selected bullet
      bslib::layout_column_wrap(
        width = 1/6, 
        !!!lapply(1:nrow(bull), FUN = function(x) parse_rglui(x, name = "x3prglprev", land_name = bull$land_names[x]))
      )
    })
    
    
    # SECTION: CROSSCUT INTERACTIVITY-----------------------------------
    
    # OUTPUT UI - Crosscut Select Bullet Drop-down
    output$CCBull1 <- shiny::renderUI({
      shiny::req(is_crosscut(bulldata$stage))
      shiny::req(bulldata$preCC)
      
      # DROP-DOWN - Select Bullet
      bullets <- bulldata$preCC
      shiny::selectInput("cc_bulsel", "Select Bullet", choices = unique(bullets$bullet), selected = unique(bullets$bullet)[1], multiple = FALSE)
    })
    
    # OUTPUT UI - Crosscut Sliders, Finalize Button, and Compare Button
    output$CCBull2 <- shiny::renderUI({
      shiny::req(is_crosscut(bulldata$stage))
      shiny::req(bulldata$preCC)
      shiny::req(input$cc_bulsel)
      
      # Filter selected bullet
      bullets <- filter_bullet_col(df = bulldata$preCC, selected = input$cc_bulsel)
      
      # Calculate Y coordinate ranges for each bullet land in microns
      bullet_y_ranges <- get_max_microns(bullets = bullets)
      
      # Render crosscut sliders and Finalize Crosscut and Compare Bullets buttons
      list(
        # Render crosscut sliders
        mapply(render_ccsl, id = 1:nrow(bullets), ymin = 0, ymax = bullet_y_ranges, yset = bullets$crosscut, SIMPLIFY = FALSE),
        # BUTTON - Finalize Crosscut
        shiny::fluidRow(shiny::column(12, shiny::actionButton("saveCC", label = "Finalise CrossCut"), align="center")),
        shiny::hr(),
        # BUTTON - Compare Bullets
        shiny::fluidRow(shiny::column(12, shiny::actionButton("doprocessCC", label = "Compare Bullets"), align="center"))
      )
    })
    
    # OUTPUT UI - Display Lands with Crosscuts
    output$CCBullLand <- 	shiny::renderUI({
      shiny::req(is_crosscut(bulldata$stage))
      shiny::req(bulldata$preCC)
      shiny::req(input$cc_bulsel)
      
      # Filter selected bullet
      bullets <- filter_bullet_col(df = bulldata$preCC, selected = input$cc_bulsel)
      
      # Refresh tab on change
      temp_refresh <- input$prevreport
      
      # Render lands with crosscuts 
      for(idx in 1:nrow(bullets)) {
        local({
          cidx <- idx
          # OUTPUT RGL - Render lands with crosscuts
          output[[paste0("CC_Sel_",idx)]] <- rgl::renderRglwidget({
            render_land(
              x3p = bullets$x3p[[cidx]],
              ccut = input[[paste0("CCsl", cidx)]],
              rotate = TRUE,
              sample_m = sample_m,
              img_size = 500,
              img_zoom = 0.4
            )
            rgl::rglwidget()
          })
        })
      }
      
      # Display lands with crosscuts 
      bslib::layout_column_wrap(
        width = 1/6, 
        !!!lapply(1:nrow(bullets), FUN = function(x) parse_rglui(x, name = "CC_Sel_", land_name = NULL))
      )
    })
    
    # OBSERVE EVENT - Finalize Crosscut button
    # Update crosscut location in data frame with current crosscut slider values
    shiny::observeEvent(input$saveCC,{
      shiny::req(is_crosscut(bulldata$stage))
      shiny::req(bulldata$preCC)
      
      bullets <- bulldata$preCC
      
      # Update crosscut column in bullets data frame with crosscut sliders
      bullets <- update_cc_from_slider_wrapper(
        bullets = bullets, 
        selected = input$cc_bulsel,
        all_inputs = shiny::reactiveValuesToList(input)
      )
      
      # Store bullets with crosscut locations
      bulldata$preCC <- bullets
      bulldata$preCC_export <- make_export_df(df = bullets)
    })
    
    # OBSERVE EVENT - Compare Bullets Button
    # Push preCC to postCC and change stage to "grooves"
    shiny::observeEvent(input$doprocessCC,{
      shiny::req(is_crosscut(bulldata$stage))
      shiny::req(bulldata$preCC)
      
      progress <- shiny::Progress$new(); on.exit(progress$close())
      
      # Extract crosscut data
      bullets <- get_ccdata_wrapper(postCC = bulldata$preCC, progress = progress)
      
      # Find the optimal groove locations
      bullets <- get_grooves_wrapper(bullets = bullets, progress = progress)
      
      # Push preCC data frame to postCC
      bulldata$postCC <- bullets
      bulldata$postCC_export <- make_export_df(bullets)
      
      # Push unique bullet and land names to reactive values for grooves drop-down
      # menus
      grooves$bullets <- unique(bulldata$postCC$bullet)
      grooves$lands <- unique(bulldata$postCC$land)
      
      # Reset preCC to NULL
      bulldata$preCC <- NULL
      bulldata$preCC_export <- NULL
      
      bulldata$stage <- c("upload", "crosscut", "groove")
    })
    
    
    # SECTION: GROOVES INTERACTIVITY------------------------------------------
    
    # REACTIVE - Filtered profile for grooves interactivity
    profile_df <- shiny::reactive({
      shiny::req(is_groove(bulldata$stage, strict = TRUE))
      shiny::req(bulldata$postCC)
      shiny::req(input$groove_bulsel)
      shiny::req(input$groove_landsel)
      
      land <- filter_bullet_land_cols(
        df = bulldata$postCC, 
        sel_bullet = input$groove_bulsel,
        sel_land = input$groove_landsel,
        unnest_data = "ccdata"
      ) 
    })
    
    # OBSERVE EVENT - Save Grooves
    shiny::observeEvent(input$save_grooves_button, {
      shiny::req(is_groove(bulldata$stage, strict = TRUE))
      shiny::req(bulldata$postCC)
      shiny::req(input$groove_bulsel)
      shiny::req(input$groove_landsel)
      shiny::req(profile_df())
      
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
    output$grooveBullSelUI <- shiny::renderUI({
      shiny::req(is_groove(bulldata$stage, strict = TRUE))
      shiny::req(grooves$bullets)
      
      # DROP-DOWN - Select Bullet
      shiny::selectInput(
        "groove_bulsel", 
        "Select Bullet", 
        choices = grooves$bullets, 
        selected = NULL, 
        multiple = FALSE
      )
    })
    
    # OUTPUT UI - Land Select Bullet Drop-down
    output$grooveLandSelUI <- shiny::renderUI({
      shiny::req(is_groove(bulldata$stage, strict = TRUE))
      shiny::req(grooves$bullets)
      
      # DROP-DOWN - Select Land
      shiny::selectInput(
        "groove_landsel", 
        "Select Land", 
        choices = grooves$lands, 
        selected = NULL, 
        multiple = FALSE
      )
    })
    
    # OUTPUT UI - Groove Sliders
    output$grooveSlidersUI <- shiny::renderUI({
      shiny::req(is_groove(bulldata$stage, strict = TRUE))
      shiny::req(bulldata$postCC)
      shiny::req(input$groove_bulsel)
      shiny::req(input$groove_landsel)
      
      # Profile data frame of selected bullet and land
      df <- profile_df()
      
      # Get default groove locations
      grooveL_default <- df$grooves[[1]]$groove[1]
      grooveR_default <- df$grooves[[1]]$groove[2]
      
      # Render crosscut sliders and Finalize Crosscut and Compare Bullets buttons
      list(
        shiny::sliderInput(
          inputId = "grooveL",
          label = "Left Groove",
          min = 0,
          max = floor(max(df$x) / 2),
          value = grooveL_default,
          round = TRUE
        ),
        shiny::sliderInput(
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
    output$groovesButtonsUI <- shiny::renderUI({
      shiny::req(is_groove(bulldata$stage, strict = TRUE))
      shiny::req(bulldata$postCC)
      shiny::req(input$groove_bulsel)
      shiny::req(input$groove_landsel)
      
      list(
        shiny::fluidRow(
          shiny::column(12, shiny::actionButton("save_grooves_button", label = "Save Grooves"), align="center")
        ),
        shiny::hr(),
        shiny::fluidRow(
          shiny::column(12, shiny::actionButton("grooves_next_button", label = "Next Step"), align="center")
        )
      )
      
    })
    
    # PLOT OUTPUT - Render profiles with grooves
    output$profile_plot <- shiny::renderPlot({
      shiny::req(is_groove(bulldata$stage, strict = TRUE))
      shiny::req(bulldata$postCC)
      shiny::req(input$groove_bulsel)
      shiny::req(input$groove_landsel)
      
      plot_profile(
        df = profile_df(),
        left_groove = input$grooveL,
        right_groove = input$grooveR
      )

    })
    
    # OUTPUT UI - Display Crosscut (Profiles) with Grooves
    output$groovePlotsUI <- 	shiny::renderUI({
      shiny::req(is_groove(bulldata$stage, strict = TRUE))
      shiny::req(bulldata$postCC)
      shiny::req(input$groove_bulsel)
      
      shiny::plotOutput(session$ns("profile_plot"))
    })
    
    # OBSERVE EVENT - Next Step
    # Get signal, features, and random forest score, and bullet scores
    shiny::observeEvent(input$grooves_next_button, {
      shiny::req(is_groove(bulldata$stage, strict = TRUE))
      shiny::req(bulldata$postCC)
      shiny::req(bulldata$postCC$grooves)
      
      bulldata$stage = c("upload", "crosscut", "groove", "report")
      shiny::updateTabsetPanel(session, "prevreport", selected = "Comparison Report")
      
      progress <- shiny::Progress$new(); on.exit(progress$close())
      
      # Extract the signals
      bullets <- get_signals_wrapper(bullets = bulldata$postCC, progress = progress)
      
      # Align the signals
      signals_results <- get_aligned_signals_wrapper(bullets = bullets, progress = progress)
      bullets <- signals_results$bullets
      comparisons <- signals_results$comparisons
      
      # Get Resolution
      resolution <- x3ptools::x3p_get_scale(bullets$x3p[[1]])
      
      # Get Features
      features_results <- get_features_wrapper(comparisons = comparisons, resolution = resolution, progress = progress)
      comparisons <- features_results$comparisons
      features <- features_results$features
      
      # Predict random forest scores
      progress$set(message = "Predicting RandomForest Scores", value = .45)
      features$rfscore <- predict(bulletxtrctr::rtrees, newdata = features, type = "prob")[,2]
      
      # Calculate bullet scores
      bullet_scores <- get_bullet_scores_wrapper(features = features, progress = progress)
      
      # Denote same source
      # just get the 'best phase' not just ones that are 'matches'
      bullet_scores$data <- lapply(
        bullet_scores$data,
        function(d) cbind(d, samesource = bulletxtrctr::bullet_to_land_predict(land1 = d$landA, land2 = d$landB, d$rfscore, alpha = .9, difference = 0.01))
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
    
    # SECTION: PHASE TEST-----------------------------------------------
    
    shiny::observe({
      shiny::req(is_report(bulldata$stage))
      shiny::req(bulldata$comparison)
      shiny::req(bulldata$comparison$bullet_scores)
      shiny::req(input$comp_bul1)
      shiny::req(input$comp_bul2)
      
      d <- filter_bulletA_bulletB_cols(
        df = bulldata$comparison$bullet_scores,
        selected1 = input$comp_bul1,
        selected2 = input$comp_bul2,
        unnest_data = "data"
      )
      
      tryCatch({
        phase$test_results <- bulletxtrctr::phase_test(land1 = d$landA, land2 = d$landB, d$ccf)
      }, error = function(e) {
        return(d)
      })
    })
    
    # SECTION: GENERATE REPORT------------------------------------------
    
    # OUTPUT UI - Report Comparison sidebar
    output$reportSelUI <- shiny::renderUI({
      shiny::req(is_report(bulldata$stage))
      shiny::req(is.null(bulldata$preCC))
      shiny::req(bulldata$comparison)
      
      all_bullets <- unique(bulldata$comparison$bullet_scores$bulletA)
      list(
        # DROP-DOWN - Compare Bullet
        shiny::selectInput("comp_bul1", "Compare Bullet", choices = all_bullets, selected = all_bullets[1]),
        # DROP-DOWN - With Bullet
        shiny::selectInput("comp_bul2", "With Bullet", choices = all_bullets, selected = all_bullets[2]),
        shiny::hr()
      )
    })
    
    # MODULE - reportServer
    reportServer(
      "report1", 
      bullet_data = bulldata, 
      comp_bul1 = shiny::reactive(input$comp_bul1), 
      comp_bul2 = shiny::reactive(input$comp_bul2),
      phase_test_results = phase$test_results
    )

  }
  
  shiny::shinyApp(ui, server, ...)
  
}
