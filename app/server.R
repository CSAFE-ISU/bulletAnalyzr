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
source("R/helper.R")
source("R/bullet-lists.R")
source("R/bullet-transformations.R")


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
  observeEvent(input$confirm_autonomous, {
    updateTabsetPanel(session, "prevreport", selected = "Upload Bullet")
  })
  
  
  # SECTION: UPLOAD BULLET TAB -----------------------------------------------
  
  # OUTPUT UI - Select bullet lands sidebar ----
  output$bul_x3pui <- renderUI({
    # Button - Bullet Land x3p Files ----
    fileInput("bul_x3p", "Select Bullet Land x3p files", accept = ".x3p", multiple = TRUE)
  })
  
  # OBSERVE EVENT - Bullet Land x3p Files button ----
  observeEvent(input$bul_x3p, {
    # Get default bullet name ----
    bullet_name <- identify_bullet(input$bul_x3p$name)
    updateTextInput(session, "bul_x3p_name", value = bullet_name)
    
    # Switch alert on ----
    values$show_alert <- TRUE
  })
  
  # OBSERVE EVENT - Add Bullet to Comparison List button ----
  # Push current bullet data to all bullet data object
  observeEvent(input$up_bull, {
    req(nrow(bulldata$cbull) > 0)
    
    bulldata$allbull <- add_cbull_to_allbull(
      cbull = bulldata$cbull,
      bul_x3p_name = input$bul_x3p_name,
      allbull = bulldata$allbull
    )
    bulldata$allbull_export <- make_export_df(df = bulldata$allbull)
    
    # Switch upload button off ----
    disable("up_bull")
  })
  
  
  # SECTION: BULLET PREVIEWS ON UPLOAD TAB --------------------------------
  
  # REACTIVE - Copy lands to tempdir and read bullet ----
  uploaded_bull <- reactive({
    temp_refresh <- input$prevreport
    
    # Create Temporary Directory and save bullets in it ----
    temp_dir <- copy_to_tempdir(
      filepath = input$bul_x3p$datapath,
      filename = input$bul_x3p$name
    )
    
    return(read_bullet(temp_dir))
  })
  
  # OUTPUT UI - Upload Bullet tab panel ----
  output$lpupload <- renderUI({
    req(input$bul_x3p)
    
    # Switch upload button off ----
    disable("up_bull")
    progress <- shiny::Progress$new();on.exit(progress$close())
    
    # Read bullet ----
    progress$set(message = "Reading Bullets", value = .25)
    bull <- uploaded_bull()
    
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
    
    # Render bullet ----
    progress$set(message = "Rendering Previews", value = .75)
    for(idx in 1:nrow(bull)) {
      local({
        cidx <- idx
        # OUTPUT RGL - Bullet ----
        output[[paste0("x3prgl",idx)]] <- renderRglwidget({
          render_land(x3p = bull$x3p[[cidx]])
          rglwidget()
        })
      })
    }
    
    # Enable upload button ----
    enable("up_bull")
    
    # Display bullet ----
    layout_column_wrap(
      width = 1/6,
      !!!lapply(1:nrow(bull), FUN = function(x) parse_rglui(x, name = "x3prgl", land_name = bull$land_names[x]))
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
    allbull <- bulldata$allbull
    bull <- allbull[allbull$bullet == input$prev_bul,]
    
    # Render selected bullet ----
    progress$set(message = "Rendering Previews", value = .75)
    for(idx in 1:nrow(bull)) {
      local({
        cidx <- idx
        # OUTPUT RGL - Bullet ----
        output[[paste0("x3prglprev",idx)]] <- renderRglwidget({
          x3p_image(x3p_sample(bull$x3p[[cidx]],m = 5) %>% x3p_rotate(), size = 500, zoom = .4)
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
  output$bull_sel <- renderUI({
    req(nrow(bulldata$allbull) > 0)
    
    # Store allbull ----
    allbull <- bulldata$allbull
    
    # CHECK BOX - Select Bullets to Compare ----
    checkboxGroupInput(
      "bullcompgroup",
      label = "Select Bullets to Compare", 
      choices = unique(bulldata$allbull$bullet),
      selected = unique(bulldata$allbull$bullet)
    )
  })
  
  # OBSERVE EVENT - Compare Bullets button (Upload Bullet Tab) ----
  observeEvent(input$doprocess, {
    req(length(input$bullcompgroup) > 0)
    
    values$show_alert <- FALSE
    progress <- shiny::Progress$new(); on.exit(progress$close())
    
    bullets <- bulldata$allbull
    
    # Find optimal crosscuts ----
    progress$set(message = "Get suitable Cross Sections", value = 0)
    bullets$crosscut <- sapply(bullets$x3p, x3p_crosscut_optimize, ylimits = c(150, NA))
    
    # Store bullets as preCC or postCC ----
    if(interactive_cc) {
      bulldata$preCC <- bullets
      bulldata$preCC_export <- make_export_df(df = bullets)
    }
    if(!interactive_cc) {
      bulldata$postCC <- bullets
      bulldata$postCC_export <- make_export_df(df = bullets)
    }
    
    # Switch to Comparison Report tab panel ----
    updateTabsetPanel(session, "prevreport", selected = "Comparison Report")
  })
  
  # OBSERVE EVENT - bulldata$postCC - Get crosscut data, grooves, signal, features, and random forest score ----
  # bulldata$postCC is populated when the Compare Bullets button (doprocessCC)
  # on the Comparison Report tab panel is clicked
  observeEvent(bulldata$postCC, {
    req(bulldata$postCC)
    
    progress <- shiny::Progress$new(); on.exit(progress$close())
    
    # Extract crosscut data ----
    progress$set(message = "Finalizing Cross Sections", value = 0)
    bullets <- bulldata$postCC
    bullets$ccdata <- mapply(try_x3p_crosscut, bullets$x3p, bullets$crosscut, SIMPLIFY = FALSE)
    
    # Get Resolution ----
    resolution <- x3p_get_scale(bullets$x3p[[1]])
    
    # Find the optimal groove locations ----
    progress$set(message = "Get the Groove Locations", value = .05)
    bullets$grooves <- lapply(
      bullets$ccdata, 
      function(x) cc_locate_grooves(x, method = "middle", adjust = 30, return_plot = FALSE)
    )
    
    # Extract the signals ----
    progress$set(message = "Extracting Signal", value = .1)
    bullets$sigs <- mapply(
      function(ccdata, grooves) cc_get_signature(ccdata, grooves, span1 = 0.75, span2 = 0.03),
      bullets$ccdata,
      bullets$grooves,
      SIMPLIFY = FALSE
    )
    bullets$bulletland <- paste0(bullets$bullet, "-", bullets$land)
    lands <- unique(bullets$bulletland)
    
    # Align the signals ----
    progress$set(message = "Align Signals", value = .15)
    comparisons <- data.frame(expand.grid(land1 = lands, land2 = lands), stringsAsFactors = FALSE)
    comparisons$aligned <- mapply(
      function(x, y, bullets) sig_align(bullets$sigs[bullets$bulletland == x][[1]]$sig, bullets$sigs[bullets$bulletland == y][[1]]$sig),
      comparisons$land1,
      comparisons$land2,
      MoreArgs = list(bullets = bullets),
      SIMPLIFY = FALSE
    )
    
    # Calculate ccf0 ----
    progress$set(message = "Evaluating Features", value = .2)
    comparisons$ccf0 <- sapply(comparisons$aligned, function(x) extract_feature_ccf(x$lands))
    
    # Evaluate striation marks ----
    progress$set(message = "Evaluating Striation Marks", value = .25)
    comparisons$striae <- lapply(comparisons$aligned, sig_cms_max, span = 75)
    
    # Extract features ----
    progress$set(message = "Extracting Features", value = .35)
    comparisons$bulletA <- sapply(strsplit(as.character(comparisons$land1),"-"),"[[",1)
    comparisons$bulletB <- sapply(strsplit(as.character(comparisons$land2),"-"),"[[",1)
    comparisons$landA <- sapply(strsplit(as.character(comparisons$land1),"-"),"[[",2)
    comparisons$landB <- sapply(strsplit(as.character(comparisons$land2),"-"),"[[",2)
    comparisons$features <- mapply(
      extract_features_all,
      comparisons$aligned,
      comparisons$striae,
      MoreArgs = list(resolution = resolution),
      SIMPLIFY = FALSE
    )
    
    # Scale features ----
    progress$set(message = "Scaling Features", value = .4)
    features <- tidyr::unnest(
      comparisons[,c("land1", "land2", "ccf0", "bulletA", "bulletB", "landA", "landB", "features")], 
      cols = features
    )
    features <- features %>% 
      mutate(cms = cms_per_mm,matches = matches_per_mm, mismatches = mismatches_per_mm, non_cms = non_cms_per_mm)
    
    # Predict random forest scores ----
    progress$set(message = "Predicting RandomForest Scores", value = .45)
    features$rfscore <- predict(rtrees, newdata = features, type = "prob")[,2]
    
    # Calculate bullet scores ----
    progress$set(message = "Preparing Report Data", value = .5)
    bullet_scores <- features %>% group_by(bulletA, bulletB) %>% tidyr::nest()
    bullet_scores$bullet_score <- sapply(bullet_scores$data,function(d) max(compute_average_scores(land1 = d$landA, land2 = d$landB, d$rfscore, verbose = FALSE)))
    
    # Denote same source ----
    # just get the 'best phase' not just ones that are 'matches'
    bullet_scores$data <- lapply(
      bullet_scores$data,
      function(d) cbind(d, samesource = bullet_to_land_predict(land1 = d$landA, land2 = d$landB, d$rfscore, alpha = .9, difference = 0.01))
    )
    
    # Render lands with crosscuts snapshot ----
    bullets$x3pimg <- NA
    for(idx in 1:nrow(bullets)) {
      progress$set(message = "Rendering Report Objects", value = round(seq(from = .55, to = .85, length.out = nrow(bullets)), 2)[idx])
      bullets$x3pimg[idx] <- render_crosscut_snap(bullets$source[idx], bullets$x3p[[idx]], bullets$crosscut[idx])	
    }
    
    # Store comparison report data ----
    progress$set(message = "Preparing Report", value = .9)
    bulldata$comparison <- list(
      bullets = bullets,
      comparisons = comparisons,
      features_scaled = features,
      bullet_scores = bullet_scores
    )
    bulldata$comparison_export <- list(
      bullets = make_export_df(bullets),
      comparisons = make_export_df(comparisons),
      features_scaled = make_export_df(features),
      bullet_scores = make_export_df(bullet_scores)
    )
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
    bullets <- bulldata$preCC
    bullets <- bullets[bullets$bullet == input$cc_bulsel,]
    
    # Calculate Y coordinate ranges for each bullet land in microns
    bullet_y_ranges <- sapply(bullets$x3p, function(x3p) {
      # Get the Y coordinate range from the x3p header info
      y_max <- floor(x3p$header.info$incrementY * (x3p$header.info$sizeY - 1))
      return(y_max)
    })
    
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
    
    # Add crosscut locations to data frame ----
    bullets[bullets$bullet == input$cc_bulsel,]$crosscut <- sapply(1:sum(bullets$bullet == input$cc_bulsel), function(x) input[[paste("CCsl",x)]])
    
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
    bullets <- bulldata$preCC
    bullets <- bullets[bullets$bullet == input$cc_bulsel,]
    
    # Refresh tab on change ----
    temp_refresh <- input$prevreport
    
    # Render lands with crosscuts ---- 
    for(idx in 1:nrow(bullets)) {
      local({
        cidx <- idx
        # OUTPUT RGL - Render lands with crosscuts ----
        output[[paste0("CC_Sel_",idx)]] <- renderRglwidget({
          bullets$x3p[[cidx]] %>%
            x3p_add_hline(yintercept = input[[paste("CCsl",cidx)]], size = 20, color = "#eeeeee") %>%
            x3p_rotate %>%
            x3p_sample(m = 5) %>%
            x3p_image(size = 500, zoom=.4)
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
  
  # OUTPUT UI - Report Download sidebar ----
  output$reportDownUI <- renderUI({
    req(is.null(bulldata$preCC))
    req(bulldata$comparison)
    
    # BUTTON - Download Report ----
    fluidRow(column(12, screenshotButton(label = "Download Report", id = "reportUI", filename="Bullet Comparison Report", scale = 2), align="center"))
  })
  
  # OUTPUT UI - Report Comparison tab panel ----
  output$reportUI <- renderUI({
    req(is.null(bulldata$preCC))
    req(bulldata$comparison)
    req(input$comp_bul1)
    req(input$comp_bul2)
    
    BullComp <- list(
      # Phase test results ----
      fluidRow(
        column(12, "SUMMARY OF RESULTS", align = "left", class = "h3"), 
        column(12, "Phase Test", align = "left", class = "h3"), 
        column(12, textOutput("bull_comp_score"), align = "left", class = "h4"), 
        column(12, textOutput("bull_comp_test"), align = "left", class = "h4"),
        hr(),
      ),
      br(),
      fluidRow(
        # Bullet score matrix ----
        column(6, plotOutput("bull_comp")),
        # Land score matrix ----
        column(6, plotOutput("land_comp")),
        # Score matrices help text ----
        column(12, tags$p("Higher scores indicate more similarity. The thick frames indicate the selected bullet comparison (left) and the land comparisons of the best phase (right).", class = "body"))
      ),
      br(),
      br(),
      # Crosscut plots ----
      fluidRow(column(12, plotOutput("land_visCC"), align = "center")),
      br(),
      br(),
      # Signal plots ----
      fluidRow(column(12, plotOutput("land_visSig"), align = "center")),
      br()
    )
    
    # Collapsible reports ----
    LandComp <- list()
    
    # Filter selected bullets ----
    bullet_scores <- bulldata$comparison$bullet_scores
    bullet_scores <- bullet_scores[bullet_scores$bulletA == input$comp_bul1 & bullet_scores$bulletB == input$comp_bul2,]
    if(nrow(bullet_scores$data[[1]])>0) {
      # Collect land-wise data ----
      bsldata <- bullet_scores$data[[1]]
      # Sort in descending order ----
      # just re-order the data - that will be safer and quicker
      bsldata <- bsldata %>% 
        mutate(samesource = factor(samesource, levels = c(TRUE, FALSE))) %>%
        group_by(samesource) %>% 
        arrange(desc(rfscore), .by_group = TRUE)
      odridx <- order(bsldata$rfscore, decreasing = TRUE) # this should be in order now
      
      # getting scales and instrument info ... not correct yet, but just for the first scan
      scale <- bulldata$cbull$x3p[[1]] %>% x3p_get_scale()
      instrument <- bulldata$cbull$x3p[[1]] %>% x3p_show_xml("Manufacturer")
      
      # Generate Collapsible UI Panel List in a loop
      bsCollapsePanelList <- list()
      
      show_n <- min(c(sum(bsldata$samesource == TRUE) + 3, length(odridx)))
      # show all the best-phase comparisons and the three top comparisons
      for(idx in 1:show_n) {
        
        # Data Table Comparison ---------------------------------------------------
        
        BullCompBulls <- bulldata$comparison$bullets
        temptable <- data.frame(
          Feature = c("Left Land File", "Left Land MD5", "Left Land Instrument (resolution [µm/px])", 
                      "Right Land File", "Right Land MD5", "Left Land Instrument (resolution [µm/px])", 
                      "Cross Correlation Function", "Mean Distance btw Signals [Âµm]",
                      "Signal Length [mm]", "# Matching Striae Per Millimeter",
                      "# Mis-Matching Striae Per Millimeter", "CMS Per Millimeter",
                      "Non-CMS Per Millimeter", "Peak Sum"),
          Value = c(
            BullCompBulls$filename[BullCompBulls$bullet == input$comp_bul1 & BullCompBulls$land == bsldata$landA[odridx[idx]]],
            BullCompBulls$md5sum[BullCompBulls$bullet == input$comp_bul1 & BullCompBulls$land == bsldata$landA[odridx[idx]]],
            sprintf("%s (%s)", instrument, scale),
            BullCompBulls$filename[BullCompBulls$bullet == input$comp_bul2 & BullCompBulls$land == bsldata$landB[odridx[idx]]],
            BullCompBulls$md5sum[BullCompBulls$bullet == input$comp_bul2 & BullCompBulls$land == bsldata$landB[odridx[idx]]],
            sprintf("%s (%s)", instrument, scale),
            round(bsldata$ccf[odridx[idx]],3),
            round(bsldata$D[odridx[idx]],3),
            round(bsldata$length_mm[odridx[idx]],3),
            round(bsldata$matches_per_mm[odridx[idx]],3),
            round(bsldata$mismatches_per_mm[odridx[idx]],3),
            round(bsldata$cms_per_mm[odridx[idx]],3),
            round(bsldata$non_cms_per_mm[odridx[idx]],3),
            round(bsldata$sum_peaks[odridx[idx]],3)
          )
        )
        temptable_dt <- datatable(
          temptable, 
          rownames = FALSE, 
          options = list(paging = FALSE, ordering = FALSE, searching = FALSE, bLengthChange = FALSE, bInfo = FALSE)
        )
        
        # RGL Render Comparison ---------------------------------------------------
        
        local({
          cidx <- idx
          BullCompBulls <- bulldata$comparison$bullets
          rglLidx <- which(BullCompBulls$bullet == input$comp_bul1 & BullCompBulls$land == bsldata$landA[odridx[cidx]])
          rglRidx <- which(BullCompBulls$bullet == input$comp_bul2 & BullCompBulls$land == bsldata$landB[odridx[cidx]])
          rglL <- BullCompBulls$x3pimg[[rglLidx]]
          rglR <- BullCompBulls$x3pimg[[rglRidx]]
          output[[paste0("rglWinL",idx)]] <- renderImage({list(src = rglL, contentType = 'image/png')}, deleteFile = FALSE)
          output[[paste0("rglWinR",idx)]] <- renderImage({list(src = rglR, contentType = 'image/png')}, deleteFile = FALSE)
        })
        temp_rgl <- layout_column_wrap(
          width = 1/2,
          imageOutput(paste0("rglWinL",idx)),
          imageOutput(paste0("rglWinR",idx)),
          height = "250px"
        )
        
        # Groove Plot -------------------------------------------------------------
        
        local({
          cidx <- idx
          BullCompBulls <- bulldata$comparison$bullets
          GroovePlotLidx <- which(BullCompBulls$bullet == input$comp_bul1 & BullCompBulls$land == bsldata$landA[odridx[idx]])
          GroovePlotRidx <- which(BullCompBulls$bullet == input$comp_bul2 & BullCompBulls$land == bsldata$landB[odridx[idx]])
          GroovesL <- as.numeric(BullCompBulls$grooves[[GroovePlotLidx]]$groove)
          GroovesR <- as.numeric(BullCompBulls$grooves[[GroovePlotRidx]]$groove)
          CCDataL <- BullCompBulls$ccdata[[GroovePlotLidx]] - GroovesL[1]
          CCDataR <- BullCompBulls$ccdata[[GroovePlotRidx]] - GroovesR[1]
          output[[paste0("GroovePlotL",idx)]] <- renderPlot({
            groove_plot(CCDataL, GroovesL) +
              ggtitle(sprintf("Land %s profile", bsldata$land1[odridx[cidx]]))
          })
          output[[paste0("GroovePlotR",idx)]] <- renderPlot({
            groove_plot(CCDataR, GroovesR) +
              ggtitle(sprintf("Land %s profile", bsldata$land2[odridx[cidx]]))
          })
        })
        temp_groove <- list(
          fluidRow(
            column(6, plotOutput(paste0("GroovePlotL", idx)), align = "center"),
            column(6, plotOutput(paste0("GroovePlotR", idx)), align = "center")
          ),
          fluidRow(column(12, p("Shaded areas are excluded from the analysis"), align = "center"))
        )
        
        # Signal Comparison -------------------------------------------------------
        
        local({
          cidx <- idx
          BullCompComps <- bulldata$comparison$comparisons
          scale <- bulldata$cbull$x3p[[1]] %>% x3p_get_scale()
          SigPlotData <- BullCompComps$aligned[
            (BullCompComps$bulletA == input$comp_bul1) &
              (BullCompComps$bulletB == input$comp_bul2) &
              (BullCompComps$landA == bsldata$landA[odridx[idx]]) &
              (BullCompComps$landB == bsldata$landB[odridx[idx]])
          ][[1]]$lands
          SigPlotData <- tidyr::gather(SigPlotData, Signal, value, sig1, sig2)
          
          SigPlotData$Signal[SigPlotData$Signal == "sig1"] <- "Left LEA"
          SigPlotData$Signal[SigPlotData$Signal == "sig2"] <- "Right LEA"
          output[[paste0("SigPlot",idx)]] <- renderPlot({
            ggplot(SigPlotData, aes(x = x*scale, y = value, colour = Signal, linetype = Signal)) + 
              geom_line(na.rm = TRUE, alpha = 0.9, linewidth = 1) +
              scale_color_manual(values = c("darkorange", "purple4")) + 
              xlab("Position along width of Land [µm]") +
              ylab("Signal [µm]") +
              ggtitle("Aligned signals of LEAs")+
              theme(legend.position = "bottom") 
          })
        })
        temp_signal <- fluidRow(column(12, plotOutput(paste0("SigPlot", idx)), align = "center"))
        
        # Combine Results ---------------------------------------------------------
        
        panel_name <- paste0(bsldata$land1[odridx[idx]], " vs ", bsldata$land2[odridx[idx]]," (RF Score = ", round(bsldata$rfscore[odridx[idx]],4), ")")
        bsCollapsePanelList[[idx]] <- bsCollapsePanel(panel_name, temptable_dt, br(), temp_rgl, temp_groove, br(), temp_signal, style = "primary")
      }
      
      # Collapsible UI Panels ---------------------------------------------------
      LandComp <- do.call(bsCollapse, args = c(id = "collapseExample", multiple = TRUE, bsCollapsePanelList))
    }
    
    # Return Full Collapsible Report
    return(c(BullComp, LandComp$children))
  })
  
  
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
  
  # OUTPUT - Phase test score ----
  output$bull_comp_score <- renderText({
    req(phase$test_results)
    
    pt <- phase$test_results
    
    if (is.data.frame(pt)) {
      scores <- pt %>%
        group_by(samesource) %>% 
        summarize(avg = mean(scores, na.rm = TRUE)) %>%
        purrr::pluck("avg") %>%
        unlist()
      return(sprintf("Phase Test Score: %.4f", abs(diff(scores))))
    } else if (inherits(pt, "phase.test")) {
      return(sprintf("Phase Test Score: %.4f", pt$estimate))
    } else {
      stop("Phase test results have an unexpected class")
    }
  })
  
  # OUTPUT - Phase test probability of false id ----
  output$bull_comp_test <- renderText({
    req(phase$test_results)
    
    pt <- phase$test_results
    
    if (is.data.frame(pt)) {
      return("Test result unstable")
    } else if (inherits(pt, "phase.test")) {
      pval <- pt$p.value
      prob <- case_when(
        pval < 0.0000001 ~ "Less than 1 in 10 Million",
        pval < 0.000001 ~ "Less than 1 in 1 Million",
        pval < 0.00001 ~ "Less than 1 in 100,000",
        pval < 0.0001 ~ "Less than 1 in 10,000",
        pval < 0.001 ~ "Less than 1 in 1,000",
        pval < 0.01 ~ "Less than 1 in 100"
      )
      return(sprintf("Probability of False Identification: %s (Type I Error)", prob))
    } else {
      stop("Phase test results have an unexpected class")
    }
  })
  
  # OUTPUT - Bullet score matrix ----
  output$bull_comp <- renderPlot({
    req(bulldata$comparison)
    
    bullet_scores <- bulldata$comparison$bullet_scores
    bullet_scores$selsource <- FALSE
    bullet_scores$selsource[bullet_scores$bulletA == input$comp_bul1 & bullet_scores$bulletB == input$comp_bul2] <- TRUE
    bullet_scores$selsource[bullet_scores$bulletB == input$comp_bul1 & bullet_scores$bulletA == input$comp_bul2] <- TRUE
    bullet_scores %>% 
      ggplot(aes(x = bulletA, y = bulletB, fill = bullet_score, colour = selsource)) +
      geom_tile() +
      labs(fill = "Bullet Score") +
      scale_fill_gradient2(low = "grey80", high = "darkorange", midpoint = .5, limits = c(0,1)) +
      scale_colour_manual(values = c("black", "black")) +
      geom_tile(linewidth = 1, data = bullet_scores %>% filter(selsource)) +
      geom_text(aes(label = round(bullet_score, 2)), size = 6) +
      ggtitle("Bullet-to-Bullet Score Matrix") +
      xlab("") +
      ylab("") +
      guides(colour = "none") +
      coord_equal() 
  })
  
  # OUTPUT - Land score matrix ----
  output$land_comp <- renderPlot({
    req(bulldata$comparison)
    req(input$comp_bul1)
    req(input$comp_bul2)
    
    bullet_scores <- bulldata$comparison$bullet_scores
    bullet_scores <- bullet_scores[bullet_scores$bulletA == input$comp_bul1 & bullet_scores$bulletB == input$comp_bul2,]
    features <- bullet_scores %>% tidyr::unnest(data)
    features %>% 
      ggplot(aes(x = landA, y = landB, fill = rfscore, colour = samesource)) +
      geom_tile() +
      labs(fill = "Land Score") +
      scale_fill_gradient2(low = "grey80", high = "darkorange", midpoint = .5, limits = c(0,1)) +
      scale_colour_manual(values = c("black", "black")) +
      geom_tile(linewidth = 1, data = features %>% filter(samesource == TRUE)) +
      geom_text(aes(label = round(rfscore, 2)), size = 6) +
      xlab(sprintf("Lands on %s", features$bulletA[1])) +
      ylab(sprintf("Lands on %s", features$bulletB[1])) + 
      ggtitle("Land-to-Land Score Matrix",
              subtitle = sprintf("Bullet: %s vs %s", features$bulletA[1], features$bulletB[1])) + 
      guides(colour = "none") +
      coord_equal() 
  })
  
  # OUTPUT - Crosscut plots ----
  output$land_visCC <- renderPlot({
    req(bulldata$comparison)
    req(input$comp_bul1)
    req(input$comp_bul2)
    
    bullets <- bulldata$comparison$bullets
    bullets <- bullets[bullets$bullet %in% c(input$comp_bul1,input$comp_bul2), ]
    crosscuts <- bullets %>% tidyr::unnest(ccdata)
    crosscuts$x <- crosscuts$x / 1000
    CCplot <- crosscuts %>% 
      ggplot(aes(x = x, y = value)) + 
      geom_line() +
      facet_grid(bullet ~ land, labeller = "label_both") +
      xlab("Position along width of Land [mm]") +
      ylab("Surface Height [µm]") + 
      ggtitle("Cross-section of the bullet land at a suitable cross-section location") 
    return(CCplot)
  })
  
  # OUTPUT - Signal plots ----
  output$land_visSig <- renderPlot({
    req(bulldata$comparison)
    req(input$comp_bul1)
    req(input$comp_bul2)
    
    bullets <- bulldata$comparison$bullets
    bullets <- bullets[bullets$bullet %in% c(input$comp_bul1, input$comp_bul2),]
    signatures <- bullets %>% select(source, bullet, land, sigs) %>% tidyr::unnest(sigs)
    signatures$x <- signatures$x / 1000
    Sigplot <- signatures %>% 
      filter(!is.na(sig), !is.na(raw_sig)) %>%
      ggplot(aes(x = x)) + 
      geom_line(aes(y = raw_sig), colour = "grey70", show.legend = T) +
      geom_line(aes(y = sig), colour = "grey30", show.legend = T) +
      facet_grid(bullet ~ land, labeller = "label_both") +
      ylim(c(-5, 5)) +
      xlab("Position along width of Land [mm]") +
      ylab("Signal [µm]") +
      ggtitle("Raw and LOESS-smoothed Signal for Bullet Profile")
    return(Sigplot)
  })
  
}
