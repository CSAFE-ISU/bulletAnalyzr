#' UI for the Report Sidebar
#'
#' @param id The module id
#'
#' @returns A Shiny UI element
#' @noRd
reportSidebarUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("reportDownUI"))
}

#' UI for the Report in the Main Panel
#'
#' @param id The module id
#'
#' @returns A Shiny UI element
#' @noRd
reportMainUI <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    shiny::uiOutput(ns("report")),
    shiny::uiOutput(ns("report_panels"))
  )
}

#' Server for the Report Module
#'
#' @param id The module id
#' @param bullet_data A data frame
#' @param comp_bul1 The name of the first selected bullet
#' @param comp_bul2 The name of the second selected bullet
#' @param phase_test_results The results of the phase test
#'
#' @returns NULL
#' @export
reportServer <- function(id, bullet_data = NULL, comp_bul1 = NULL, comp_bul2 = NULL, phase_test_results = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # OUTPUT UI - Report comparison top ----
    output$report <- shiny::renderUI({
      shiny::req(bullet_data)
      shiny::req(bullet_data$stage == "report")
      shiny::req(is.null(bullet_data$preCC))
      shiny::req(bullet_data$comparison)
      shiny::req(comp_bul1())
      shiny::req(comp_bul2())
      
      progress <- shiny::Progress$new(); on.exit(progress$close())
      
      # Render crosscut snapshots ----
      bullet_data$comparison$bullets <- render_crosscut_snap_wrapper(
        bullets = bullet_data$comparison$bullets, 
        progress = progress
      )
      
      # Filter selected bullets ----
      bullet_scores <- filter_bulletA_bulletB_cols(
        df = bullet_data$comparison$bullet_scores,
        selected1 = comp_bul1(),
        selected2 = comp_bul2()
      )
      
      bsldata <- get_bsldata(bullet_scores = bullet_scores)
      odridx <- get_rf_order(bsldata)
      
      # getting scales and instrument info ... not correct yet, but just for the first scan
      scale <- bullet_data$allbull$x3p[[1]] %>% x3ptools::x3p_get_scale()
      instrument <- bullet_data$allbull$x3p[[1]] %>% x3ptools::x3p_show_xml("Manufacturer")
      
      # Generate Collapsible UI Panel List in a loop
      bsCollapsePanelList <- list()
      
      show_n <- min(c(sum(bsldata$samesource == TRUE) + 3, length(odridx)))
      # show all the best-phase comparisons and the three top comparisons
      for(idx in 1:show_n) {
        
        # Data Table Comparison ---------------------------------------------------
        temptable_dt <- make_temptable(
          BullCompBulls = bullet_data$comparison$bullets, 
          selected1 = comp_bul1(), 
          selected2 = comp_bul2(),
          bsldata = bsldata, 
          odridx = odridx, 
          idx = idx, 
          instrument = instrument, 
          scale = scale
        )
        
        # RGL Render Comparison ---------------------------------------------------
        local({
          x3pimg_results <- filter_x3pimg(
            BullCompBulls = bullet_data$comparison$bullets,
            selected1 = comp_bul1(),
            selected2 = comp_bul2(),
            bsldata = bsldata,
            odridx = odridx,
            cidx = idx
          )
          output[[paste0("rglWinL",idx)]] <- shiny::renderImage({list(src = x3pimg_results$rglL, contentType = 'image/png')}, deleteFile = FALSE)
          output[[paste0("rglWinR",idx)]] <- shiny::renderImage({list(src = x3pimg_results$rglR, contentType = 'image/png')}, deleteFile = FALSE)
        })
        temp_rgl <- bslib::layout_column_wrap(
          width = 1/2,
          shiny::imageOutput(session$ns(paste0("rglWinL",idx))),
          shiny::imageOutput(session$ns(paste0("rglWinR",idx))),
          height = "250px"
        )
        
        # Groove Plot -------------------------------------------------------------
        local({
          results <- filter_grooves_ccdata(
            BullCompBulls = bullet_data$comparison$bullets,
            selected1 = comp_bul1(),
            selected2 = comp_bul2(),
            bsldata = bsldata,
            odridx = odridx,
            cidx = idx
          )
          cidx <- idx
          output[[paste0("GroovePlotL", cidx)]] <- shiny::renderPlot({
            groove_plot(results$CCDataL, results$GroovesL) +
              ggplot2::ggtitle(sprintf("Land %s profile", bsldata$land1[odridx[cidx]]))
          })
          output[[paste0("GroovePlotR", cidx)]] <- shiny::renderPlot({
            groove_plot(results$CCDataR, results$GroovesR) +
              ggplot2::ggtitle(sprintf("Land %s profile", bsldata$land2[odridx[cidx]]))
          })
        })
        temp_groove <- list(
          shiny::fluidRow(
            shiny::column(6, shiny::plotOutput(session$ns(paste0("GroovePlotL", idx))), align = "center"),
            shiny::column(6, shiny::plotOutput(session$ns(paste0("GroovePlotR", idx))), align = "center")
          ),
          shiny::fluidRow(shiny::column(12, shiny::tags$p("Shaded areas are excluded from the analysis"), align = "center"))
        )
        
        # Signal Comparison -------------------------------------------------------
        local({
          sig_plot_data <- filter_sig_plot_data(
            BullCompComps = bullet_data$comparison$comparisons,
            selected1 = comp_bul1(),
            selected2 = comp_bul2(),
            bsldata = bsldata,
            odridx = odridx,
            cidx = idx
          )
          cidx <- idx
          scale <- bullet_data$allbull$x3p[[1]] %>% x3ptools::x3p_get_scale()
          output[[paste0("SigPlot", cidx)]] <- shiny::renderPlot({
            plot_signal(sig_plot_data = sig_plot_data, scale = scale)
          })
        })
        temp_signal <- shiny::fluidRow(shiny::column(12, shiny::plotOutput(session$ns(paste0("SigPlot", idx))), align = "center"))
        
        # Combine Results ---------------------------------------------------------
        panel_name <- get_panel_name(bsldata = bsldata, odridx = odridx, idx = idx)
        bsCollapsePanelList[[idx]] <- shinyBS::bsCollapsePanel(
          panel_name, 
          temptable_dt, 
          shiny::br(), 
          temp_rgl,
          temp_groove, 
          shiny::br(), 
          temp_signal, 
          style = "primary"
        )
      }
      
      # Collapsible UI Panels ---------------------------------------------------
      LandComp <- do.call(shinyBS::bsCollapse, args = c(id = "collapseExample", multiple = TRUE, bsCollapsePanelList))
      
      report_parts <- htmltools::tagList(
        # Phase test results ----
        shiny::fluidRow(
          shiny::column(12, "SUMMARY OF RESULTS", align = "left", class = "h3"), 
          shiny::column(12, "Phase Test", align = "left", class = "h3"), 
          shiny::column(12, shiny::textOutput(session$ns("bull_comp_score")), align = "left", class = "h4"), 
          shiny::column(12, shiny::textOutput(session$ns("bull_comp_test")), align = "left", class = "h4"),
          shiny::hr(),
        ),
        shiny::br(),
        shiny::fluidRow(
          # Bullet score matrix ----
          shiny::column(6, shiny::plotOutput(session$ns("bull_comp"))),
          # Land score matrix ----
          shiny::column(6, shiny::plotOutput(session$ns("land_comp"))),
          # Score matrices help text ----
          shiny::column(12, shiny::tags$p("Higher scores indicate more similarity. The thick frames indicate the selected bullet comparison (left) and the land comparisons of the best phase (right).", class = "body"))
        ),
        shiny::br(),
        shiny::br(),
        # Crosscut plots ----
        shiny::fluidRow(shiny::column(12, shiny::plotOutput(session$ns("land_visCC")), align = "center")),
        shiny::br(),
        shiny::br(),
        # Signal plots ----
        shiny::fluidRow(shiny::column(12, shiny::plotOutput(session$ns("land_visSig")), align = "center")),
        shiny::br(),
        shiny::br(),
        LandComp$children
      )
  
      return(report_parts)
    })
    
    # OUTPUT UI - Report Download sidebar ----
    output$reportDownUI <- shiny::renderUI({
      shiny::req(bullet_data)
      shiny::req(bullet_data$stage == "report")
      shiny::req(is.null(bullet_data$preCC))
      shiny::req(bullet_data$comparison)
      
      # BUTTON - Download Report ----
      htmltools::tagList(
        shiny::fluidRow(shiny::column(12, shinyscreenshot::screenshotButton(label = "Download Report", id = session$ns("report"), filename="Bullet Comparison Report", scale = 2), align="center")),
        shiny::br(),
        shiny::fluidRow(shiny::column(12, downloadDataUI(session$ns("data1"))), align = "center")
      )
    })
    
    downloadDataServer("data1", bullet_data = bullet_data, phase_test_results = phase_test_results, drop_x3p = TRUE)
    
    # OUTPUT - Phase test score ----
    output$bull_comp_score <- shiny::renderText({
      shiny::req(bullet_data)
      shiny::req(bullet_data$stage == "report")
      shiny::req(phase_test_results)
      
      if (is.data.frame(phase_test_results)) {
        scores <- get_phase_scores_wrapper(phase_test_results = phase_test_results)
        return(sprintf("Phase Test Score: %.4f", abs(diff(scores))))
      } else if (inherits(phase_test_results, "phase.test")) {
        return(sprintf("Phase Test Score: %.4f", phase_test_results$estimate))
      } else {
        stop("Phase test results have an unexpected class")
      }
    })
    
    # OUTPUT - Phase test probability of false id ----
    output$bull_comp_test <- shiny::renderText({
      shiny::req(bullet_data)
      shiny::req(bullet_data$stage == "report")
      shiny::req(phase_test_results)
      
      if (is.data.frame(phase_test_results)) {
        return("Test result unstable")
      } else if (inherits(phase_test_results, "phase.test")) {
        pval <- phase_test_results$p.value
        prob <- dplyr::case_when(
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
    output$bull_comp <- shiny::renderPlot({
      shiny::req(bullet_data)
      shiny::req(bullet_data$stage == "report")
      shiny::req(bullet_data$comparison)
      
      bullet_scores <- bullet_data$comparison$bullet_scores
      bullet_scores$selsource <- FALSE
      bullet_scores$selsource[bullet_scores$bulletA == comp_bul1() & bullet_scores$bulletB == comp_bul2()] <- TRUE
      bullet_scores$selsource[bullet_scores$bulletB == comp_bul1() & bullet_scores$bulletA == comp_bul2()] <- TRUE
      plot_bullet_score_matrix(bullet_scores = bullet_scores)
    })
    
    # OUTPUT - Land score matrix ----
    output$land_comp <- shiny::renderPlot({
      shiny::req(bullet_data)
      shiny::req(bullet_data$stage == "report")
      shiny::req(bullet_data$comparison)
      shiny::req(comp_bul1())
      shiny::req(comp_bul1())
      
      features <- filter_bulletA_bulletB_cols(
        df = bullet_data$comparison$bullet_scores,
        selected1 = comp_bul1(),
        selected2 = comp_bul2(),
        unnest_data = "data"
      )
      
      plot_land_score_matrix(features = features)
      
    })
    
    # OUTPUT - Crosscut plots ----
    output$land_visCC <- shiny::renderPlot({
      shiny::req(bullet_data)
      shiny::req(bullet_data$stage == "report")
      shiny::req(bullet_data$comparison)
      shiny::req(comp_bul1())
      shiny::req(comp_bul2())
      
      crosscuts <- filter_bullet_col(
        df = bullet_data$comparison$bullets,
        selected = c(comp_bul1(), comp_bul2()),
        unnest_data = "ccdata"
      )
      
      plot_all_crosscuts(crosscuts = crosscuts)
    
    })
    
    # OUTPUT - Signal plots ----
    output$land_visSig <- shiny::renderPlot({
      shiny::req(bullet_data)
      shiny::req(bullet_data$stage == "report")
      shiny::req(bullet_data$comparison)
      shiny::req(comp_bul1())
      shiny::req(comp_bul2())
      
      bullets <- bullet_data$comparison$bullets
      bullets <- bullets[bullets$bullet %in% c(comp_bul1(), comp_bul2()),]
      
      plot_all_signals(bullets = bullets)
    })
    
  })
}
