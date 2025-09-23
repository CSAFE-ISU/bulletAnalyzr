# UI for sidebar
reportSidebarUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("reportDownUI"))
}

# UI that displays phase test, score matrices, and plots in main tab panel
reportMainUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("report"))
}

reportServer <- function(id, bullet_data, comp_bul1, comp_bul2, phase_test_results) {
  moduleServer(id, function(input, output, session) {
    # OUTPUT UI - Report Comparison tab panel ----
    output$report <- renderUI({
      req(is.null(bullet_data$preCC))
      req(bullet_data$comparison)
      req(comp_bul1())
      req(comp_bul2())
      
      BullComp <- list(
        # Phase test results ----
        fluidRow(
          column(12, "SUMMARY OF RESULTS", align = "left", class = "h3"), 
          column(12, "Phase Test", align = "left", class = "h3"), 
          column(12, textOutput(session$ns("bull_comp_score")), align = "left", class = "h4"), 
          column(12, textOutput(session$ns("bull_comp_test")), align = "left", class = "h4"),
          hr(),
        ),
        br(),
        fluidRow(
          # Bullet score matrix ----
          column(6, plotOutput(session$ns("bull_comp"))),
          # Land score matrix ----
          column(6, plotOutput(session$ns("land_comp"))),
          # Score matrices help text ----
          column(12, tags$p("Higher scores indicate more similarity. The thick frames indicate the selected bullet comparison (left) and the land comparisons of the best phase (right).", class = "body"))
        ),
        br(),
        br(),
        # Crosscut plots ----
        fluidRow(column(12, plotOutput(session$ns("land_visCC")), align = "center")),
        br(),
        br(),
        # Signal plots ----
        fluidRow(column(12, plotOutput(session$ns("land_visSig")), align = "center")),
        br()
      )
      
      # Filter selected bullets ----
      bullet_scores <- filter_selected_bullets(
        bullet_scores = bullet_data$comparison$bullet_scores,
        selected1 = comp_bul1(),
        selected2 = comp_bul2()
      )
      if(nrow(bullet_scores$data[[1]]) > 0) {
        
        bsldata <- get_bsldata(bullet_scores = bullet_scores)
        odridx <- get_rf_order(bsldata)
        
        # getting scales and instrument info ... not correct yet, but just for the first scan
        scale <- bullet_data$cbull$x3p[[1]] %>% x3p_get_scale()
        instrument <- bullet_data$cbull$x3p[[1]] %>% x3p_show_xml("Manufacturer")
        
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
            output[[paste0("rglWinL",idx)]] <- renderImage({list(src = x3pimg_results$rglL, contentType = 'image/png')}, deleteFile = FALSE)
            output[[paste0("rglWinR",idx)]] <- renderImage({list(src = x3pimg_results$rglR, contentType = 'image/png')}, deleteFile = FALSE)
          })
          temp_rgl <- layout_column_wrap(
            width = 1/2,
            imageOutput(session$ns(paste0("rglWinL",idx))),
            imageOutput(session$ns(paste0("rglWinR",idx))),
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
            output[[paste0("GroovePlotL", cidx)]] <- renderPlot({
              groove_plot(results$CCDataL, results$GroovesL) +
                ggtitle(sprintf("Land %s profile", bsldata$land1[odridx[cidx]]))
            })
            output[[paste0("GroovePlotR", cidx)]] <- renderPlot({
              groove_plot(results$CCDataR, results$GroovesR) +
                ggtitle(sprintf("Land %s profile", bsldata$land2[odridx[cidx]]))
            })
          })
          temp_groove <- list(
            fluidRow(
              column(6, plotOutput(session$ns(paste0("GroovePlotL", idx))), align = "center"),
              column(6, plotOutput(session$ns(paste0("GroovePlotR", idx))), align = "center")
            ),
            fluidRow(column(12, p("Shaded areas are excluded from the analysis"), align = "center"))
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
            scale <- bullet_data$cbull$x3p[[1]] %>% x3p_get_scale()
            output[[paste0("SigPlot", cidx)]] <- renderPlot({
              plot_signal(sig_plot_data = sig_plot_data, scale = scale)
            })
          })
          temp_signal <- fluidRow(column(12, plotOutput(session$ns(paste0("SigPlot", idx))), align = "center"))
          
          # Combine Results ---------------------------------------------------------
          panel_name <- get_panel_name(bsldata = bsldata, odridx = odridx, idx = idx)
          bsCollapsePanelList[[idx]] <- bsCollapsePanel(
            panel_name, 
            temptable_dt, 
            br(), 
            temp_rgl, 
            temp_groove, 
            br(), 
            temp_signal, 
            style = "primary"
          )
        }
        
        # Collapsible UI Panels ---------------------------------------------------
        LandComp <- do.call(bsCollapse, args = c(id = "collapseExample", multiple = TRUE, bsCollapsePanelList))
      }
      
      # Return Full Collapsible Report
      return(c(BullComp, LandComp$children))
    })
    
    # OUTPUT UI - Report Download sidebar ----
    output$reportDownUI <- renderUI({
      req(is.null(bullet_data$preCC))
      req(bullet_data$comparison)
      
      # BUTTON - Download Report ----
      fluidRow(column(12, screenshotButton(label = "Download Report", id = session$ns("report"), filename="Bullet Comparison Report", scale = 2), align="center"))
    })
    
    # OUTPUT - Phase test score ----
    output$bull_comp_score <- renderText({
      req(phase_test_results)
      
      if (is.data.frame(phase_test_results)) {
        scores <- phase_test_results %>%
          group_by(samesource) %>% 
          summarize(avg = mean(scores, na.rm = TRUE)) %>%
          purrr::pluck("avg") %>%
          unlist()
        return(sprintf("Phase Test Score: %.4f", abs(diff(scores))))
      } else if (inherits(phase_test_results, "phase.test")) {
        return(sprintf("Phase Test Score: %.4f", phase_test_results$estimate))
      } else {
        stop("Phase test results have an unexpected class")
      }
    })
    
    # OUTPUT - Phase test probability of false id ----
    output$bull_comp_test <- renderText({
      req(phase_test_results)
      
      if (is.data.frame(phase_test_results)) {
        return("Test result unstable")
      } else if (inherits(phase_test_results, "phase.test")) {
        pval <- phase_test_results$p.value
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
      req(bullet_data$comparison)
      
      bullet_scores <- bullet_data$comparison$bullet_scores
      bullet_scores$selsource <- FALSE
      bullet_scores$selsource[bullet_scores$bulletA == comp_bul1() & bullet_scores$bulletB == comp_bul2()] <- TRUE
      bullet_scores$selsource[bullet_scores$bulletB == comp_bul1() & bullet_scores$bulletA == comp_bul2()] <- TRUE
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
      req(bullet_data$comparison)
      req(comp_bul1())
      req(comp_bul1())
      
      bullet_scores <- bullet_data$comparison$bullet_scores
      bullet_scores <- bullet_scores[bullet_scores$bulletA == comp_bul1() & bullet_scores$bulletB == comp_bul2(),]
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
      req(bullet_data$comparison)
      req(comp_bul1())
      req(comp_bul2())
      
      bullets <- bullet_data$comparison$bullets
      bullets <- bullets[bullets$bullet %in% c(comp_bul1(), comp_bul2()), ]
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
      req(bullet_data$comparison)
      req(comp_bul1())
      req(comp_bul2())
      
      bullets <- bullet_data$comparison$bullets
      bullets <- bullets[bullets$bullet %in% c(comp_bul1(), comp_bul2()),]
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
    
  })
}