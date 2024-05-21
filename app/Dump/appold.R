## Load Libraries
library(shiny)
library(shinythemes)
library(shinyjs)
library(shinyBS)
library(shinycssloaders)
library(plotly)
library(dplyr)
library(tidyr)
library(bulletr)
library(ggplot2)
library(gridExtra)
library(randomForest)

options(shiny.maxRequestSize = 30*1024^2)

## Data Resources
bullet_choices <- file.path("data", "bullets", dir("data/bullets"))
names(bullet_choices) <- basename(bullet_choices)
addResourcePath("images", "images")

## UI  
ui <- fluidPage(title = "BulletAnalyzr",
        useShinyjs(),
    
    tags$head(
        tags$link(
            href = "https://fonts.googleapis.com/css?family=Montserrat:400,500,700,900|Ubuntu:400,500,700",
            rel = "stylesheet",
            type = "text/css"
        ),
        tags$link(rel = "shortcut icon", href = "favicon.png", type = "image/png"),
        tags$link(rel = "icon", href = "favicon.png", type = "image/png")
    ),
    includeCSS("css/styles.css"),

    # Use tags$script to include custom JavaScript
    tags$head(
        tags$script(
            HTML("
                $(document).on('click', '#printBtn', function() {
                    window.print();
                });
            ")
        )
    ),

    tags$div(id="app-container",
        fluidRow(
            column(width = 4,tags$a(target = "_blank", href="https://forensicstats.org", tags$img(src = "images/BulletAnalzr-Mark-2.png", width="500px"))),
            column(width = 4,br()),
            column(width = 4,tags$a(target = "_blank", href="https://forensicstats.org", tags$img(src = "images/BulletAnalyzr-Design-2.png", width="500px")),align="right"),
        ),
        tags$div(id="main-content",
    navbarPage(title = NULL,#div(div(id = "img-id",img(src = "csafe_tools_blue.png")),NULL),
    tabPanel("Home",
    sidebarLayout(
        tags$div(id="my-sidebar", sidebarPanel(
            # tags$h1(class = "responsive-text", "BULLETANALYZER"),
            # br(),
            # tags$head(tags$style("#info{font-size: 18px;}")),
            
            hidden(checkboxInput("stage0", "Stage 0")),
            hidden(checkboxInput("stage1", "Stage 1")),
            hidden(checkboxInput("stage2", "Stage 2")),
            hidden(checkboxInput("stage3", "Stage 3")),
            hidden(checkboxInput("stage4", "Stage 4")),
            hidden(checkboxInput("stage5", "Stage 5")),
            hidden(checkboxInput("stage6", "Stage 6")),
            
            hidden(checkboxInput("stage00", "Stage 0")),
            hidden(checkboxInput("stage11", "Stage 1")),
            hidden(checkboxInput("stage22", "Stage 2")),
            hidden(checkboxInput("stage33", "Stage 3")),
            hidden(checkboxInput("stage44", "Stage 4")),
            hidden(checkboxInput("stage55", "Stage 5")),
            hidden(checkboxInput("stage66", "Stage 6")),
            
            shinyjs::hidden(
                div(id = "stepstep",
                    h4("Step-By-Step Procedure"),
                    helpText("Press the following button to begin the step-by-step version of the algorithm, where each parameter can be tweaked according to your liking."),
                    actionButton("confirm_stepstep", "Begin Step-By-Step", icon = icon("check"))
                ),
                
                hr()
            ),
            
            div(id = "autonomous",
                tags$h1(class = "responsive-text","GET STARTED"),
                br(),
                helpText("Press the following button to automatically use all the default parameters to get a similarity value for two bullet lands."),
                br(),
                actionButton("confirm_autonomous", "Begin")#, icon = icon("check"))
            ),
            
            shinyjs::hidden(
                div(id = "landselect",
                    conditionalPanel(condition = "!input.stage0 || input.stage5",
                                     br(),
                                     
                                     h4("Stage 1 Options"),
                                     
                                     selectInput("choose1", "Choose First Land", choices = c("Upload Image", bullet_choices), selected = bullet_choices[5]),
                                     
                                     conditionalPanel(condition = "input.choose1 == 'Upload Image'",
                                                      fileInput("file1", "First Bullet Land")                 
                                     ),
                                     
                                     selectInput("choose2", "Choose Second Land", choices = c("Upload Image", bullet_choices), selected = bullet_choices[7]),
                                     
                                     conditionalPanel(condition = "input.choose2 == 'Upload Image'",
                                                      fileInput("file2", "Second Bullet Land")              
                                     ),
                                     
                                     actionButton("confirm0", "Generate Report")#, icon = icon("check")),
                                     
                                     # hr()
                    )
                )
            ),
            
            conditionalPanel(condition = "input.stage5", hr()),
            
            conditionalPanel(condition = "input.stage0 && !input.stage1 || input.stage5",
                h4("Stage 2 Options"),
                
                hr(),
                
                sliderInput("xcoord1", "X Coordinate (First Land)", min = 1, max = 251, value = 136, step = 1),
                sliderInput("xcoord2", "X Coordinate (Second Land)", min = 252, max = 502, value = 386, step = 1),
                
                hr(),
                
                actionButton("confirm", "Confirm Coordinates", icon = icon("check")),
                
                # hr(),
                
                # actionButton("back", "Back to Stage 1", icon = icon("backward"))
            ),
            
            conditionalPanel(condition = "input.stage5", hr()),
            
            conditionalPanel(condition = "input.stage1 && !input.stage2 || input.stage5",
                h4("Stage 3 Options"),
                
                hr(),
                
                sliderInput("bounds1", "Coordinate Bounds 1", min = 0, max = 2400, value = c(0, 2400)),
                sliderInput("bounds2", "Coordinate Bounds 2", min = 0, max = 2400, value = c(0, 2400)),
                
                hr(),
                
                actionButton("confirm2", "Confirm Bounds", icon = icon("check")),
                 
                # hr(),
                
                # actionButton("back2", "Back to Stage 2", icon = icon("backward"))
            ),
            
            conditionalPanel(condition = "input.stage5", hr()),
            
            conditionalPanel(condition = "input.stage2 && !input.stage3 || input.stage5",
                h4("Stage 4 Options"),
                
                hr(),
                
                sliderInput("span", "Loess Span", min = 0.01, max = 0.99, value = 0.03, step = 0.01),
                
                hr(),
                
                actionButton("confirm3", "Confirm Span", icon = icon("check")),
                
                # hr(),
                
                # actionButton("back3", "Back to Stage 3", icon = icon("backward"))
            ),
            
            conditionalPanel(condition = "input.stage5", hr()),
            
            conditionalPanel(condition = "input.stage3 && !input.stage4 || input.stage5",
                h4("Stage 5 Options"),
                
                hr(),
                
                numericInput("alignment", "Alignment", min = -1000, max = 1000, step = 1.5625, value = 0),
                
                hr(),
                
                actionButton("confirm4", "Confirm Alignment", icon = icon("check")),
                
                # hr(),
                
                # actionButton("back4", "Back to Stage 4", icon = icon("backward"))
            ),
            
            conditionalPanel(condition = "input.stage4",
                 h4("Stage 6 Options"),
                 
                 hr(),
                 
                 sliderInput("smoothfactor", "Smoothing Factor", min = 1, max = 100, value = 35, step = 1),
                 
                 hr(),
                 
                 actionButton("confirm5", "Confirm Smoothing", icon = icon("check")),
                 
                 # hr(),
                 
                 #actionButton("back5", "Back to Stage 5", icon = icon("backward"))
            ),
            
            conditionalPanel(condition = "input.stage5",
                             h4("Stage 7 Options"),
                             
                             hr(),
                             
                             actionButton("confirm6", "Confirm Features", icon = icon("check")),
                             
                             # hr(),
                             
                             # actionButton("back6", "Back to Stage 6", icon = icon("backward"))
            ),
            
            hidden(
                h4("Lighting Options"),
                sliderInput("subsample", "Subsample Factor", min = 1, max = 20, value = 2),
                sliderInput("ambient_lighting", "Ambient Lighting", min = 0, max = 1, step = 0.1, value = 0.8),
                sliderInput("diffuse_lighting", "Diffuse Lighting", min = 0, max = 1, step = 0.1, value = 0.8),
                sliderInput("specular_lighting", "Specular Lighting", min = 0, max = 2, step = 0.05, value = 0.05),
                sliderInput("roughness_lighting", "Roughness Lighting", min = 0, max = 1, step = 0.1, value = 0.5),
                sliderInput("fresnel_lighting", "Fresnel Lighting", min = 0, max = 5, step = 0.1, value = 0.2)
            ),
            
            br(),
            conditionalPanel(condition = "input.stage5",
                h4("RESULTS EXPORT"),
                helpText("Press the following button to export the given results to a PDF file, or print the results."),
                actionButton("printBtn", "Print / Save to PDF")
            )
        )),
        
        mainPanel(
              conditionalPanel(condition = "input.stage5",
                   div(class = "center-container",
                       div(class = "rounded-box",
                           div(style = "text-align: center; color: #9E1A97; font-size: 32px; font-weight: 700; font-family: 'Montserrat', sans-serif;", "RESULTS:"),
                           div(style = "text-align: center;", h4(uiOutput("rfpred")))
                       )
                   ),
                   
                   hr(),
                   
                   h3("Features"),
                   hr(),
                   HTML("Here are the values of the features computed on the aligned bullet signatures."),
                   
                   DT::dataTableOutput("features"),
                   
                   hr(),
                   
                   actionButton("restart", "Restart Algorithm", icon = icon("refresh")),
                   
                   hr()
              ),
            conditionalPanel(condition = "input.stage0 && !input.stage1 || input.stage5",
                 h3("Stage 2: Finding a Stable Region"),
                 hr(),
                 HTML("Below you will find surface topologies of the two bullet lands you have uploaded. You can rotate, pan, zoom, and perform a number of other functions to examine the surfaces.<br><br>Our goal is to find a <b>stable region</b>. We want an area of the bullet where there is minimal noise or tank rash, but plenty of pronounced striation markings.<br><br>We step through cross-sections of each land at a fixed step size, and uses the CCF (cross-correlation function) to determine stability (a high CCF means that subsequent cross-sections are similar to each other). We begin this procedure near the area where striation markings are typically most pronounced.<br><br><b>We have automatically identified what is believed to be a stable region.</b> You may choose the location to take a cross-section if the algorithm's choice is not satisfactory.")           
            ),
            conditionalPanel(condition = "input.stage1 && !input.stage2 || input.stage5",
                 h3("Stage 3: Removing Grooves"),
                 hr(),
                 HTML("The cross-sections you have taken are shown below. Our next goal will be to remove the grooves, which contain no relevant information for matching, and greatly exceed the size of a typical striation mark. The more accurate we are with groove detection, the less noise we introduce to the remaining steps.<br><br>We use a double-pass smoothing method to determine the location of the grooves. <b>We have again attempted to locate the grooves for you</b>, but you may define them yourself. As you adjust the sliders, the plot will automatically update."),
                 hr(),
                 
                 withSpinner(plotOutput("crosssection"))
            ),
            conditionalPanel(condition = "input.stage2 && !input.stage3 || input.stage5",
                 h3("Stage 4: Removing Global Structure"),
                 hr(),
                 HTML("We have removed the grooves, but the global structure of the cross-section still dominates the overall appearance, making striae more difficult to locate. At this point we are going to fit a loess regression to model this structure. The loess regression includes a span parameter which adjusts the amount of smoothing used. Different values will yield different output. We default to a span of 0.03, but this may be adjusted as desired. "),
                 hr(),
                 
                 withSpinner(plotOutput("loess1")),
                 withSpinner(plotOutput("loess2"))
            ),
            conditionalPanel(condition = "input.stage3 && !input.stage4 || input.stage5",
                 h3("Stage 5: Aligning Signatures"),
                 hr(),
                 HTML("The residuals from the loess fit we have extracted in the previous stage are called the bullet <b>signatures</b>. They will form the basis for the rest of the analysis. Since scans are not always taken at a fixed location, and the rotation of the bullet may be slightly less than ideal, the signatures may not automatically `align` in their respective coordinate spaces. <br><br> Because the signatures are defined by the residuals, the peaks and valleys visible in this plot represent the striation markings we are looking for. In order to make matching easier, our next step is to align the two signatures. <br><br> The alignment parameter defines a horizontal offset that will shift the green graph to the right.  The objective is to create perfect alignment between the graphs so that the correlation between the two will be maximized. We suggest an optimal alignment, but it can be adjusted if necessary."),
                 
                 plotOutput("alignment")
            ),
            conditionalPanel(condition = "input.stage4",
                             h3("Stage 6: Peaks and Valleys"),
                             hr(),
                             HTML("With aligned signatures, we now turn our attention to determining what constitutes a peak or a valley. Since there is a lot of noise, this step involves one more smoothing pass. The amount of smoothing reduces the possibility of noisy detections of peaks and valleys in the signature, but too much smoothing can smooth over some real features of the signature. <br><br> We can specify a smoothing window, called the <b>smoothing factor</b>, as the number of neighbors to include in the window. For instance, a value of 16 would mean that the nearest 16 points, spanning 16 * 1.5625 = 25 micrometers, would be included."),
                             
                             withSpinner(plotOutput("peaks1")),
                             withSpinner(plotOutput("peaks2"))
            ),
            #conditionalPanel(condition = "input.stage5",
            #     h3("Stage 6: Extract Features"),
            #     hr(),
            #     HTML("We now have smoothed, aligned bullet signatures with associated peaks and valleys. This gives us a number of features we can extract.<br><br>At this point, there is really nothing left to configure about the algorithm. The features extracted are displayed below. The definitions of each can be found in Hare 2016. Press Confirm Features when you are ready to get your predicted probability of a match.")),
            #     
            #     dataTableOutput("features")
            #),
            
            
            h3("WELCOME TO BULLETANALYZR!"),
            p("Our innovation combines 3D imagery and sophisticated algorithms to revolutionize bullet analysis. This prototype demonstrates how our methods can calculate the likelihood of the observed similarity if two bullets originated from the same firearm versus different firearms. It's a work in progress, evolving through feedback from diverse communities."),
            br(),
            h4("BULLET LAND SURFACES",align="center"),
            withSpinner(plotlyOutput("trendPlot", height = "700px")),
            hr()
        )
    )),
                            tabPanel(   
                                        "About",
                                        h4(HTML("CSAFE Tools is a software suite of state-of-the-art statistical libraries designed to assist practitioners in analyzing forensic data. This work was developed in collaboration with the Center for Statistics and Applications in Forensic Evidence (CSAFE) at Iowa State University and Omni Analytics Group. These procedures are fully open-source and transparent. For more details on the underlying code, please see the <a href='https://github.com/OAITI/bulletmatcher' target='_blank'>GitHub repository</a> for the companion R package.")),
                                        br(), br(),
                                        h4(HTML("This software is an implementation of a front-end to the <a href='https://github.com/CSAFE-ISU/bulletr' target='_blank'>bulletr package</a>.")),
                                        h4(HTML("This application will walk through the steps used to programmatically determine the probability that two bullets were fired from the same gun. During discharge, as a bullet travels out of the chamber, it is imprinted with a groove signature that is unique to that gun’s barrel. The grooved pattern of a gun’s barrel is so distinct that the striations that are imprinted on a set of fired bullet need only be matched across a small region for there to be statistical confidence of a match; therefore probabilistic comparisons can be made at the bullet land level which represent only one-sixth of a bullet.<br><br>
                                                                Hare, E., Hofmann, H., and Carriquiry, A., Algorithmic Approaches to Match Degraded Land Impressions. Law, Probability and Risk, mgx018, <a href='https://doi.org/10.1093/lpr/mgx018' target='_blank'>https://doi.org/10.1093/lpr/mgx018</a><br>
                                                                Hare, E., Hofmann, H., and Carriquiry, A., Automatic Matching of Bullet Land Impressions. Annals of Applied Statistics. doi: 10.1214/17-AOAS1080"
                                            )),
                                            hr()
                        ),
                        tabPanel("Instructions",),
                        tabPanel("Contact",)
                        ))),
    # Footer
    tags$div(id="global-footer",
        fluidRow(
            column(width = 4,tags$img(src="csafe_tools_blue.png", alt="Logo", height = "60px")),
            column(width = 4,tags$p("195 Durham Center, 613 Morrill Road, Ames, Iowa, 50011")),
            column(width = 4,tags$p("(C) 2023 | All Rights Reserved", class="right-float"))
        )
    )
)

server <- function(input, output, session) {
    # showModal(modalDialog(
    #     title = "Welcome to BulletAnalyzr! ",
    #     " Our innovation combines 3D imagery and sophisticated algorithms to revolutionize bullet analysis. This prototype demonstrates how our methods can calculate the likelihood of the observed similarity if two bullets originated from the same firearm versus different firearms. It's a work in progress, evolving through feedback from diverse communities. Click 'Get Started' on the next screen to explore this prototype further.",
    #     easyClose = TRUE,
    #     footer = modalButton("Ok")
    # ))

    bullet1 <- reactive({
        withProgress(message = "Loading bullet data...", expr = {
            if (input$choose1 == "Upload Image") {
                if (is.null(input$file1)) return(NULL)
                
                return(read_x3p(input$file1$datapath))    
            }
            
            return(read_x3p(input$choose1))
        })
    })
    
    bullet2 <- reactive({
        withProgress(message = "Loading bullet data...", expr = {
            if (input$choose2 == "Upload Image") {
                if (is.null(input$file2)) return(NULL)
                
                return(read_x3p(input$file2$datapath))    
            }
            
            cat(input$file2)
            
            return(read_x3p(input$choose2))
        })
    })
    
    values <- reactiveValues(app_type = "stepstep")
    
    observeEvent(input$confirm_autonomous, {
        values$app_type <- "autonomous"
        
        shinyjs::hide("stepstep")
        shinyjs::hide("confirm_autonomous")
        shinyjs::show("landselect")
        shinyjs::hide("prelim")
        shinyjs::show("land")
    })
    
    observeEvent(input$confirm_stepstep, {
        values$app_type <- "stepstep"

        shinyjs::hide("autonomous")
        shinyjs::hide("confirm_stepstep")
        shinyjs::show("landselect")
        shinyjs::hide("prelim")
        shinyjs::show("land")
    })
    
    observeEvent(input$confirm0, {
        shinyjs::hide("autonomous")
        shinyjs::hide("stepstep")
        
        if (!is.null(bullet1()) && !is.null(bullet2())) {
            updateCheckboxInput(session, "stage0", value = TRUE)
            if (values$app_type == "autonomous") {
                updateCheckboxInput(session, "stage00", value = TRUE)
            }
        }    
    })
    
    observeEvent(input$stage00, {
        if (input$confirm0 && input$stage00) {
            updateCheckboxInput(session, "stage1", value = TRUE)
            updateCheckboxInput(session, "stage11", value = TRUE)
        }
    }, priority = -1)
    
    observeEvent(input$stage11, {
        if (input$confirm0 && input$stage11 && values$app_type == "autonomous") {
            updateCheckboxInput(session, "stage2", value = TRUE)
            updateCheckboxInput(session, "stage22", value = TRUE)
        }
    }, priority = -1)
    
    observeEvent(input$stage22, {
        if (input$confirm0 && input$stage22 && values$app_type == "autonomous") {
            updateCheckboxInput(session, "stage3", value = TRUE)
            updateCheckboxInput(session, "stage33", value = TRUE)            
        }
    }, priority = -1)
    
    observeEvent(input$stage33, {
        if (input$confirm0 && input$stage33 && values$app_type == "autonomous") {
            updateCheckboxInput(session, "stage4", value = TRUE)
            updateCheckboxInput(session, "stage44", value = TRUE)            
        }
    }, priority = -1)
    
    observeEvent(input$stage44, {
        if (input$confirm0 && input$stage44 && values$app_type == "autonomous") {
            updateCheckboxInput(session, "stage5", value = TRUE)
            updateCheckboxInput(session, "stage55", value = TRUE)            
        }
    }, priority = -1)
    
    observeEvent(input$stage55, {
        if (input$confirm0 && input$stage55 && values$app_type == "autonomous") {
            updateCheckboxInput(session, "stage6", value = TRUE)
            updateCheckboxInput(session, "stage66", value = TRUE)            
        }
    }, priority = -1)
    
    theSurface <- reactive({
        if (is.null(bullet1()) || is.null(bullet2())) return(NULL)
        
        b1 <- bullet1()
        b2 <- bullet2()
        
        surf.b1 <- b1[[2]]
        surf.b2 <- b2[[2]]
        
        minrows <- min(nrow(surf.b1), nrow(surf.b2))
        
        surf.mat <- cbind(surf.b1[1:minrows,], surf.b2[1:minrows,])
        
        x_idx <- seq(1, nrow(surf.mat), by = 2)
        y_idx <- seq(1, ncol(surf.mat), by = 2)
        
        return(surf.mat[x_idx, y_idx])
    })
    
    observe({
        updateSliderInput(session, "xcoord1", max = ncol(theSurface()) / 2, value = ncol(theSurface()) / 4)
        updateSliderInput(session, "xcoord2", max = ncol(theSurface()), min = 1 + ncol(theSurface()) / 2, value = ncol(theSurface()) * 3 / 4)
    })
    
    output$trendPlot <- renderPlotly({
        if (is.null(theSurface())) return(NULL)
        
        p <- plot_ly(z = theSurface(), type = "surface", showscale = FALSE, lighting = list(ambient = input$ambient_lighting,
                                                                                            diffuse = input$diffuse_lighting,
                                                                                            specular = input$specular_lighting,
                                                                                            roughness = input$roughness_lighting,
                                                                                            fresnel = input$fresnel_lighting))
        p
    })
    
    observeEvent(input$stage0, {
        if (!is.null(theSurface()) && input$stage0) {
            withProgress(message = "Calculating CCF...", expr = {
                crosscut1 <- bulletCheckCrossCut("",
                                                 bullet = bullet1(),
                                                 xlimits = seq(25, 500, by = 25))
                
                crosscut2 <- bulletCheckCrossCut("",
                                                 bullet = bullet2(),
                                                 xlimits = seq(25, 500, by = 25))
                
                updateSliderInput(session, "xcoord1", value = crosscut1)
                updateSliderInput(session, "xcoord2", value = crosscut2 + ncol(theSurface()) / 2)
            })
        }
    })
    
    observeEvent(input$confirm, {
        updateCheckboxInput(session, "stage1", value = TRUE)
    })
    
    observeEvent(input$back, {
        updateCheckboxInput(session, "stage0", value = FALSE)
    })
    
    fortified1 <- reactive({
        if (is.null(bullet1()) || !input$stage1) return(NULL)
        
        bul <- bullet1()
        bul[[3]] <- "b1"
        names(bul)[3] <- "path"
        
        return(fortify_x3p(bul))
    })
    
    fortified2 <- reactive({
        if (is.null(bullet2()) || !input$stage1) return(NULL)
        
        bul <- bullet2()
        bul[[3]] <- "b2"
        names(bul)[3] <- "path"
        
        return(fortify_x3p(bul))
    })
    
    crosscut1 <- reactive({
        if (is.null(bullet1()) || !input$stage1) return(NULL)
        
        return(get_crosscut(bullet = bullet1(), x = input$xcoord1))
    })
    
    crosscut2 <- reactive({
        if (is.null(bullet2()) || !input$stage1) return(NULL)
        
        return(get_crosscut(bullet = bullet2(), x = input$xcoord2 - ncol(theSurface()) / 2))
    })
    
    observe({
        if (!is.null(fortified1()) && !is.null(fortified2())) {
            updateSliderInput(session, "bounds1", max = floor(max(fortified1()$y)), value = c(0, floor(max(fortified1()$y))))
            updateSliderInput(session, "bounds2", max = floor(max(fortified2()$y)), value = c(0, floor(max(fortified2()$y))))
        }
    })
    
    observeEvent(input$stage1, {
        if (!is.null(crosscut1()) && !is.null(crosscut2())) {
            
            withProgress(message = "Locating grooves...", expr = {
                groove1 <- get_grooves(crosscut1())
                groove2 <- get_grooves(crosscut2())
                
                updateSliderInput(session, "bounds1", value = groove1$groove)
                updateSliderInput(session, "bounds2", value = groove2$groove)
            })
        }
    })
    
    output$crosssection <- renderPlot({
        if (is.null(fortified1()) || is.null(fortified2())) return(NULL)
        
        fortified <- fortified1()
        fortified2 <- fortified2()
        
        myx <- unique(fortified$x)
        xval <- myx[which.min(abs(myx - input$xcoord1))]
        myx2 <- unique(fortified2$x)
        xval2 <- myx2[which.min(abs(myx2 - (input$xcoord2 - ncol(theSurface()) / 2)))]
        
        plotdat <- fortified %>%
            filter(x == xval) %>%
            select(-x) %>%
            full_join(
                fortified2 %>%
                    filter(x == xval2) %>%
                    select(-x)
                , by = c("y" = "y")) %>%
            rename(bullet1 = value.x, bullet2 = value.y) %>%
            gather(key = bullet, value = value, bullet1:bullet2)
        
        plotdat$include <- FALSE
        plotdat$include[plotdat$bullet == "bullet1"] <- (plotdat$y[plotdat$bullet == "bullet1"] >= input$bounds1[1] & plotdat$y[plotdat$bullet == "bullet1"] <= input$bounds1[2])
        plotdat$include[plotdat$bullet == "bullet2"] <- (plotdat$y[plotdat$bullet == "bullet2"] >= input$bounds2[1] & plotdat$y[plotdat$bullet == "bullet2"] <= input$bounds2[2])
        
        vline.data <- data.frame(zleft = c(input$bounds1[1], input$bounds2[1]),
                                 zright = c(input$bounds1[2], input$bounds2[2]),
                                 bullet = c("bullet1", "bullet2"))
        
        ggplot(data = plotdat, aes(x = y, y = value, alpha = include)) +
            facet_wrap(~bullet, nrow = 2) +
            geom_vline(aes(xintercept = zleft), colour = "blue", data = vline.data) +
            geom_vline(aes(xintercept = zright), colour = "blue", data = vline.data) +
            geom_line(linewidth = 1) +
            xlim(c(0, max(plotdat$y))) +
            theme_bw()
    })
    
    observeEvent(input$confirm2, {
        updateCheckboxInput(session, "stage2", value = TRUE)
    })
    
    observeEvent(input$back2, {
        updateCheckboxInput(session, "stage1", value = FALSE)
    })
    
    loess1 <- reactive({
        if (is.null(crosscut1()) || !input$stage2) return(NULL)
        
        return(fit_loess(bullet = crosscut1(), groove = list(groove = input$bounds1), span = input$span))
    })
    
    loess2 <- reactive({
        if (is.null(crosscut2()) || !input$stage2) return(NULL)
        
        return(fit_loess(bullet = crosscut2(), groove = list(groove = input$bounds2), span = input$span))
    })
    
    processed1 <- reactive({
        if (is.null(fortified1()) || !input$stage2) return(NULL)
        
        myx <- unique(fortified1()$x)
        xval <- myx[which.min(abs(myx - input$xcoord1))]
        
        processBullets(bullet = bullet1(), name = "b1", x = xval, grooves = input$bounds1)
    })
    
    processed2 <- reactive({
        if (is.null(fortified2()) || !input$stage2) return(NULL)
        
        myx <- unique(fortified2()$x)
        xval <- myx[which.min(abs(myx - (input$xcoord2  - ncol(theSurface()) / 2)))]
        
        processBullets(bullet = bullet2(), name = "b2", x = xval, grooves = input$bounds2)
    })
    
    smoothed <- reactive({
        if (is.null(processed1()) || is.null(processed2())) return(NULL)
        
        bullets_processed <- list(b1 = processed1(), b2 = processed2())
        
        result <- bullets_processed %>% bind_rows %>% bulletSmooth(span = input$span)
        result$bullet <- c(rep("b1", nrow(processed1())), rep("b2", nrow(processed2())))
        
        return(result)
    })
    
    output$loess1 <- renderPlot({
        if (is.null(loess1()) || is.null(smoothed())) return(NULL)
        
        withProgress(message = "Loading plots...", {
            p1 <- ggplot(data = filter(smoothed(), bullet == "b1"), aes(x = y, y = l30)) +
                geom_line() +
                theme_bw()
            
            grid.arrange(loess1()$fitted, p1, ncol = 2)
        })
    })
    
    output$loess2 <- renderPlot({
        if (is.null(loess2()) || is.null(smoothed())) return(NULL)
        
        withProgress(message = "Loading plots...", {
            p2 <- ggplot(data = filter(smoothed(), bullet == "b2"), aes(x = y, y = l30)) +
                geom_line() +
                theme_bw()
            
            grid.arrange(loess2()$fitted, p2, ncol = 2)
        })
    })
    
    observeEvent(input$confirm3, {
        updateCheckboxInput(session, "stage3", value = TRUE)
    })
    
    observeEvent(input$back3, {
        updateCheckboxInput(session, "stage2", value = FALSE)
    })
    
    myalign <- reactive({
        if (is.null(smoothed())) return(NULL)
        
        bulletAlign(data = smoothed())
    })
    
    observeEvent(input$stage3, {
        if (!is.null(myalign())) {
            withProgress(message = "Determining alignment...", expr = {
                updateSliderInput(session, "alignment", value = myalign()$lag)
            })
        }
    })
    
    chosenalign <- reactive({
        if (is.null(myalign())) return(NULL)
        
        alignval <- round(input$alignment / 1.5625, digits = 0) * 1.5625
        
        chosen <- myalign()
        chosen$lag <- alignval
        chosen$bullets$y[chosen$bullets$bullet == "b2"] <- chosen$bullets$y[chosen$bullets$bullet == "b2"] - min(chosen$bullets$y[chosen$bullets$bullet == "b2"]) + chosen$lag
        
        return(chosen)
    })
    
    output$alignment <- renderPlot({
        if (is.null(chosenalign())) return(NULL)
        
        mydat <- chosenalign()$bullets

        ggplot(data = mydat, aes(x = y, y = l30, colour = bullet, alpha = I(0.8))) +
            geom_line() +
            theme(legend.position = "bottom") +
            theme_bw()
    })
    
    observeEvent(input$confirm4, {
        updateCheckboxInput(session, "stage4", value = TRUE)
    })
    
    observeEvent(input$back4, {
        updateCheckboxInput(session, "stage3", value = FALSE)
    })
    
    peaks1 <- reactive({
        if (is.null(chosenalign()) || !input$stage4) return(NULL)
        
        bAlign <- chosenalign()
        lofX <- bAlign$bullet
        
        return(get_peaks(subset(lofX, bullet == "b1"), smoothfactor = input$smoothfactor))
    })
    
    peaks2 <- reactive({
        if (is.null(chosenalign()) || !input$stage4) return(NULL)
        
        bAlign <- chosenalign()
        lofX <- bAlign$bullet
        
        return(get_peaks(subset(lofX, bullet == "b2"), smoothfactor = input$smoothfactor))
    })
    
    output$peaks1 <- renderPlot({
        if (is.null(peaks1())) return(NULL)
        
        return(peaks1()$plot)
    })
    
    output$peaks2 <- renderPlot({
        if (is.null(peaks2())) return(NULL)
        
        return(peaks2()$plot)
    })
    
    CMS <- reactive({
        if (is.null(peaks1()) || is.null(peaks2())) return(NULL)
        
        bAlign <- chosenalign()
        lofX <- bAlign$bullet
        
        peaks1 <- peaks1()
        peaks2 <- peaks2()
        
        peaks1$lines$bullet <- "b1"
        peaks2$lines$bullet <- "b2"
        
        lines <- striation_identify(peaks1$lines, peaks2$lines)
        maxCMS <- maxCMS(lines$match == TRUE)
        list(maxCMS = maxCMS, ccf = bAlign$ccf, lag = bAlign$lag, 
             lines = lines, bullets = lofX)
    })
    
    features <- reactive({
        if (is.null(CMS())) return(NULL)
        
        res <- CMS()
        
        lofX <- res$bullets
        aligned <- chosenalign()
        b12 <- unique(lofX$bullet)
        
        subLOFx1 <- subset(aligned$bullets, bullet==b12[1])
        subLOFx2 <- subset(aligned$bullets, bullet==b12[2])
        
        ys <- intersect(subLOFx1$y, subLOFx2$y)
        idx1 <- which(subLOFx1$y %in% ys)
        idx2 <- which(subLOFx2$y %in% ys)
        distr.dist <- mean((subLOFx1$val[idx1] - subLOFx2$val[idx2])^2, na.rm=TRUE)
        distr.sd <- sd(subLOFx1$val, na.rm=TRUE) + sd(subLOFx2$val, na.rm=TRUE)
        km <- which(res$lines$match)
        knm <- which(!res$lines$match)
        if (length(km) == 0) km <- c(length(knm)+1,0)
        if (length(knm) == 0) knm <- c(length(km)+1,0)
        # browser()
        # feature extraction
        
        signature.length <- min(nrow(subLOFx1), nrow(subLOFx2))
        
        data.frame(ccf=res$ccf, lag=res$lag,
                   D=distr.dist,
                   sd.D = distr.sd,
                   b1=b12[1], b2=b12[2], x1 = subLOFx1$x[1], x2 = subLOFx2$x[1],
                   #num.matches = sum(res$lines$match),
                   signature.length = signature.length,
                   matches.per.y = sum(res$lines$match) / signature.length,
                   #num.mismatches = sum(!res$lines$match),
                   mismatches.per.y = sum(!res$lines$match) / signature.length,
                   #cms = res$maxCMS,
                   cms.per.y = res$maxCMS / signature.length,
                   #cms2 = bulletr::maxCMS(subset(res$lines, type==1 | is.na(type))$match),
                   cms2.per.y = bulletr::maxCMS(subset(res$lines, type==1 | is.na(type))$match) / signature.length,
                   #non_cms = bulletr::maxCMS(!res$lines$match),
                   non_cms.per.y = bulletr::maxCMS(!res$lines$match) / signature.length,
                   #left_cms = max(knm[1] - km[1], 0),
                   left_cms.per.y = max(knm[1] - km[1], 0) / signature.length,
                   #right_cms = max(km[length(km)] - knm[length(knm)],0),
                   right_cms.per.y = max(km[length(km)] - knm[length(knm)],0) / signature.length,
                   #left_noncms = max(km[1] - knm[1], 0),
                   left_noncms.per.y = max(km[1] - knm[1], 0) / signature.length,
                   #right_noncms = max(knm[length(knm)]-km[length(km)],0),
                   right_noncms.per.y = max(knm[length(knm)]-km[length(km)],0) / signature.length,
                   #sumpeaks = sum(abs(res$lines$heights[res$lines$match])),
                   sumpeaks.per.y = sum(abs(res$lines$heights[res$lines$match])) / signature.length
        )
    })
    
    output$features <- DT::renderDataTable({
        if (is.null(features())) return(NULL)
        
        result <- as.data.frame(t(features()))
        result <- cbind(Feature = rownames(result), result)
        names(result)[2] <- "Value"
        
        clean_result <- result %>%
            filter(Feature %in% c("ccf", "D", "signature.length", "matches.per.y",
                                  "mismatches.per.y", "cms.per.y", "non_cms.per.y",
                                  "sumpeaks.per.y")) %>%
            mutate(Feature = c("CCF", "D", "Signature Length in Millimeters", "Matches Per Millimeter",
                               "Mismatches Per Millimeter", "CMS Per Millimeter",
                               "Non-CMS Per Millimeter", "Peak Sum Per Millimeter"),
                   Value = c(as.numeric(as.character(Value[1:2])), as.numeric(as.character(Value[3])) / 1000 * 1.5625, as.numeric(as.character(Value[4:8])) / 1.5625 * 1000))
        
        clean_result$Value <- sprintf("%.4f", clean_result$Value)
        
        return(as_tibble(clean_result))
    })
    
    observeEvent(input$confirm5, {
        updateCheckboxInput(session, "stage5", value = TRUE)
    })
    
    observeEvent(input$back5, {
        updateCheckboxInput(session, "stage4", value = FALSE)
    })
    
    observeEvent(input$confirm6, {
        updateCheckboxInput(session, "stage6", value = TRUE)
    })
    
    observeEvent(input$back6, {
        updateCheckboxInput(session, "stage5", value = FALSE)
    })
    
    output$rfpred <- renderText({
        if (is.null(features())) return(NULL)
        
        features <- features()
        features$b1 <- gsub(".x3p", "", basename(as.character(features$b1)))
        features$b2 <- gsub(".x3p", "", basename(as.character(features$b2)))
        features$span <- span
        
        includes <- setdiff(names(features), c("b1", "b2", "data", "resID", "id.x", "id.y", "pred", "span", "forest"))
        
        load("data/rf.RData")
        
        matchprob <- sprintf("%.4f", predict(rtrees, newdata = features[,includes], type = "prob")[,2])
        if (matchprob == "0.0000") matchprob <- "< .0001" else if (matchprob == "1.0000") matchprob <- "> .9999"
        
        return(HTML(paste0("The same source similarity value is<br>", h1(matchprob))))
    })
    
    observeEvent(input$restart, {
        session$reload()
    })
    
}

shinyApp(ui = ui, server = server)
