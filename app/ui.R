## Load Libraries
library(shiny)
library(shinyjs)
library(shinyBS)
library(ggplot2)
library(bslib)
library(bsicons)
library(shinycssloaders)
library(shinyscreenshot)
library(sessioninfo)

## Load Bullet Libraries
options(rgl.useNULL = TRUE)
library(rgl)

version <- "0.5.0-beta.1"
## Config
options(shiny.maxRequestSize = 30*1024^2)
addResourcePath("images", "images")

source("R/report-module.R")

ui <- shinyUI({
  fluidPage(title = "BulletAnalyzr",
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
  tags$div(id="app-container",
    fluidRow(
            column(width = 4,tags$a(target = "_blank", href="https://forensicstats.org", tags$img(src = "images/BulletAnalzr-Mark-2.png", width="500px"))),
            column(width = 4,br()),
            column(width = 4,tags$a(target = "_blank", href="https://forensicstats.org", tags$img(src = "images/BulletAnalyzr-Design-2.png", width="500px")),align="right"),
        ),
    tags$div(id="main-content",
      # navbarPage(title = div(div(id = "img-id",img(src = "csafe_tools_blue_h.png", alt="Logo", height = "40px"))),
        navbarPage(NULL,
            tabPanel("Home",
              source("R/ui_inner.R", local = TRUE)$value,
            ),
            tabPanel( 
                      "About",
                      h3("BULLETANALYZR"),
                      h4(paste0("v", version)),
                     # h4(HTML("CSAFE Tools is a software suite of state-of-the-art statistical libraries designed to assist practitioners in analyzing forensic data. This work was developed in collaboration with the Center for Statistics and Applications in Forensic Evidence (CSAFE) at Iowa State University and Omni Analytics Group. These procedures are fully open-source and transparent. For more details on the underlying code, please see the <a href='https://github.com/OAITI/bulletmatcher' target='_blank'>GitHub repository</a> for the companion R package.")),
                     # br(), br(),
                     # h4(HTML("This software is an implementation of a front-end to the <a href='https://github.com/heike/bulletxtrctr' target='_blank'>bulletxtrctr package</a>.")),
                     # h4(HTML("This application will walk through the steps used to programmatically determine the probability that two bullets were fired from the same gun. During discharge, as a bullet travels out of the chamber, it is imprinted with a groove signature that is unique to that gun’s barrel. The grooved pattern of a gun’s barrel is so distinct that the striations that are imprinted on a set of fired bullet need only be matched across a small region for there to be statistical confidence of a match; therefore probabilistic comparisons can be made at the bullet land level which represent only one-sixth of a bullet.<br><br>
                     #           Hare, E., Hofmann, H., and Carriquiry, A., Algorithmic Approaches to Match Degraded Land Impressions. Law, Probability and Risk, mgx018, <a href='https://doi.org/10.1093/lpr/mgx018' target='_blank'>https://doi.org/10.1093/lpr/mgx018</a><br>
                     #           Hare, E., Hofmann, H., and Carriquiry, A., Automatic Matching of Bullet Land Impressions. Annals of Applied Statistics. doi: 10.1214/17-AOAS1080"
                     # )),
HTML('<p>CSAFE Tools is a software suite of state-of-the-art statistical libraries designed to assist practitioners in analyzing forensic data. This work was developed in collaboration with the Center for Statistics and Applications in Forensic Evidence (CSAFE) at Iowa State University and Omni Analytics Group. These procedures are fully open-source and transparent. For more details on the underlying code, please see the <a href="https://github.com/CSAFE-isu/bulletAnalyzr">GitHub repository</a> for the companion R package.</p>
<p>This software is an implementation of a front-end to the <a href="https://github.com/heike/bulletxtrctr">bulletxtrctr R package</a>.</p>
<p>This application will walk through the steps used to programmatically determine the probability that two bullets were fired from the same gun. During discharge, as a bullet travels out of the chamber, small micro-imperfections in the barrel leave striation marks on the surface of the bullet. These striation marks asre the basis for a comparative analysis: bullets that are fired through the same barrel share marks that make them generally more similar than bullets that are fired from different barrels.</p>
<section id="references" class="level4">
<h4 class="anchored" data-anchor-id="references">References</h4>
<p>Eric Hare, Heike Hofmann, Alicia Carriquiry. <strong>Algorithmic approaches to match degraded land impressions</strong>, <em>Law, Probability and Risk</em>, Volume 16, Issue 4, December 2017, Pages 203–221, https://doi.org/10.1093/lpr/mgx018</p>
<p>Eric Hare, Heike Hofmann and Alicia Carriquiry. <strong>Automatic Matching of Bullet Land Impressions</strong>, <em>The Annals of Applied Statistics</em>, Volume 11, Number 4, 2017, pp.&nbsp;2332–56. JSTOR, http://www.jstor.org/stable/26362188.</p>
</section>
<section id="technical-session-info" class="level4">
<h4 class="anchored" data-anchor-id="technical-session-info">Technical Session Info</h4>'),
verbatimTextOutput(outputId="sessionInfo"),
hr()
            ),
            tabPanel("Instructions",),
            tabPanel("Contact",),
  ))),

  # Footer
  tags$div(id="global-footer",
    fluidRow(
      column(width = 4,tags$img(src="csafe_tools_blue_h.png", alt="Logo", height = "40px")),
      column(width = 4,tags$p("195 Durham Center, 613 Morrill Road, Ames, Iowa, 50011")),
      column(width = 4,tags$p("(C) 2023-2025 | All Rights Reserved", class="right-float"))
    )
  ))  
})
