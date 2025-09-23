sidebarLayout(tags$div(id="my-sidebar",
  sidebarPanel(width=3,
    fluidPage(

      ## Welcome Page
      conditionalPanel(condition="input.prevreport == 'Welcome'",
          div(id = "autonomous",
                tags$h1(class = "responsive-text","GET STARTED"),
                br(),
                helpText("Press the following button to start using the app by uploading the bullet data."),
                br(),
                actionButton("confirm_autonomous", "Begin")#, icon = icon("check"))
            ),
      ),

      ## Bullet Select and manipulate Input 
      conditionalPanel(condition="input.prevreport == 'Upload Bullet'",
          fluidRow(column(12,uiOutput("bul_x3pui"))),
          hr()
      ),
      conditionalPanel(condition="input.prevreport == 'Preview Bullet'",
          uiOutput("prevSelUI"),
      ),
      conditionalPanel(condition="input.prevreport == 'Comparison Report'",
          uiOutput("CCBull1"),
          uiOutput("CCBull2"),
          uiOutput("reportSelUI"),
      ),

      ## Bullet Add to Comparison UI
      conditionalPanel(condition="input.prevreport == 'Upload Bullet'",
          fluidRow(
            column(12,textInput("bul_x3p_name", label="Bullet Name",value="",placeholder="Bullet Name Here ...")),
            column(12,actionButton("up_bull", label = "Add Bullet to Comparison List"),align="center")
          ),
          hr(),
      ),

      ## Bullet Comparison UI
      conditionalPanel(condition="input.prevreport == 'Upload Bullet'",
          fluidRow(
            column(12,uiOutput("bull_sel")),
            column(12,actionButton("doprocess", label = "Compare Bullets"),align="center"),
          ),
          div(id = "orientation-note",
              br(),
              helpText("Note that for valid comparisons results, scans have to be oriented with the tip of the bullet pointing left."),
              br()
          )
      ),
    
      ## Download Report Button
      conditionalPanel(condition="input.prevreport == 'Comparison Report'",
                       reportSidebarUI("report1"),
      ),
  ))),
  mainPanel(
    tabsetPanel(id="prevreport",

      ## Welcome
      tabPanel("Welcome",
                h3("WELCOME TO BULLETANALYZR!"),
                p("Our innovation combines 3D imagery and sophisticated algorithms to revolutionize bullet analysis. This prototype demonstrates how our methods can calculate the likelihood of the observed similarity if two bullets originated from the same firearm versus different firearms. It's a work in progress, evolving through feedback from diverse communities."),
      ),

      ## Upload Bullet RGL Windows
      tabPanel("Upload Bullet",uiOutput("lpupload")),

      ## Upload Bullet RGL Windows
      tabPanel("Preview Bullet",uiOutput("lpreview")),

      ## Comparison Report
      tabPanel("Comparison Report", 
          shinycssloaders::withSpinner(uiOutput("CCBullLand")),
          shinycssloaders::withSpinner(reportMainUI("report1"))
      )  
    )
  )
)

