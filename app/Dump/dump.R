library(shiny)
library(bslib)
options(rgl.useNULL = TRUE)
options(shiny.port = 4569)
options(shiny.host= "0.0.0.0")
library(rgl)
library(x3ptools)
library(bulletxtrctr)
library(shinyscreenshot)

ui <- bootstrapPage(
	screenshotButton(label = "Download Report", id = "allplot",filename="Bullet Comparison Report",scale=1),
	uiOutput("allplot")
		
)

server <- function(input, output)
{

	bull <- read_bullet("~/Desktop/csafe/bullet/bulletanalyzer/dump/bull")
	# bull <- read_bullet("~/Desktop/bull")
	bull$x3p <- lapply(bull$x3p,x3p_m_to_mum)
	bull$x3p <- lapply(bull$x3p,function(x) y_flip_x3p(rotate_x3p(x,angle = -90)))
	x3p_scaled <- x3p_interpolate(bull$x3p[[1]],resx=20)
	output$plot1 <- renderRglwidget({image_x3p(x3p_scaled,zoom=1);rglwidget()})
	output$plot2 <- renderRglwidget({image_x3p(x3p_scaled,zoom=1);rglwidget()})
	output$plot3 <- renderRglwidget({image_x3p(x3p_scaled,zoom=1);rglwidget()})
	output$plot4 <- renderRglwidget({image_x3p(x3p_scaled,zoom=1);rglwidget()})
	output$plot5 <- renderRglwidget({image_x3p(x3p_scaled,zoom=1);rglwidget()})
	output$plot6 <- renderRglwidget({image_x3p(x3p_scaled,zoom=1);rglwidget()})
	output$plot7 <- renderRglwidget({image_x3p(x3p_scaled,zoom=1);rglwidget()})
	output$plot8 <- renderRglwidget({image_x3p(x3p_scaled,zoom=1);rglwidget()})
	output$plot9 <- renderRglwidget({image_x3p(x3p_scaled,zoom=1);rglwidget()})
	output$plot10 <- renderRglwidget({image_x3p(x3p_scaled,zoom=1);rglwidget()})
	output$plot11 <- renderRglwidget({image_x3p(x3p_scaled,zoom=1);rglwidget()})
	output$plot12 <- renderRglwidget({image_x3p(x3p_scaled,zoom=1);rglwidget()})
	output$plot13 <- renderRglwidget({image_x3p(x3p_scaled,zoom=1);rglwidget()})
	output$plot14 <- renderRglwidget({image_x3p(x3p_scaled,zoom=1);rglwidget()})
	output$plot15 <- renderRglwidget({image_x3p(x3p_scaled,zoom=1);rglwidget()})
	output$plot16 <- renderRglwidget({image_x3p(x3p_scaled,zoom=1);rglwidget()})
	output$plot17 <- renderRglwidget({image_x3p(x3p_scaled,zoom=1);rglwidget()})
	output$plot18 <- renderRglwidget({image_x3p(x3p_scaled,zoom=1);rglwidget()})
	output$plot19 <- renderRglwidget({image_x3p(x3p_scaled,zoom=1);rglwidget()})
	output$plot20 <- renderRglwidget({image_x3p(x3p_scaled,zoom=1);rglwidget()})

	output$allplot <- renderUI({layout_column_wrap(
										width = 1/5,
										rglwidgetOutput('plot1'),
										rglwidgetOutput('plot2'),
										rglwidgetOutput('plot3'),
										rglwidgetOutput('plot4'),
										rglwidgetOutput('plot5'),
										rglwidgetOutput('plot6'),
										rglwidgetOutput('plot7'),
										rglwidgetOutput('plot8'),
										rglwidgetOutput('plot9'),
										rglwidgetOutput('plot10'),
										rglwidgetOutput('plot11'),
										rglwidgetOutput('plot12'),
										rglwidgetOutput('plot13'),
										rglwidgetOutput('plot14'),
										rglwidgetOutput('plot15'),
										rglwidgetOutput('plot16'),
										rglwidgetOutput('plot17'),
										rglwidgetOutput('plot18'),
										rglwidgetOutput('plot19'),
										rglwidgetOutput('plot20')
	)})
}


shinyApp(ui = ui, server = server)
