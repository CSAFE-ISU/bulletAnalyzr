## Load Libraries
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

## Load Bullet Libraries
options(rgl.useNULL = TRUE)
library(rgl)
library(x3ptools) # remotes::install_github("heike/x3ptools")
library(bulletxtrctr) # remotes::install_github("heike/bulletxtrctr")

## Force use of chromote
library(pagedown)
library(curl) # for webshot

## Config
options(shiny.maxRequestSize = 30*1024^2)
addResourcePath("images", "images")
theme_set(theme_bw())
theme_update(
  text = element_text(size = 22), 
  plot.title = element_text(size=22,face="bold")
)
interactive_cc = TRUE

#################################################################################
## Helper Functions
source("helper.R")
#################################################################################
## Render RGL Widget UI
parse_rglui <- function(x, name = "x3prgl", land_name = NULL)
{
  if (is.null(land_name)) land_name <- x
	card(
		card_header(class = "bg-dark",paste0("Land ", land_name)),
		max_height = 600,
		full_screen = FALSE,
		rglwidgetOutput(paste0(name,x),height=600,width=200),
	)
}

## Render Land into image with CrossCut line
render_land <- function(src,x3p,ccut)
{
	imgsrc <- gsub(".x3p$",".png",src)
	x3p %>%
	  x3p_add_hline(yintercept = ccut, size = 20, color = "#eeeeee") %>%
	  x3p_sample(m=5) %>%
	  x3p_image(size = 600, zoom=.25)
	snapshot3d(imgsrc,webshot=TRUE)
	return(imgsrc)
}

## Render Slider to adjust CrossCut
render_ccsl <- function(id, ymin,ymax,yset)
{
	sliderInput(inputId = paste("CCsl",id), label = NULL, min = ymin, max = ymax, value = yset)
}
#################################################################################
#################################################################################


server <- function(input, output, session) {
	#################################################################################
	## Bullet Data Upload and Storage 
	#################################################################################
	observeEvent(input$confirm_autonomous,{updateTabsetPanel(session, "prevreport", selected = "Upload Bullet")})

	## Reactive object to hold the bullet and comparison data
	bulldata <- reactiveValues(allbull=data.frame(),cbull=data.frame(),preCC = NULL, postCC = NULL, comparison=NULL)

	## Bullet Land Files Input
	output$bul_x3pui <- renderUI({fileInput("bul_x3p", "Select Bullet Land x3p files", accept = ".x3p",multiple=TRUE)})
	
	observeEvent(input$bul_x3p, {
	   # bullet_name <- sub("^(.*)\\s+[^\\s]+$", "\\1", input$bul_x3p[1]$name)[1]
	    bullet_name <- identify_bullet(input$bul_x3p$name)
	    updateTextInput(session, "bul_x3p_name", value = bullet_name)
	})

	## Push current bullet data to all bullet data object
	observeEvent(input$up_bull,{
								if(nrow(bulldata$cbull)==0) return(NULL)
								allbull <- bulldata$allbull
								allbull <- allbull[!(allbull$bullet %in% input$bul_x3p_name),]
								bull <- bulldata$cbull
								bull$bullet <- input$bul_x3p_name
								#bull$land <- 1:nrow(bull)
								bull$land <- factor(bull$land_names, levels = bull$land_names)
								bulldata$allbull <- rbind(allbull,bull)
								disable("up_bull")
				})
	#################################################################################
	#################################################################################


  #################################################################################
	## Preview Bullets while Uploading Bullet lands
	#################################################################################
	output$lpupload <- renderUI({
									if(is.null(input$bul_x3p)) return(NULL)
									disable("up_bull")
									progress <- shiny::Progress$new();on.exit(progress$close())

									## Refresh on Tab Change
									temp_refresh <- input$prevreport

									# Create Temporary Directory and save bullets in it
									temp_dir <- tempfile()
									dir.create(temp_dir)
									file.copy(input$bul_x3p$datapath, paste0(temp_dir, "/", input$bul_x3p$name))
									## Read Bullet
									progress$set(message = "Reading Bullets", value = .25)
									bull <- read_bullet(temp_dir)
									
									# Check if we need to rotate the bullet
									hinfo <- b1$x3p[[1]]$header.info
									if (hinfo$sizeX > hinfo$sizeY) {
									    alert("Detected rotated bullet, rotating 90 degrees...")
									    bull$x3p <- lapply(bull$x3p, x3p_rotate, angle = -90)
									}
									
									cond_x3p_m_to_mum <- function(x3p)
									{
									  scale <- x3p %>% x3p_get_scale()
									  if (scale < .1) x3p <-  x3p %>% x3p_m_to_mum() # only scale conditionally
									  x3p
									}
									bull$x3p <- lapply(bull$x3p,cond_x3p_m_to_mum)
									bull$md5sum <- tools::md5sum(bull$source)
									bull$filename <- basename(bull$source)
									bull$land_names <- identify_lands(bull$filename)
									bull$bullet_name <- identify_bullet(bull$filename)
									bulldata$cbull <- bull
									
									## Render Bullet
									progress$set(message = "Rendering Previews", value = .75)
									for(idx in 1:nrow(bull)) 
									{
										local({
												cidx <- idx
												output[[paste0("x3prgl",idx)]] <- renderRglwidget({
																									x3p_image(x3p_sample(bull$x3p[[cidx]],m=5) %>% x3p_rotate(),size=500,zoom=.4)
																									rglwidget()
																					})
											})
									}

									## Enable Upload Button
									enable("up_bull")

									## UI
									layout_column_wrap(
										width = 1/6,
										!!!lapply(1:nrow(bull), FUN= function(x) parse_rglui(x, name = "x3prgl", land_name = bull$land_names[x]))
									)
						})
	#################################################################################
	#################################################################################
	

  #################################################################################
	## Preview Bullet Selection
	#################################################################################
	output$prevSelUI <- renderUI({
  									if(nrow(bulldata$allbull)==0) return(NULL)
  									allbull <- bulldata$allbull
  									selectInput("prev_bul","Preview Bullet",choices=unique(allbull$bullet),selected=NULL,multiple = FALSE)
  						})
	output$lpreview <- renderUI({
									if(nrow(bulldata$allbull)==0) return(NULL)
									if(length(input$prev_bul)==0) return(NULL)
									progress <- shiny::Progress$new();on.exit(progress$close())

									## Refresh on Tab Change
									temp_refresh <- input$prevreport

									## Render Bullet
									allbull <- bulldata$allbull
									bull <- allbull[allbull$bullet==input$prev_bul,]

									progress$set(message = "Rendering Previews", value = .75)
									for(idx in 1:nrow(bull)) 
									{
										local({
												cidx <- idx
												output[[paste0("x3prglprev",idx)]] <- renderRglwidget({
																									x3p_image(x3p_sample(bull$x3p[[cidx]],m=5) %>% x3p_rotate(),size=500, zoom=.4)
																									rglwidget()
																					})
											})
									}

									## UI
									layout_column_wrap(
										width = 1/6, 
										!!!lapply(1:nrow(bull), FUN = function(x) parse_rglui(x, name = "x3prglprev", land_name = bull$land_names[x]))
									)
						})
	#################################################################################
	#################################################################################


	#################################################################################
	## Compare Bullet Selection and processing
  #################################################################################
   	output$bull_sel <- renderUI({
  									if(nrow(bulldata$allbull)==0) return(NULL)
  									allbull <- bulldata$allbull
  									checkboxGroupInput(
  										"bullcompgroup",
  										label = "Select Bullets to Compare", 
    									choices = unique(bulldata$allbull$bullet),
    									selected = unique(bulldata$allbull$bullet)
    								)
  						})

   	## Start Process before Interactivity
  	observeEvent(input$doprocess,{
								if(length(input$bullcompgroup)==0) return(NULL)
								progress <- shiny::Progress$new();on.exit(progress$close())

								## Fetch All Bullets
								bullets <- bulldata$allbull

								## Get the ideal Cross Sections
								progress$set(message = "Get suitable Cross Sections", value = 0)
								bullets$crosscut <- sapply(bullets$x3p,x3p_crosscut_optimize, ylimits = c(150, NA))

								## If Interactive Push the data to preCC Stage
								if(interactive_cc) bulldata$preCC <- bullets
								if(!interactive_cc) bulldata$postCC <- bullets

								## Update the selected Panel
								updateTabsetPanel(session, "prevreport", selected = "Comparison Report")
		})

  	## Contimue Process after Interactivity
		observeEvent(bulldata$postCC,{
								if(is.null(bulldata$postCC)) return(NULL)
								progress <- shiny::Progress$new();on.exit(progress$close())
								
								progress$set(message = "Finalizing Cross Sections", value = 0)
								bullets <- bulldata$postCC
								try_x3p_crosscut <- function(x3p, y = NULL, range = 1e-5) 
								{
								  res <- x3p_crosscut(x3p=x3p, y = y, range = range)
								  if (nrow(res) == 0) res <- x3p_crosscut(x3p=x3p, y = NULL, range = range)
								  return(res)
								}
								bullets$ccdata <- mapply(try_x3p_crosscut,bullets$x3p,bullets$crosscut, SIMPLIFY=FALSE)
								# browser()

								## Get Resolution
								resolution <- x3p_get_scale(bullets$x3p[[1]])

								## Get the Groove Locations
								progress$set(message = "Get the Groove Locations", value = .05)
								bullets$grooves <- lapply(bullets$ccdata,function(x) cc_locate_grooves(x,method = "middle", adjust = 30, return_plot = FALSE))

								## Extracting Signal
								progress$set(message = "Extracting Signal", value = .1)
								bullets$sigs <- mapply(function(ccdata,grooves) cc_get_signature(ccdata, grooves, span1 = 0.75, span2 = 0.03) ,bullets$ccdata,bullets$grooves,SIMPLIFY=FALSE)
								bullets$bulletland <- paste0(bullets$bullet,"-", bullets$land)
								lands <- unique(bullets$bulletland)

								## Align Signal
								progress$set(message = "Align Signals", value = .15)
								comparisons <- data.frame(expand.grid(land1 = lands, land2 = lands), stringsAsFactors = FALSE)
								comparisons$aligned <- mapply(function(x,y,bullets) sig_align(bullets$sigs[bullets$bulletland == x][[1]]$sig, bullets$sigs[bullets$bulletland == y][[1]]$sig),comparisons$land1,comparisons$land2,MoreArgs=list(bullets=bullets),SIMPLIFY=FALSE)
								
								## Evaluating Features
								progress$set(message = "Evaluating Features", value = .2)
								comparisons$ccf0 <- sapply(comparisons$aligned,function(x) extract_feature_ccf(x$lands))

							  ## Evaluating Striation Marks
								progress$set(message = "Evaluating Striation Marks", value = .25)
								comparisons$striae <- lapply(comparisons$aligned,sig_cms_max,span=75)

								# ## Evaluating Features
								# progress$set(message = "Evaluating Features", value = .3)
								# comparisons$cms_per_mm <- mapply(function(x,y,resolution) extract_feature_cms_per_mm(x$lines,y$lands,resolution),comparisons$striae,comparisons$striae,MoreArgs=list(resolution=resolution),SIMPLIFY=FALSE)
								# comparisons$matches0 <- as.numeric(sapply(comparisons$striae,function(s) bulletxtrctr:::extract_helper_feature_n_striae(s$lines, type = "peak", match = TRUE)))
								# comparisons$mismatches0 <- as.numeric(sapply(comparisons$striae,function(s) bulletxtrctr:::extract_helper_feature_n_striae(s$lines, type = "peak", match = FALSE)))


								## Extracting Features
								progress$set(message = "Extracting Features", value = .35)
								comparisons$bulletA <- sapply(strsplit(as.character(comparisons$land1),"-"),"[[",1)
								comparisons$bulletB <- sapply(strsplit(as.character(comparisons$land2),"-"),"[[",1)
								comparisons$landA <- sapply(strsplit(as.character(comparisons$land1),"-"),"[[",2)
								comparisons$landB <- sapply(strsplit(as.character(comparisons$land2),"-"),"[[",2)
								comparisons$features <- mapply(extract_features_all,comparisons$aligned,comparisons$striae,MoreArgs=list(resolution=resolution),SIMPLIFY=FALSE)


								## Scaling Features
								progress$set(message = "Scaling Features", value = .4)
								features <- tidyr::unnest(comparisons[,c("land1", "land2", "ccf0", "bulletA", "bulletB", "landA", "landB", "features")], cols=features)
								features <- features %>% mutate(cms = cms_per_mm,matches = matches_per_mm,mismatches = mismatches_per_mm,non_cms = non_cms_per_mm)

								## Predicting RandomForest Scores
								progress$set(message = "Predicting RandomForest Scores", value = .45)
								features$rfscore <- predict(rtrees, newdata = features, type = "prob")[,2]

								## Preparing Data for Report
								progress$set(message = "Preparing Report Data", value = .5)
								bullet_scores <- features %>% group_by(bulletA, bulletB) %>% tidyr::nest()
#								browser()
#								debugonce(compute_average_scores)
								bullet_scores$bullet_score <- sapply(bullet_scores$data,function(d) max(compute_average_scores(land1 = d$landA, land2 = d$landB, d$rfscore, verbose=FALSE)))
								# just get the 'best phase' not just ones that are 'matches'
#								debugonce(bullet_to_land_predict)
								bullet_scores$data <- lapply(bullet_scores$data,
								                             function(d) 
								                               cbind(d,samesource=bullet_to_land_predict(land1 = d$landA, land2 = d$landB, d$rfscore, alpha = .9, difference=0.01)))
								
								
								# Rendering Bullet Images for Report
								bullets$x3pimg <- NA
								for(idx in 1:nrow(bullets))
								{
									progress$set(message = "Rendering Report Objects", value = round(seq(from=.55,to=.85,length.out=nrow(bullets)),2)[idx])
									bullets$x3pimg[idx] <- render_land(bullets$source[idx],bullets$x3p[[idx]],bullets$crosscut[idx])	
								}

								## Saving Report Data
								progress$set(message = "Preparing Report", value = .9)
								bulldata$comparison <- list(bullets=bullets,comparisons=comparisons,features_scaled=features,bullet_scores=bullet_scores)
				})
  	#################################################################################
  	#################################################################################


		#################################################################################
		## Cross Cut Interactivity
  	#################################################################################
		output$CCBull1 <- renderUI({
													if(is.null(bulldata$preCC)) return(NULL)

													## Pre Cross Cut Data
													bullets <- bulldata$preCC
													selectInput("cc_bulsel","Select Bullet",choices=unique(bullets$bullet),selected=NULL,multiple = FALSE)
												})
		output$CCBull2 <- renderUI({
													if(is.null(bulldata$preCC) | is.null(input$cc_bulsel)) return(NULL)

													## Pre Cross Cut Data
													bullets <- bulldata$preCC
													bullets <- bullets[bullets$bullet == input$cc_bulsel,]
													list(
																mapply(render_ccsl,1:nrow(bullets),0,500,bullets$crosscut,SIMPLIFY=FALSE),
																fluidRow(column(12,actionButton("saveCC", label = "Finalise CrossCut"),align="center")),
																	hr(),
																fluidRow(column(12,actionButton("doprocessCC", label = "Compare Bullets"),align="center"))
													)
												})

		observeEvent(input$saveCC,{
																		if(is.null(bulldata$preCC)) return(NULL)
																		bullets <- bulldata$preCC
																		bullets[bullets$bullet == input$cc_bulsel,]$crosscut <- sapply(1:sum(bullets$bullet == input$cc_bulsel),function(x) input[[paste("CCsl",x)]])
																		bulldata$preCC = bullets
			})

		observeEvent(input$doprocessCC,{
																		if(is.null(bulldata$preCC)) return(NULL)
																		bullets <- bulldata$preCC
																		bulldata$preCC <- NULL
																		bulldata$postCC <- bullets
			})

		output$CCBullLand <- 	renderUI({
									if(is.null(bulldata$preCC) | is.null(input$cc_bulsel)) return(NULL)
									
									## Render Bullet
									bullets <- bulldata$preCC
									bullets <- bullets[bullets$bullet == input$cc_bulsel,]

									## Refresh on Tab Change
									temp_refresh <- input$prevreport

									for(idx in 1:nrow(bullets))
									{
										local({
														cidx <- idx
														output[[paste0("CC_Sel_",idx)]] <- renderRglwidget({
																																	bullets$x3p[[cidx]] %>%
																																	x3p_add_hline(yintercept = input[[paste("CCsl",cidx)]], size = 20, color = "#eeeeee") %>%
																																	x3p_rotate %>%
																																	x3p_sample(m=5) %>%
																																	x3p_image(size = 500, zoom=.4)
																																	rglwidget()
																																})
											})
									}

									layout_column_wrap(
										width = 1/6, 
										!!!lapply(1:nrow(bullets), FUN = function(x) parse_rglui(x, name = "CC_Sel_", land_name = NULL))
									)
						})
		#################################################################################
		#################################################################################



  	#################################################################################
		## Generate Bullet Comparison Report UI
  	#################################################################################
  	## Side Panel UI
  	output$reportSelUI <- renderUI({
  										if(!is.null(bulldata$preCC)) return(NULL)
  										if(is.null(bulldata$comparison)) return(NULL)
  										all_bullets <- unique(bulldata$comparison$bullet_scores$bulletA)
  										list(
  											selectInput("comp_bul1","Compare Bullet",choices=all_bullets,selected=all_bullets[1]),
  											selectInput("comp_bul2","With Bullet",choices=all_bullets,selected=all_bullets[2]),
  											hr()
  										)
  							})

  	## Side Panel UI Download Report
  	output$reportDownUI <- renderUI({
  										if(!is.null(bulldata$preCC)) return(NULL)
  										if(is.null(bulldata$comparison)) return(NULL)
  										fluidRow(column(12,screenshotButton(label = "Download Report", id = "reportUI",filename="Bullet Comparison Report",scale=2),align="center"))
  							})

  	## Main Panel UI Bullet Comparison Report
  	output$reportUI <- renderUI({
  									if(!is.null(bulldata$preCC)) return(NULL)
  									if(is.null(bulldata$comparison)) return(NULL)
  									if(is.null(input$comp_bul1) | is.null(input$comp_bul2)) return(NULL)

  	 								## Bullet Comparison Report
  									BullComp <- list(
  									  fluidRow(
  									      column(12, "SUMMARY OF RESULTS", align="left", class="h3"), 
  									      column(12, "Phase Test", align="left", class="h3"), 
  									      column(12, textOutput("bull_comp_score"), align="left", class="h4"), 
  									      column(12, textOutput("bull_comp_test"), align="left", class="h4"),
  									    hr(),
  									  ),
  									  br(),
  									  fluidRow(
											          				column(6,plotOutput("bull_comp")),
											          				column(6,plotOutput("land_comp")),
											          				column(12, tags$p("Higher scores indicate more similarity. The thick frames indicate the selected bullet comparison (left) and the land comparisons of the best phase (right).", class="body"))
											        ),
				  										br(),br(),
											        fluidRow(column(12,plotOutput("land_visCC"),align="center")),
											        br(),br(),
											        fluidRow(column(12,plotOutput("land_visSig"),align="center")),
  									  				br()
								      )

  									## Land Comparison Collapsible Report
  									LandComp <- list()
  									bullet_scores <- bulldata$comparison$bullet_scores
  									bullet_scores <- bullet_scores[bullet_scores$bulletA==input$comp_bul1 & bullet_scores$bulletB==input$comp_bul2,]
  									# bullet_scores$data[[1]] <- bullet_scores$data[[1]][bullet_scores$data[[1]]$samesource,] # HH: only showing 'matches' is extremely biased
  									if(nrow(bullet_scores$data[[1]])>0)
  									{
  										## Collect Land wise Data
  										bsldata <- bullet_scores$data[[1]]
  										# just re-order the data - that will be safer and quicker
  										bsldata <- bsldata %>% 
  										  mutate(samesource = factor(samesource, levels = c(TRUE, FALSE))) %>%
  										  group_by(samesource) %>% arrange(desc(rfscore), .by_group = TRUE)
  								
  										odridx <- order(bsldata$rfscore,decreasing=TRUE) # this should be in order now

  										# getting scales and instrument info ... not correct yet, but just for the first scan
  										scale <- bulldata$cbull$x3p[[1]] %>% x3p_get_scale()
  										instrument <- bulldata$cbull$x3p[[1]] %>% x3p_show_xml("Manufacturer")

  										## Generate Collapsible UI Panel List in a loop
  										bsCollapsePanelList <- list()
  										
  										show_n <- min(c(sum(bsldata$samesource==TRUE) + 3, length(odridx)))
  										# show all the best-phase comparisons and the three top comparisons
  										for(idx in 1:show_n)
  										{
  											#########################################################################################################
  											## Data Table Comparison
  											#########################################################################################################
  											BullCompBulls <- bulldata$comparison$bullets
  											temptable <- data.frame(
  																		Feature = c("Left Land File","Left Land MD5", "Left Land Instrument (resolution [µm/px])", 
  																		            "Right Land File","Right Land MD5","Left Land Instrument (resolution [µm/px])", 
  																		            "Cross Correlation Function","Mean Distance btw Signals [µm]",
  																		            "Signal Length [mm]","# Matching Striae Per Millimeter",
  																		            "# Mis-Matching Striae Per Millimeter","CMS Per Millimeter",
  																		            "Non-CMS Per Millimeter","Peak Sum"),
  																		Value = c(
  																					BullCompBulls$filename[BullCompBulls$bullet==input$comp_bul1 & BullCompBulls$land == bsldata$landA[odridx[idx]]],
  																					BullCompBulls$md5sum[BullCompBulls$bullet==input$comp_bul1 & BullCompBulls$land == bsldata$landA[odridx[idx]]],
  																					sprintf("%s (%s)", instrument, scale),
  																					BullCompBulls$filename[BullCompBulls$bullet==input$comp_bul2 & BullCompBulls$land == bsldata$landB[odridx[idx]]],
  																					BullCompBulls$md5sum[BullCompBulls$bullet==input$comp_bul2 & BullCompBulls$land == bsldata$landB[odridx[idx]]],
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
  											temptable_dt <- datatable(temptable,rownames=FALSE,options = list(paging = FALSE,ordering=FALSE,searching=FALSE,bLengthChange = FALSE,bInfo = FALSE))
  											#########################################################################################################
  											#########################################################################################################

  											#########################################################################################################
  											## RGL Render Comparison
  											#########################################################################################################
  											local({
  												cidx <- idx
	  											BullCompBulls <- bulldata$comparison$bullets
	  											rglLidx <- which(BullCompBulls$bullet==input$comp_bul1 & BullCompBulls$land == bsldata$landA[odridx[cidx]])
	  											rglRidx <- which(BullCompBulls$bullet==input$comp_bul2 & BullCompBulls$land == bsldata$landB[odridx[cidx]])
	  											rglL <- BullCompBulls$x3pimg[[rglLidx]]
	  											rglR <- BullCompBulls$x3pimg[[rglRidx]]
	  											output[[paste0("rglWinL",idx)]] = renderImage({list(src = rglL, contentType = 'image/png')}, deleteFile = FALSE)
	  											output[[paste0("rglWinR",idx)]] = renderImage({list(src = rglR, contentType = 'image/png')}, deleteFile = FALSE)
  											})
  											temp_rgl <- layout_column_wrap(
  											  width = 1/2,
  											  imageOutput(paste0("rglWinL",idx)),
  											  imageOutput(paste0("rglWinR",idx)),
  											  height = "250px"
  											)
  											#########################################################################################################
  											#########################################################################################################


  											#########################################################################################################
  											## Groove Plot
  											#########################################################################################################

  											local({
  												cidx <- idx
	  											BullCompBulls <- bulldata$comparison$bullets
	  											GroovePlotLidx <- which(BullCompBulls$bullet==input$comp_bul1 & BullCompBulls$land == bsldata$landA[odridx[idx]])
	  											GroovePlotRidx <- which(BullCompBulls$bullet==input$comp_bul2 & BullCompBulls$land == bsldata$landB[odridx[idx]])
	  											GroovesL <- as.numeric(BullCompBulls$grooves[[GroovePlotLidx]]$groove)
	  											GroovesR <- as.numeric(BullCompBulls$grooves[[GroovePlotRidx]]$groove)
	  											CCDataL <- BullCompBulls$ccdata[[GroovePlotLidx]] - GroovesL[1]
	  											CCDataR <- BullCompBulls$ccdata[[GroovePlotRidx]] - GroovesR[1]
	  											output[[paste0("GroovePlotL",idx)]] = renderPlot({
																																		groove_plot(CCDataL, GroovesL) +
																																		ggtitle(sprintf("Land %s profile",bsldata$land1[odridx[cidx]]))
	  																														})
	  											output[[paste0("GroovePlotR",idx)]] = renderPlot({
																																		groove_plot(CCDataR, GroovesR) +
																																		ggtitle(sprintf("Land %s profile",bsldata$land2[odridx[cidx]]))
	  																														})
  											})
  											temp_groove <- list(
  																			fluidRow(
			  																		column(6,plotOutput(paste0("GroovePlotL",idx)),align="center"),
			  																		column(6,plotOutput(paste0("GroovePlotR",idx)),align="center")
			  																),
  																			fluidRow(column(12,p("Shaded areas are excluded from the analysis"),align="center"))
  																		)
  											#########################################################################################################
  											#########################################################################################################


												#########################################################################################################
  											## Signal Comparison
  											#########################################################################################################
  											local({
  												cidx <- idx
	  											BullCompComps <- bulldata$comparison$comparisons
	  											scale <- bulldata$cbull$x3p[[1]] %>% x3p_get_scale()
	  											SigPlotData <- BullCompComps$aligned[
	  																					(BullCompComps$bulletA == input$comp_bul1)&
	  																					(BullCompComps$bulletB == input$comp_bul2)&
	  																					(BullCompComps$landA == bsldata$landA[odridx[idx]])&
	  																					(BullCompComps$landB == bsldata$landB[odridx[idx]])
	  																				][[1]]$lands
	  											SigPlotData <- tidyr::gather(SigPlotData,Signal, value, sig1, sig2)
	  											
	  											SigPlotData$Signal[SigPlotData$Signal=="sig1"] <- "Left LEA"
	  											SigPlotData$Signal[SigPlotData$Signal=="sig2"] <- "Right LEA"
													output[[paste0("SigPlot",idx)]] = renderPlot({
																																ggplot(SigPlotData,aes(x = x*scale, y = value, colour = Signal, linetype=Signal)) + 
																							    							geom_line(na.rm=TRUE, alpha = 0.9, linewidth = 1) +
												    																		scale_color_manual(values = c("darkorange", "purple4")) + 
																														  	xlab("Position along width of Land [µm]") +
																														  	ylab("Signal [µm]") +
																														  	ggtitle("Aligned signals of LEAs")+
																														  	theme(legend.position = "bottom") 
																														})
  											})
  											temp_signal <- fluidRow(column(12,plotOutput(paste0("SigPlot",idx)),align="center"))
  											#########################################################################################################
  											#########################################################################################################

  											## Combine Results
  											panel_name <- paste0(bsldata$land1[odridx[idx]]," vs ",bsldata$land2[odridx[idx]]," (RF Score = ",round(bsldata$rfscore[odridx[idx]],4),")")
  											bsCollapsePanelList[[idx]] <- bsCollapsePanel(panel_name, temptable_dt, br(),temp_rgl, temp_groove, br(), temp_signal, style = "primary")
  										}

  										## Generate Collapsible UI Panel
  										LandComp <- do.call(bsCollapse,args=c(id = "collapseExample",multiple=TRUE,bsCollapsePanelList))
  									}

  									## If no Land Match
  									# if(nrow(bullet_scores$data[[1]])==0) LandComp$children <- list(fluidRow(column(12,h3("No Land Matches in this Bullet Pair."),align="center")),br())

  									## Return Full Collapsible Report
  									return(c(BullComp, LandComp$children))
  						})
  	#################################################################################
  	#################################################################################


  	#################################################################################
		## Generate Bullet Comparison Report Server Outputs
  	#################################################################################
  	## Bullet Comarison Score
   	output$bull_comp_score <- renderText({
																if(is.null(bulldata$comparison)) return(NULL)
																bullet_scores <- bulldata$comparison$bullet_scores
																bullet_scores$selsource <- FALSE
																bullet_scores$selsource[bullet_scores$bulletA==input$comp_bul1 & bullet_scores$bulletB==input$comp_bul2] <- TRUE
																d <- bullet_scores %>% filter(selsource) %>% tidyr::unnest(data)
																res <- try(test <- bulletxtrctr:::phase_test(land1 = d$landA, land2 = d$landB, d$ccf), silent = TRUE)
																if (any(class(res) %in% "try-error")) 
																{
																		scores <- d %>%
																							group_by(samesource) %>% 
																							summarize(avg = mean(scores, na.rm=TRUE)) %>%
																							purrr::pluck("avg") %>%
																							unlist()
																		return(sprintf("Phase Test Score: %.4f", abs(diff(scores))))
																}
																return(sprintf("Phase Test Score: %.4f", test$estimate))
															})


   	## Bullet Comarison Mismatch Prob
   	output$bull_comp_test <- renderText({
																if(is.null(bulldata$comparison)) return(NULL)
																bullet_scores <- bulldata$comparison$bullet_scores
																bullet_scores$selsource <- FALSE
																bullet_scores$selsource[bullet_scores$bulletA==input$comp_bul1 & bullet_scores$bulletB==input$comp_bul2] <- TRUE
																d <- bullet_scores %>% filter(selsource) %>% tidyr::unnest(data)
																res <- try(test <- bulletxtrctr:::phase_test(land1 = d$landA, land2 = d$landB, d$ccf), silent = TRUE)
																if(any(class(res) %in% "try-error")) return("Test result unstable")
																pval <- sprintf("%.4f", test$p.value)
																if (test$p.value < 0.01) pval <- "Less than 1 in 100" 
																if (test$p.value < 0.001) pval <- "Less than 1 in 1,000" 
																if (test$p.value < 0.0001) pval <- "Less than 1 in 10,000" 
																if (test$p.value < 0.00001) pval <- "Less than 1 in 100,000" 
																if (test$p.value < 0.000001) pval <- "Less than 1 in 1 Million" 
																if (test$p.value < 0.0000001) pval <- "Less than 1 in 10 Million" 
																return(sprintf("Probability of False Identification: %s (Type I Error)", pval))
															})
  	

  	## Bullet Comparison Heatmap
  	output$bull_comp <- renderPlot({
  									if(is.null(bulldata$comparison)) return(NULL)
  									bullet_scores <- bulldata$comparison$bullet_scores
  									bullet_scores$selsource <- FALSE
  									bullet_scores$selsource[bullet_scores$bulletA==input$comp_bul1 & bullet_scores$bulletB==input$comp_bul2] <- TRUE
  									bullet_scores$selsource[bullet_scores$bulletB==input$comp_bul1 & bullet_scores$bulletA==input$comp_bul2] <- TRUE
  									bullet_scores %>% 
									  ggplot(aes(x = bulletA, y = bulletB, fill = bullet_score, colour=selsource)) +
									  geom_tile() +
									  labs(fill="Bullet Score") +
									  scale_fill_gradient2(low = "grey80", high = "darkorange", midpoint = .5, limits = c(0,1)) +
									  scale_colour_manual(values = c("black", "black")) +
									  geom_tile(linewidth = 1, data = bullet_scores %>% filter(selsource)) +
									  geom_text(aes(label = round(bullet_score, 2)),size=6) +
  									ggtitle("Bullet-to-Bullet Score Matrix") +
									  xlab("") +
									  ylab("") +
									  guides(colour="none") +
									  coord_equal() 
  						})

  	## Land Comparison Heatmap
  	output$land_comp <- renderPlot({
  									if(is.null(bulldata$comparison)) return(NULL)
  									if(is.null(input$comp_bul1) | is.null(input$comp_bul2)) return(NULL)
  									bullet_scores <- bulldata$comparison$bullet_scores
  									bullet_scores <- bullet_scores[bullet_scores$bulletA==input$comp_bul1 & bullet_scores$bulletB==input$comp_bul2,]
  									features <- bullet_scores %>% tidyr::unnest(data)
  									features %>% 
									  ggplot(aes(x = landA, y = landB, fill = rfscore, colour=samesource)) +
									  geom_tile() +
									  labs(fill="Land Score") +
									  scale_fill_gradient2(low = "grey80", high = "darkorange", midpoint = .5, limits = c(0,1)) +
									  scale_colour_manual(values = c("black", "black")) +
  									geom_tile(linewidth = 1, data = features %>% filter(samesource==TRUE)) +
  									geom_text(aes(label = round(rfscore, 2)),size=6) +
									  xlab(sprintf("Lands on %s", features$bulletA[1])) +
  									ylab(sprintf("Lands on %s", features$bulletB[1])) + 
  									ggtitle("Land-to-Land Score Matrix",
  									subtitle=sprintf("Bullet: %s vs %s", features$bulletA[1], features$bulletB[1])) + 
									  guides(colour="none") +
									  coord_equal() 
  						})

  	## Visualize Cross Cuts 
  	output$land_visCC <- renderPlot({
  									if(is.null(bulldata$comparison)) return(NULL)
  									if(is.null(input$comp_bul1) | is.null(input$comp_bul2)) return(NULL)

  									bullets <- bulldata$comparison$bullets
  									bullets <- bullets[bullets$bullet %in% c(input$comp_bul1,input$comp_bul2),]
  									crosscuts <- bullets %>% tidyr::unnest(ccdata)
  									crosscuts$x <- crosscuts$x/1000
  									CCplot <- crosscuts %>% 
											  ggplot(aes(x = x, y = value)) + 
											  geom_line() +
											  facet_grid(bullet~land, labeller="label_both") +
											  xlab("Position along width of Land [mm]") +
											  ylab("Surface Height [µm]") + 
											  ggtitle("Cross-section of the bullet land at a suitable cross-section location") 
									return(CCplot)
  						})

  	## Visualize Signals
  	output$land_visSig <- renderPlot({
  									if(is.null(bulldata$comparison)) return(NULL)
  									if(is.null(input$comp_bul1) | is.null(input$comp_bul2)) return(NULL)

  									bullets <- bulldata$comparison$bullets
  									bullets <- bullets[bullets$bullet %in% c(input$comp_bul1,input$comp_bul2),]
  									signatures <- bullets %>% select(source,bullet,land, sigs) %>% tidyr::unnest(sigs)
  									signatures$x <- signatures$x/1000
  									Sigplot <- signatures %>% 
												  filter(!is.na(sig),!is.na(raw_sig)) %>%
												  ggplot(aes(x = x)) + 
												  geom_line(aes(y = raw_sig), colour = "grey70",show.legend = T) +
												  geom_line(aes(y = sig), colour = "grey30",show.legend = T) +
												  facet_grid(bullet~land, labeller="label_both") +
												  ylim(c(-5,5)) +
												  xlab("Position along width of Land [mm]") +
												  ylab("Signal [µm]") +
												  ggtitle("Raw and LOESS-smoothed Signal for Bullet Profile")
									return(Sigplot)
  						})
  	#################################################################################
  	#################################################################################
}