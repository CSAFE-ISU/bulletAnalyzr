cond_x3p_m_to_mum <- function(x3p)
{
  scale <- x3p %>% x3p_get_scale()
  if (scale < .1) x3p <-  x3p %>% x3p_m_to_mum() # only scale conditionally
  x3p
}

identify_lands <- function(words) {
  # create a list of distinguishing elements between names
  
  if (length(words) == 1) return(1)

  # split each word by character and transpose  
  list <- strsplit(words, split="")
  tlist <- purrr::list_transpose(list)
  # toss out everything that matches
  difflist <- tlist %>% purrr::map(.f = function(l) { if (length(unique(l)) > 1) return(l); NULL})
  difflist <- purrr::discard(difflist, is.null) 
  # transpose back and make 'word'
  difflist <- difflist %>% purrr::list_transpose() %>% purrr::map_chr(paste, collapse="")
#  if (length(words) != length(difflist)) browser()
  make.unique(difflist) # make sure that something is there and it is different
}


identify_bullet <- function(words) {
  # create a list of the same elements between names
  if (length(words) == 1) return(words)
  
  # split each word by character and transpose  
  list <- strsplit(words, split="")
  tlist <- purrr::list_transpose(list)
  # toss out everything that's different
  samelist <- tlist %>% purrr::map(.f = function(l) { if (length(unique(l)) == 1) return(l); NULL})
  samelist <- purrr::discard(samelist, is.null) 
  # transpose back and make 'word'
  if (length(samelist) == 0) return("Enter name of Bullet")
  samelist <- samelist %>% purrr::list_transpose() 
  make.names(paste(samelist[[1]], collapse="")) # delete all forbidden characters
}


groove_plot <- function(ccdata, grooves) {
    ccdata %>% 
    ggplot(aes(x = x, y = value)) + 
#    theme_bw()+
    geom_vline(xintercept = 0, colour = "grey50") + 
    geom_vline(xintercept = grooves[2]-grooves[1], colour = "grey50") +
    geom_line(linewidth = .5) + # put signal in front
    annotate("rect", fill="grey50", alpha = 0.15, xmax = 0, xmin = -Inf, ymin = -Inf, ymax = Inf) +
    annotate("rect", fill="grey50", alpha = 0.15, xmax = Inf, xmin = grooves[2]-grooves[1], ymin = -Inf, ymax = Inf) +
    geom_line(linewidth = 1, data = filter(ccdata, between(x, 0, grooves[2]-grooves[1]))) + # put signal in front
    scale_x_continuous(
      breaks=c(0,round(as.numeric(grooves[2]-grooves[1]),0),round(seq(min(ccdata$x),max(ccdata$x),by=500),-2)),
      labels=c("\n0",paste0("\n",round(as.numeric(grooves[2]-grooves[1]),0)),round(seq(min(ccdata$x),max(ccdata$x),by=500),-2))
    ) +
    xlab("Position along width of Land [µm]") +
    ylab("Surface Height [µm]") 
}

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

make_export_df <- function(df) {
  # Modify data frame for export for testing. Drop the x3p column because it
  # makes the snapshots 100+ MB. Change source column from filepath to filename
  # because the temp directory filepath will change every time, but the
  # filenames should remain consistent.
  df <- df %>% 
    dplyr::select(-tidyselect::any_of(c("x3p", "x3pimg"))) 
  
  if ("source" %in% colnames(df)) {
    df <- df %>%
      dplyr::mutate(source = basename(source))
  }

  return(df)
}

## Render Land into image with CrossCut line
render_land <- function(src, x3p, ccut) {
  imgsrc <- gsub(".x3p$", ".png", src)
  x3p %>%
    x3p_add_hline(yintercept = ccut, size = 20, color = "#eeeeee") %>%
    x3p_sample(m = 5) %>%
    x3p_image(size = 600, zoom = .25)
  snapshot3d(imgsrc, webshot = TRUE)
  return(imgsrc)
}

## Render Slider to adjust CrossCut
render_ccsl <- function(id, ymin,ymax,yset)
{
  sliderInput(inputId = paste("CCsl",id), label = NULL, min = ymin, max = ymax, value = yset)
}

# Render the session info as text
render_session_info <- function(session) {
  renderText({{
    sessioninfo::session_info(to_file = TRUE)
    sessionInfo <- readLines(con="session-info.txt")
    paste(sessionInfo, collapse="\n")
  }})
}

try_x3p_crosscut <- function(x3p, y = NULL, range = 1e-5) 
{
  res <- x3p_crosscut(x3p=x3p, y = y, range = range)
  if (nrow(res) == 0) res <- x3p_crosscut(x3p=x3p, y = NULL, range = range)
  return(res)
}
