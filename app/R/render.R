## Render RGL Widget UI
parse_rglui <- function(x, name = "x3prgl", land_name = NULL) {
  if (is.null(land_name)) land_name <- x
  card(
    card_header(class = "bg-dark",paste0("Land ", land_name)),
    max_height = 600,
    full_screen = FALSE,
    rglwidgetOutput(paste0(name,x),height=600,width=200),
  )
}

## Render Land into image with CrossCut line
render_crosscut_snap <- function(src, x3p, ccut) {
  imgsrc <- gsub(".x3p$", ".png", src)
  x3p %>%
    x3p_add_hline(yintercept = ccut, size = 20, color = "#eeeeee") %>%
    x3p_sample(m = 5) %>%
    x3p_image(size = 600, zoom = .25)
  snapshot3d(imgsrc, webshot = TRUE)
  return(imgsrc)
}

## Render Slider to adjust CrossCut
render_ccsl <- function(id, ymin, ymax, yset) {
  sliderInput(inputId = paste("CCsl",id), label = NULL, min = ymin, max = ymax, value = yset)
}

render_land <- function(x3p) {
  x3p %>%
    x3p_sample(m = 5) %>%
    x3p_rotate() %>%
    x3p_image(size = 500, zoom = .4)
}

# Render the session info as text
render_session_info <- function(session) {
  renderText({{
    sessioninfo::session_info(to_file = TRUE)
    sessionInfo <- readLines(con="session-info.txt")
    paste(sessionInfo, collapse="\n")
  }})
}