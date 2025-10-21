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
  render_land(
    x3p = x3p, 
    ccut = ccut,
    sample_m = 5,
    rotate = FALSE,
    img_size = 200,
    img_zoom = 0.25
  )
  snapshot3d(imgsrc, webshot = TRUE)
  return(imgsrc)
}

render_crosscut_snap_wrapper <- function(bullets, progress) {
  for(idx in 1:nrow(bullets)) {
    progress$set(message = "Rendering Report Objects", value = round(seq(from = .55, to = .85, length.out = nrow(bullets)), 2)[idx])
    bullets$x3pimg[idx] <- render_crosscut_snap(bullets$source[idx], bullets$x3p[[idx]], bullets$crosscut[idx])
  }
  return(bullets)
}

## Render Slider to adjust CrossCut
render_ccsl <- function(id, ymin, ymax, yset) {
  sliderInput(inputId = paste0("CCsl",id), label = NULL, min = ymin, max = ymax, value = yset)
}

render_land <- function(x3p, ccut = NULL, sample_m = 5, rotate = TRUE, img_size = 500, img_zoom = 0.4) {
  if (!is.null(ccut) && rotate) {
    img <- x3p %>%
      x3p_add_hline(yintercept = ccut, size = 20, color = "#eeeeee") %>%
      x3p_sample(m = sample_m) %>%
      x3p_rotate() %>%
      x3p_image(size = img_size, zoom = img_zoom)
  } else if (!is.null(ccut) && !rotate) {
    img <- x3p %>%
      x3p_add_hline(yintercept = ccut, size = 20, color = "#eeeeee") %>%
      x3p_sample(m = sample_m) %>%
      x3p_image(size = img_size, zoom = img_zoom)
  } else if (is.null(ccut) && rotate) {
    img <- x3p %>%
      x3p_sample(m = sample_m) %>%
      x3p_rotate() %>%
      x3p_image(size = img_size, zoom = img_zoom)
  } else if (is.null(ccut) && !rotate) {
    img <- x3p %>%
      x3p_sample(m = sample_m) %>%
      x3p_image(size = img_size, zoom = img_zoom)
  }
   
  return(img)
}

# Render the session info as text
render_session_info <- function(session) {
  renderText({{
    sessioninfo::session_info(to_file = TRUE)
    sessionInfo <- readLines(con="session-info.txt")
    paste(sessionInfo, collapse="\n")
  }})
}