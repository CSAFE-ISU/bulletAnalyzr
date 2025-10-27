#' Parse RGL Widget UI
#'
#' Creates a bslib card containing an RGL widget for displaying 3D bullet land scans.
#'
#' @param x A numeric index for the land
#' @param name A string prefix for the RGL widget output ID
#' @param land_name Optional label for the land (defaults to x)
#'
#' @returns A bslib card UI element
#' @noRd
parse_rglui <- function(x, name = "x3prgl", land_name = NULL) {
  if (is.null(land_name)) land_name <- x
  bslib::card(
    bslib::card_header(class = "bg-dark",paste0("Land ", land_name)),
    max_height = 600,
    full_screen = FALSE,
    rgl::rglwidgetOutput(paste0(name,x),height=600,width=200),
  )
}

#' Render Crosscut Snapshot
#'
#' Creates a PNG snapshot of a 3D bullet land scan with crosscut line marked.
#'
#' @param src A string containing the source file path
#' @param x3p An x3p object containing the scan data
#' @param ccut A numeric value for the crosscut location
#'
#' @returns A string containing the path to the saved PNG image
#' @noRd
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
  rgl::snapshot3d(imgsrc, webshot = TRUE)
  return(imgsrc)
}

#' Render Crosscut Snapshots Wrapper
#'
#' Creates PNG snapshots for all bullet lands with crosscut lines marked.
#'
#' @param bullets A data frame containing bullet data with x3p objects
#' @param progress A Shiny progress object
#'
#' @returns A data frame with added x3pimg column containing image paths
#' @noRd
render_crosscut_snap_wrapper <- function(bullets, progress) {
  for(idx in 1:nrow(bullets)) {
    progress$set(message = "Rendering Report Objects", value = round(seq(from = .55, to = .85, length.out = nrow(bullets)), 2)[idx])
    bullets$x3pimg[idx] <- render_crosscut_snap(bullets$source[idx], bullets$x3p[[idx]], bullets$crosscut[idx])
  }
  return(bullets)
}

#' Render Crosscut Slider
#'
#' Creates a Shiny slider input for adjusting crosscut location.
#'
#' @param id A numeric ID for the slider
#' @param ymin A numeric minimum value for the slider
#' @param ymax A numeric maximum value for the slider
#' @param yset A numeric default value for the slider
#'
#' @returns A Shiny slider input UI element
#' @noRd
render_ccsl <- function(id, ymin, ymax, yset) {
  shiny::sliderInput(inputId = paste0("CCsl",id), label = NULL, min = ymin, max = ymax, value = yset)
}

#' Render 3D Bullet Land Scan
#'
#' Creates a 3D rendering of a bullet land scan with optional crosscut line and rotation.
#'
#' @param x3p An x3p object containing the scan data
#' @param ccut Optional numeric value for crosscut location (default NULL)
#' @param sample_m Numeric sampling factor for downsampling (default 5)
#' @param rotate Logical; if TRUE, rotates the image 90 degrees (default TRUE)
#' @param img_size Numeric size parameter for the image (default 500)
#' @param img_zoom Numeric zoom level for the image (default 0.4)
#'
#' @returns An RGL widget image
#' @noRd
render_land <- function(x3p, ccut = NULL, sample_m = 5, rotate = TRUE, img_size = 500, img_zoom = 0.4) {
  if (!is.null(ccut) && rotate) {
    img <- x3p %>%
      x3ptools::x3p_add_hline(yintercept = ccut, size = 20, color = "#eeeeee") %>%
      x3ptools::x3p_sample(m = sample_m) %>%
      x3ptools::x3p_rotate() %>%
      x3ptools::x3p_image(size = img_size, zoom = img_zoom)
  } else if (!is.null(ccut) && !rotate) {
    img <- x3p %>%
      x3ptools::x3p_add_hline(yintercept = ccut, size = 20, color = "#eeeeee") %>%
      x3ptools::x3p_sample(m = sample_m) %>%
      x3ptools::x3p_image(size = img_size, zoom = img_zoom)
  } else if (is.null(ccut) && rotate) {
    img <- x3p %>%
      x3ptools::x3p_sample(m = sample_m) %>%
      x3ptools::x3p_rotate() %>%
      x3ptools::x3p_image(size = img_size, zoom = img_zoom)
  } else if (is.null(ccut) && !rotate) {
    img <- x3p %>%
      x3ptools::x3p_sample(m = sample_m) %>%
      x3ptools::x3p_image(size = img_size, zoom = img_zoom)
  }
  
  return(img)
}

#' Render Session Info
#'
#' Renders R session information as text for the About tab.
#'
#' @param session The Shiny session object
#'
#' @returns A Shiny renderText output
#' @noRd
render_session_info <- function(session) {
  shiny::renderText({{
    sessioninfo::session_info(to_file = TRUE)
    sessionInfo <- readLines(con="session-info.txt")
    paste(sessionInfo, collapse="\n")
  }})
}
