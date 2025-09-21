cond_x3p_m_to_mum <- function(x3p) {
  scale <- x3p %>% x3p_get_scale()
  if (scale < .1) x3p <-  x3p %>% x3p_m_to_mum() # only scale conditionally
  x3p
}

# Check if we need to down-sample the bullet. Calculate the closest integer
# `n` that samples reference resolution to match incrementX
downsample_bullet <- function(allbull, bullet, show_alert, session) {
  if (nrow(allbull) > 0) {
    reference_resolution <- x3p_get_scale(allbull$x3p[[1]]) / 1e6
    current_resolution <- x3p_get_scale(bullet$x3p[[1]])
    
    # Down-sample if necessary
    if (reference_resolution > current_resolution) {
      show_modal(
        title = "Higher Resolution Bullet",
        message = "Detected higher resolution bullet, down-sampling...",
        show_alert = show_alert,
        session = session
      )
      # Switch alert off ----
      show_alert <- FALSE
      m <- round(reference_resolution / current_resolution)
      
      bullet$x3p <- lapply(bullet$x3p, x3p_sample, m = m)
    } else if (reference_resolution < current_resolution) {
      show_modal(
        title = "Lower Resolution Bullet",
        message = "Detected lower resolution bullet, down-sampling previous bullets...",
        show_alert = show_alert,
        session = session
      )
      
      # Switch alert off ----
      show_alert <- FALSE
      
      m <- round(current_resolution / reference_resolution)
      allbull$x3p <- lapply(allbull$x3p, x3p_sample, m = m)
    }
  }
  
  return(list(allbull = allbull, show_alert = show_alert))
}

rotate_bullet <- function(bullet, show_alert, session = NULL) {
  
  hinfo <- bullet$x3p[[1]]$header.info
  
  if (hinfo$sizeX < hinfo$sizeY) {
    if (show_alert && !is.null(session)) {
      show_modal(
        title = "Rotated Bullet",
        message = "Detected rotated bullet, rotating 90 degrees...",
        show_alert = show_alert,
        session = session
      )
    }
    show_alert <- FALSE
    bullet$x3p <- lapply(bullet$x3p, x3p_rotate, angle = 90)
  }
  
  return(list(bullet = bullet, show_alert = show_alert))
}