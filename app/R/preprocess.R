cond_x3p_m_to_mum <- function(x3p) {
  scale <- x3p %>% x3p_get_scale()
  if (scale < .1) x3p <-  x3p %>% x3p_m_to_mum() # only scale conditionally
  x3p
}

# Check if we need to down-sample the bullet. Calculate the closest integer
# `n` that samples reference resolution to match incrementX
downsample_bullet <- function(allbull, cbull, show_alert, session) {
  if (nrow(allbull) > 0) {
    reference_resolution <- x3p_get_scale(allbull$x3p[[1]]) / 1e6
    current_resolution <- x3p_get_scale(cbull$x3p[[1]])
    
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
      
      cbull$x3p <- lapply(cbull$x3p, x3p_sample, m = m)
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
  
  return(list(allbull = allbull, cbull = cbull, show_alert = show_alert))
}

preprocess_bullet <- function(allbull, cbull, show_alert, progress, session) {
  
  # Rotate bullet (optional)
  rotate_results <- rotate_bullet(
    bullet = cbull, 
    show_alert = show_alert, 
    session = session
  )
  cbull <- rotate_results$bullet
  show_alert <- rotate_results$show_alert
  
  # Down-sample bullet (optional)
  downsample_results <- downsample_bullet(
    allbull = allbull,
    cbull = cbull,
    show_alert = show_alert,
    session = session
  )
  allbull <- downsample_results$allbull
  cbull <- downsample_results$cbull
  show_alert <- downsample_results$show_alert
  
  # Convert to microns (optional)
  cbull$x3p <- lapply(cbull$x3p, cond_x3p_m_to_mum)
  
  # Get hash
  cbull$md5sum <- tools::md5sum(cbull$source)
  
  # Get names
  cbull$filename <- basename(cbull$source)
  cbull$land_names <- as.character(1:nrow(cbull))
  cbull$bullet_name <- identify_bullet(cbull$filename)
  
  return(list(allbull = allbull, cbull = cbull))
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