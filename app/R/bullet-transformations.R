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