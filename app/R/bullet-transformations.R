rotate_bullet <- function(bullet, values, session = NULL) {
  
  hinfo <- bullet$x3p[[1]]$header.info
  
  if (hinfo$sizeX < hinfo$sizeY) {
    if (values$show_alert && !is.null(session)) {
      showModal(modalDialog(
        title = "Rotated Bullet",
        "Detected rotated bullet, rotating 90 degrees...",
        easyClose = TRUE,
        footer = modalButton("OK")
      ), session = session)
    }
    show_alert <- FALSE
    bullet$x3p <- lapply(bullet$x3p, x3p_rotate, angle = 90)
  } else {
    show_alert <- values$show_alert
  }
  
  return(list(bullet = bullet, show_alert = show_alert))
}