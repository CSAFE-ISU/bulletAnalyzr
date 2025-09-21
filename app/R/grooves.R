get_grooves <- function(bullets, progress = NULL) {
  if (!is.null(progress)) {
    progress$set(message = "Get the Groove Locations", value = .05)
  }
  
  bullets$grooves <- lapply(
    bullets$ccdata, 
    function(x) cc_locate_grooves(x, method = "middle", adjust = 30, return_plot = FALSE)
  )
  return(bullets)
}