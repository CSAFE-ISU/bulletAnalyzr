finalize_crosscuts <- function(postCC) {
  bullets <- postCC
  bullets$ccdata <- mapply(try_x3p_crosscut, postCC$x3p, postCC$crosscut, SIMPLIFY = FALSE)
  return(bullets)
}

get_default_crosscuts <- function(bullets, interactive_cc, ylimits = c(150, NA)) {
  bullets$crosscut <- sapply(bullets$x3p, x3p_crosscut_optimize, ylimits = ylimits)
  
  # Store bullets as preCC or postCC ----
  if(interactive_cc) {
    preCC <- bullets
    postCC <- NULL
  }
  if(!interactive_cc) {
    postCC <- bullets
    preCC <- NULL
  }
  
  return(list(preCC = preCC, postCC = postCC))
}
