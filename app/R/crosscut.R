finalize_crosscuts <- function(postCC, progress = NULL) {
  if (!is.null(progress)) {
    progress$set(message = "Finalizing Cross Sections", value = 0)
  }
  
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

get_bullet_scores_wrapper <- function(features, progress) {
  progress$set(message = "Preparing Report Data", value = .5)
  bullet_scores <- features %>% group_by(bulletA, bulletB) %>% tidyr::nest()
  bullet_scores$bullet_score <- sapply(bullet_scores$data, function(d) max(compute_average_scores(land1 = d$landA, land2 = d$landB, d$rfscore, verbose = FALSE)))
  return(bullet_scores)
}
