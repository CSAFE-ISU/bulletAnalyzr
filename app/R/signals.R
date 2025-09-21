get_aligned_signals <- function(bullets, progress = NULL) {
  if (!is.null(progress)) {
    progress$set(message = "Align Signals", value = .15)
  }
  
  bullets$bulletland <- paste0(bullets$bullet, "-", bullets$land)
  lands <- unique(bullets$bulletland)
  comparisons <- data.frame(expand.grid(land1 = lands, land2 = lands), stringsAsFactors = FALSE)
  comparisons$aligned <- mapply(
    function(x, y, bullets) sig_align(bullets$sigs[bullets$bulletland == x][[1]]$sig, bullets$sigs[bullets$bulletland == y][[1]]$sig),
    comparisons$land1,
    comparisons$land2,
    MoreArgs = list(bullets = bullets),
    SIMPLIFY = FALSE
  )
  return(list(bullets = bullets, comparisons = comparisons))
}

get_signals <- function(bullets, progress = NULL) {
  if (!is.null(progress)) {
    progress$set(message = "Extracting Signal", value = .1)
  }
  
  bullets$sigs <- mapply(
    function(ccdata, grooves) cc_get_signature(ccdata, grooves, span1 = 0.75, span2 = 0.03),
    bullets$ccdata,
    bullets$grooves,
    SIMPLIFY = FALSE
  )
  
  return(bullets)
}