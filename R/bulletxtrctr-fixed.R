#' Bullet to Land Predict Fixed
#'
#' A fixed version of `bulletxtrctr::bullet_to_land_predict()`. The bulletxtrctr
#' version runs correctly on Mac but not on Windows.
#'
#' @param land1 (numeric) vector with land ids of bullet 1.
#' @param land2 (numeric) vector with land ids of bullet 2.
#' @param scores (numeric) vector of scores.
#' @param difference (numeric) value describing the minimal difference between
#'   scores from same source versus different sources.
#' @param alpha (numeric) value describing the significance level for the
#'   bootstrap.
#' @param addNA how are missing values treated? addNA = TRUE leaves missing
#'   values, addNA=FALSE imputes with 0.
#'
#' @returns A logical vector
#' @noRd
bullet_to_land_predict_fixed <- function(
    land1, 
    land2, 
    scores, 
    difference = 0.01, 
    alpha = 0.9, 
    addNA = FALSE) {
  
  dframe <- data.frame(land1 = land1, land2 = land2, scores = scores)
  dframe <- dframe %>% 
    dplyr::mutate(phase = bulletxtrctr::get_phases(land1, land2))
  avgs <- compute_average_scores_fixed(
    land1 = land1, 
    land2 = land2, 
    score = scores
  )
  idx <- which.max(avgs)
  
  return(dframe$phase == idx)
}

#' Compute Average Scores Fixed
#'
#' A fixed version of `bulletxtrctr::compute_average_scores()`. The bulletxtrctr
#' version runs correctly on Mac but not on Windows.
#'
#' @param land1 (numeric) vector with land ids of bullet 1.
#' @param land2 (numeric) vector with land ids of bullet 2.
#' @param score (numeric) vector of scores.
#'
#' @returns A numeric vector
#' @noRd
compute_average_scores_fixed <- function(land1, land2, score) {
  # Prevent no visible binding for global variable note
  phase <- scores <- NULL
  
  dframe <- data.frame(land1 = land1, land2 = land2, score = score)
  dframe <- dframe %>% 
    dplyr::mutate(phase = bulletxtrctr::get_phases(land1, land2))
  dframe <- dframe %>% 
    dplyr::group_by(phase) %>% 
    dplyr::summarize(scores = mean(score, na.rm = addNA)) 
  return(dframe$scores)
}
