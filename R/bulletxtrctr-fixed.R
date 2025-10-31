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