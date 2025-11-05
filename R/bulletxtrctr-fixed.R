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

#' Bullet Phase test
#'
#' Determine a p-value for the strength of similarity between two bullets based on scores for each land-to-land comparison.
#' @param land1 vector with land ids of the LEAs in bullet 1
#' @param land2 vector with land ids of the LEAs in bullet 2
#' @param score real-valued vector consisting of land-to-land comparisons. Higher values are assumed to describe higher similarity.
#' @param sigma_0 real valued, standard deviation of the different source distribution. If `NA` (default), the standard deviation of the scores
#' corresponding to non-matches (after selecting the highest phase) is taken. Real-values are interpreted as fixed standard deviation.
#' The user can provide their own function of the form `function (data)`, where `data` is a data frame with `land1, land2, score,` and `phase`.
#' The `phase` vector consists of integer values 1, ..., `k`, where `k` is the maximum of the number of unique lands in land1 and land2. The values are
#' ordered such that the highest value `k` of phase corresponds to the elements in score with the highest average score.
#' @return phase.test object. List of estimate=est1-est2,estimate1=est1, estimate2=est2, statistic= test.statistic,
#' p.value=pvalue, parameter=sigma_0, data = dframe
#' @noRd
phase_test_fixed <- function(land1, land2, score, sigma_0 = NA) {
  # Prevent no visible binding for global variable note
  phase <- inphase <- sd <- nu <- NULL
  
  # data frame with structure land 1, land 2, score
  # returns estimate 1, estimate 2, test statistic (difference), sigma (of reference distribution),
  # and p-value
  if (is.numeric(sigma_0)) stopifnot(sigma_0 > 0) # only positive values for sigma_0 are allowed
  
  df <- data.frame(land1, land2, score)
  relabeled <- relabel_phases(df = df)
  
  n <- relabeled$n
  est1 <- relabeled$avgs$means[n]
  est2 <- relabeled$avgs$means[floor(n/2)]
  
  if (is.na(sigma_0)) {
    # use pooled variance estimator
    sigmas <- relabeled$df %>% 
      dplyr::mutate(inphase = phase==n) %>%
      dplyr::group_by(inphase) %>%
      dplyr::summarize(sd = stats::sd(score, na.rm=TRUE), nu = sum(!is.na(score)) - 1) %>%
      dplyr::ungroup() %>%
      dplyr::summarize(
        pooled = sum(nu*sd)/sum(nu)
      )
    sigma_0 <- sigmas$pooled[1]
  }
  if (is.function(sigma_0)) { 
    sigma_0 = sigma_0(df)
  }
  test.statistic <- (est1-est2)
  pvalue <- bulletxtrctr::F_T(test.statistic, sigma = sigma_0, n = n, lower.tail = FALSE)
  res <- list(
    estimate = est1 - est2,
    estimate1 = est1, 
    estimate2 = est2, 
    statistic = test.statistic,
    p.value = pvalue,
    parameter = sigma_0, 
    n = n, 
    data = df
  )
  class(res) <- c("phase.test", "list")
  return(res)
}

#' Relabel Phases by Mean
#'
#' This is a helper function for phase_test_fixed(). Calculate the mean score for each phase. Then, label the phases from
#' 1=smallest mean to n=largest mean.
#'
#' @param df A data frame containing land1, land2, and ccf columns.
#'
#' @returns A list
#' @noRd
relabel_phases <- function(df) {
  # Prevent no visible binding for global variable note
  land1 <- land2 <- phase <- score <- means <- NULL
  df <- df %>%
    dplyr::arrange(land1, land2) %>% # provides row-first order (land2 is slower index)
    dplyr::mutate(
      phase = bulletxtrctr::get_phases(land1, land2)
    ) %>%
    dplyr::arrange(land1, land2)
  n <- max(df$phase)
  avgs <- df %>% 
    dplyr::group_by(phase) %>%
    dplyr::summarize(
      means = mean(score, na.rm = TRUE)
    ) %>% 
    dplyr::ungroup() %>%
    dplyr::arrange(means) %>%
    dplyr::mutate(
      ordered = 1:n
    )
  df <- df %>% 
    dplyr::left_join(avgs, by = "phase")
  df <- df %>%
    dplyr::select(-tidyselect::any_of(c("phase", "means"))) %>%
    dplyr::rename(phase = ordered)
  return(list(df = df, avgs = avgs, n = n))
}
