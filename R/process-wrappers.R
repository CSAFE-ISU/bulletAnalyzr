#' Align Signals Wrapper
#'
#' Aligns signals between all pairs of bullet lands for comparison.
#'
#' @param bullets A data frame containing bullet data with extracted signals
#' @param progress A Shiny progress object (optional)
#'
#' @returns A list containing updated bullets and comparisons data frames
#' @noRd
get_aligned_signals_wrapper <- function(bullets, progress = NULL) {
  if (!is.null(progress)) {
    progress$set(message = "Align Signals", value = .15)
  }
  
  bullets$bulletland <- paste0(bullets$bullet, "-", bullets$land)
  lands <- unique(bullets$bulletland)
  comparisons <- data.frame(expand.grid(land1 = lands, land2 = lands), stringsAsFactors = FALSE)
  comparisons$aligned <- mapply(
    function(x, y, bullets) bulletxtrctr::sig_align(bullets$sigs[bullets$bulletland == x][[1]]$sig, bullets$sigs[bullets$bulletland == y][[1]]$sig),
    comparisons$land1,
    comparisons$land2,
    MoreArgs = list(bullets = bullets),
    SIMPLIFY = FALSE
  )
  return(list(bullets = bullets, comparisons = comparisons))
}

#' Calculate Bullet Scores Wrapper
#'
#' Computes bullet-level scores from land-level comparison features.
#'
#' @param features A data frame containing land comparison features and RF scores
#' @param progress A Shiny progress object
#'
#' @returns A data frame containing bullet scores
#' @noRd
get_bullet_scores_wrapper <- function(features, progress) {
  # Prevent no visible binding for global variable note
  bulletA <- bulletB <- NULL
  
  progress$set(message = "Preparing Report Data", value = .5)
  bullet_scores <- features %>% dplyr::group_by(bulletA, bulletB) %>% tidyr::nest()
  bullet_scores$bullet_score <- sapply(bullet_scores$data, function(d) max(bulletxtrctr::compute_average_scores(land1 = d$landA, land2 = d$landB, d$rfscore, verbose = FALSE)))
  return(bullet_scores)
}

#' Extract Crosscut Data Wrapper
#'
#' Extracts crosscut profile data from x3p objects at specified crosscut locations.
#'
#' @param postCC A data frame containing bullet data with x3p objects and crosscut locations
#' @param progress A Shiny progress object (optional)
#'
#' @returns A data frame with added ccdata column containing crosscut profiles
#' @noRd
get_ccdata_wrapper <- function(postCC, progress = NULL) {
  try_x3p_crosscut <- function(x3p, y = NULL, range = 1e-5) {
    res <- bulletxtrctr::x3p_crosscut(x3p=x3p, y = y, range = range)
    if (nrow(res) == 0) {
      res <- bulletxtrctr::x3p_crosscut(x3p=x3p, y = NULL, range = range)
    }
    return(res)
  }
  
  if (!is.null(progress)) {
    progress$set(message = "Finalizing Cross Sections", value = 0)
  }
  
  bullets <- postCC
  bullets$ccdata <- mapply(try_x3p_crosscut, postCC$x3p, postCC$crosscut, SIMPLIFY = FALSE)
  return(bullets)
}

#' Get Default Crosscut Locations Wrapper
#'
#' Automatically determines optimal crosscut locations for all bullet lands.
#'
#' @param bullets A data frame containing bullet data with x3p objects
#' @param ylimits A numeric vector of length 2 specifying Y-axis limits for optimization
#'
#' @returns A data frame with added crosscut column containing optimal locations
#' @noRd
get_default_cc_wrapper <- function(bullets, ylimits = c(150, NA)) {
  
  bullets$crosscut <- sapply(bullets$x3p, bulletxtrctr::x3p_crosscut_optimize, ylimits = ylimits)
  
  # Check for NA values
  na_idx <- is.na(bullets$crosscut)
  if (any(na_idx)) {
    bullet_land <- paste("Bullet", bullets$bullet, "Land", bullets$land)
    
    stop(paste("x3p_crosscut_optimize could not find a stable region in land:", 
               paste(bullet_land[na_idx], collapse = ", ")))
  }
  
  return(bullets)
}

#' Extract Features Wrapper
#'
#' Extracts all comparison features including CCF, striation marks, and derived metrics.
#'
#' @param comparisons A data frame containing aligned signal comparisons
#' @param resolution A numeric value for the scan resolution
#' @param progress A Shiny progress object
#'
#' @returns A list containing updated comparisons and a features data frame
#' @noRd
get_features_wrapper <- function(comparisons, resolution, progress) {
  # Prevent no visible binding for global variable note
  cms_per_mm <- matches_per_mm <- mismatches_per_mm <- non_cms_per_mm <- NULL
  
  # Calculate CCF0 ----
  progress$set(message = "Evaluating Features", value = .2)
  comparisons$ccf0 <- sapply(comparisons$aligned, function(x) bulletxtrctr::extract_feature_ccf(x$lands))
  
  # Evaluate striation marks ----
  progress$set(message = "Evaluating Striation Marks", value = .25)
  comparisons$striae <- lapply(comparisons$aligned, bulletxtrctr::sig_cms_max, span = 75)
  
  # Extract features ----
  progress$set(message = "Extracting Features", value = .35)
  comparisons$bulletA <- sapply(strsplit(as.character(comparisons$land1),"-"),"[[",1)
  comparisons$bulletB <- sapply(strsplit(as.character(comparisons$land2),"-"),"[[",1)
  comparisons$landA <- sapply(strsplit(as.character(comparisons$land1),"-"),"[[",2)
  comparisons$landB <- sapply(strsplit(as.character(comparisons$land2),"-"),"[[",2)
  comparisons$features <- mapply(
    bulletxtrctr::extract_features_all,
    comparisons$aligned,
    comparisons$striae,
    MoreArgs = list(resolution = resolution),
    SIMPLIFY = FALSE
  )
  
  # Scale features ----
  progress$set(message = "Scaling Features", value = .4)
  features <- tidyr::unnest(
    comparisons[,c("land1", "land2", "ccf0", "bulletA", "bulletB", "landA", "landB", "features")], 
    cols = features
  )
  features <- features %>% 
    dplyr::mutate(
      cms = cms_per_mm,
      matches = matches_per_mm, 
      mismatches = mismatches_per_mm, 
      non_cms = non_cms_per_mm
    )
  
  return(list(comparisons = comparisons, features = features))
}

#' Locate Grooves Wrapper
#'
#' Automatically locates groove positions for all bullet lands.
#'
#' @param bullets A data frame containing bullet data with crosscut profiles
#' @param progress A Shiny progress object (optional)
#'
#' @returns A data frame with added grooves column containing groove locations
#' @noRd
get_grooves_wrapper <- function(bullets, progress = NULL) {
  if (!is.null(progress)) {
    progress$set(message = "Get the Groove Locations", value = .05)
  }
  
  bullets$grooves <- lapply(
    bullets$ccdata, 
    function(x) bulletxtrctr::cc_locate_grooves(x, method = "middle", adjust = 30, return_plot = FALSE)
  )
  return(bullets)
}

#' Calculate Phase Scores Wrapper
#'
#' Computes average phase test scores grouped by same-source status.
#'
#' @param phase_test_results A data frame containing phase test results
#'
#' @returns A numeric vector of average scores
#' @noRd
get_phase_scores_wrapper <- function(phase_test_results) {
  # Prevent no visible binding for global variable note
  samesource <- scores <- NULL
  
  scores <- phase_test_results %>%
    dplyr::group_by(samesource) %>% 
    dplyr::summarize(avg = mean(scores, na.rm = TRUE)) %>%
    purrr::pluck("avg") %>%
    unlist()
  return(scores)
}

#' Prepare Report Data Wrapper
#'
#' Organizes all comparison data into a structured format for report generation.
#'
#' @param bullets A data frame containing bullet data
#' @param comparisons A data frame containing aligned signal comparisons
#' @param features A data frame containing comparison features
#' @param bullet_scores A data frame containing bullet-level scores
#' @param progress A Shiny progress object
#'
#' @returns A list containing comparison and comparison_export data
#' @noRd
get_report_data_wrapper <- function(bullets, comparisons, features, bullet_scores, progress) {
  progress$set(message = "Preparing Report", value = .9)
  comparison <- list(
    bullets = bullets,
    comparisons = comparisons,
    features_scaled = features,
    bullet_scores = bullet_scores
  )
  comparison_export <- list(
    bullets = make_export_df(bullets),
    comparisons = make_export_df(comparisons),
    features_scaled = make_export_df(features),
    bullet_scores = make_export_df(bullet_scores)
  )
  return(list(comparison = comparison, comparison_export = comparison_export))
}

#' Extract Signals Wrapper
#'
#' Extracts signature signals from crosscut data using groove locations.
#'
#' @param bullets A data frame containing bullet data with crosscut profiles and grooves
#' @param progress A Shiny progress object (optional)
#'
#' @returns A data frame with added sigs column containing extracted signals
#' @noRd
get_signals_wrapper <- function(bullets, progress = NULL) {
  if (!is.null(progress)) {
    progress$set(message = "Extracting Signal", value = .1)
  }
  
  bullets$sigs <- mapply(
    function(ccdata, grooves) bulletxtrctr::cc_get_signature(ccdata, grooves, span1 = 0.75, span2 = 0.03),
    bullets$ccdata,
    bullets$grooves,
    SIMPLIFY = FALSE
  )
  
  return(bullets)
}

#' Update Crosscut from Sliders Wrapper
#'
#' Updates crosscut locations in the bullets data frame from Shiny slider inputs.
#'
#' @param bullets A data frame containing bullet data
#' @param selected A string containing the name of the selected bullet
#' @param all_inputs A list of all Shiny input values
#'
#' @returns A data frame with updated crosscut values
#' @noRd
update_cc_from_slider_wrapper <- function(bullets, selected, all_inputs) {
  # Get current crosscut slider names and values as list ----
  cc_sliders <- unlist(all_inputs[grepl("^CCsl", names(all_inputs))])
  
  # Update crosscut column in bullets data frame ----
  bullets[bullets$bullet == selected,]$crosscut <- cc_sliders
  return(bullets)
}
