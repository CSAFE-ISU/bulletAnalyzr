# Comparison Utilities
#
# Shared helper functions for bullet comparison pipelines.
# Source this file at the top of comparison scripts.
#
# Usage:
#   source("docs/developers/comparisons/comparison-utils.R")


# Storage Paths -----------------------------------------------------------

get_study_path <- function(
    study, 
    extdrive = "/Volumes/T7_Shield/CSAFE/datasets/bullet_datasets",
    lss1 = "/Volumes/research/csafe-firearms/bullet-scans",
    lss2 = "/Volumes/lss/research/csafe-firearms/bullet-scans"
) {
  local_dir <- file.path(extdrive, study)
  lss1 <- file.path(lss1, study)
  lss2 <- file.path(lss2, study)
  
  if (dir.exists(local_dir)) {
    study_dir <- local_dir
  } else if (dir.exists(lss1)) {
    study_dir <- lss1
  } else if (dir.exists(lss2)) {
    study_dir <- lss2
  } else {
    stop("Are you connected to LSS or the external drive?")
  }
  return(study_dir)
}

# Unit Conversion ---------------------------------------------------------

#' Conditionally Convert x3p from Meters to Micrometers
#' @param x3p An x3p object
#' @returns An x3p object with scale in micrometers
cond_x3p_m_to_mum <- function(x3p) {
  scale <- x3ptools::x3p_get_scale(x3p)
  if (scale < 0.1) {
    x3p <- x3ptools::x3p_m_to_mum(x3p)
  }
  return(x3p)
}


# Preprocessing -----------------------------------------------------------

#' Rotate Bullet if Needed
#' Rotates bullet 90 degrees if sizeX is less than sizeY (incorrectly oriented)
#' @param bullet A data frame containing bullet data with x3p objects
#' @returns The bullet data frame with rotated x3p objects if needed
rotate_bullet_if_needed <- function(bullet) {
  for (i in seq_len(nrow(bullet))) {
    hinfo <- bullet$x3p[[i]]$header.info
    if (hinfo$sizeX < hinfo$sizeY) {
      cat("  Rotating land", i, "90 degrees (incorrect orientation detected)\n")
      bullet$x3p[[i]] <- x3ptools::x3p_rotate(bullet$x3p[[i]], angle = 90)
    }
  }
  return(bullet)
}

#' Preprocess a Bullet
#' @param bullet A data frame from bulletxtrctr::read_bullet()
#' @param bullet_name A name for this bullet
#' @returns Preprocessed bullet data frame
preprocess_bullet_standalone <- function(bullet, bullet_name) {
  cat("Preprocessing bullet:", bullet_name, "\n")
  
  # Rotate if needed
  bullet <- rotate_bullet_if_needed(bullet)
  
  # Convert to microns if needed
  bullet$x3p <- lapply(bullet$x3p, cond_x3p_m_to_mum)
  
  # Add metadata
  bullet$bullet <- bullet_name
  bullet$land <- seq_len(nrow(bullet))
  
  return(bullet)
}


# Signal Extraction and Alignment -----------------------------------------

#' Extract Signals from Crosscut Data
#' @param bullets Data frame with ccdata and grooves
#' @returns Data frame with sigs column added
extract_signals <- function(bullets) {
  cat("Extracting signals...\n")
  
  bullets$sigs <- mapply(
    function(ccdata, grooves) {
      bulletxtrctr::cc_get_signature(ccdata, grooves, span1 = 0.75, span2 = 0.03)
    },
    bullets$ccdata,
    bullets$grooves,
    SIMPLIFY = FALSE
  )
  
  return(bullets)
}

#' Align Signals Between All Land Pairs
#' @param bullets Data frame with signals
#' @returns List with bullets and comparisons data frames
align_signals <- function(bullets) {
  cat("Aligning signals between all land pairs...\n")
  
  bullets$bulletland <- paste0(bullets$bullet, "-", bullets$land)
  lands <- unique(bullets$bulletland)
  
  comparisons <- data.frame(
    expand.grid(land1 = lands, land2 = lands),
    stringsAsFactors = FALSE
  )
  
  comparisons$aligned <- mapply(
    function(x, y) {
      bulletxtrctr::sig_align(
        bullets$sigs[bullets$bulletland == x][[1]]$sig,
        bullets$sigs[bullets$bulletland == y][[1]]$sig
      )
    },
    comparisons$land1,
    comparisons$land2,
    SIMPLIFY = FALSE
  )
  
  cat("  Created", nrow(comparisons), "land-to-land comparisons\n")
  
  return(list(bullets = bullets, comparisons = comparisons))
}


# Feature Extraction ------------------------------------------------------

#' Extract All Features
#' @param comparisons Data frame with aligned signals
#' @param resolution Scan resolution
#' @returns List with comparisons and features data frames
extract_features <- function(comparisons, resolution) {
  cat("Extracting features...\n")
  
  # Calculate CCF
  cat("  Calculating cross-correlation...\n")
  comparisons$ccf0 <- sapply(comparisons$aligned, function(x) bulletxtrctr::extract_feature_ccf(x$lands))
  
  # Evaluate striation marks
  cat("  Evaluating striation marks...\n")
  comparisons$striae <- lapply(comparisons$aligned, bulletxtrctr::sig_cms_max, span = 75)
  
  # Extract bullet/land identifiers
  comparisons$bulletA <- sapply(strsplit(as.character(comparisons$land1), "-"), "[[", 1)
  comparisons$bulletB <- sapply(strsplit(as.character(comparisons$land2), "-"), "[[", 1)
  comparisons$landA <- sapply(strsplit(as.character(comparisons$land1), "-"), "[[", 2)
  comparisons$landB <- sapply(strsplit(as.character(comparisons$land2), "-"), "[[", 2)
  
  # Extract all features
  cat("  Extracting all features...\n")
  comparisons$features <- mapply(
    bulletxtrctr::extract_features_all,
    comparisons$aligned,
    comparisons$striae,
    MoreArgs = list(resolution = resolution),
    SIMPLIFY = FALSE
  )
  
  # Unnest and scale features
  features <- tidyr::unnest(
    comparisons[, c("land1", "land2", "ccf0", "bulletA", "bulletB", "landA", "landB", "features")],
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


# Scoring -----------------------------------------------------------------

#' Calculate Random Forest Scores
#' @param features Data frame with extracted features
#' @returns Data frame with rfscore column added
calculate_rf_scores <- function(features) {
  cat("Calculating random forest scores...\n")
  
  features$rfscore <- predict(bulletxtrctr::rtrees, newdata = features, type = "prob")[, 2]
  
  return(features)
}

#' Calculate Bullet-Level Scores
#' @param features Data frame with RF scores
#' @returns Data frame with bullet scores
calculate_bullet_scores <- function(features) {
  cat("Calculating bullet-level scores...\n")
  
  bullet_scores <- features %>%
    dplyr::group_by(bulletA, bulletB) %>%
    tidyr::nest()
  
  bullet_scores$bullet_score <- sapply(
    bullet_scores$data,
    function(d) {
      # Compute average scores for each phase
      dframe <- data.frame(land1 = d$landA, land2 = d$landB, score = d$rfscore)
      dframe <- dframe %>%
        dplyr::mutate(phase = bulletxtrctr::get_phases(land1, land2))
      avgs <- dframe %>%
        dplyr::group_by(phase) %>%
        dplyr::summarize(scores = mean(score, na.rm = TRUE))
      return(max(avgs$scores))
    }
  )
  
  return(bullet_scores)
}


# Statistical Tests -------------------------------------------------------

#' Run Phase Test
#' @param features Data frame with CCF scores
#' @param bulletA Name of bullet A
#' @param bulletB Name of bullet B
#' @returns Phase test results
run_phase_test <- function(features, bulletA, bulletB) {
  cat("Running phase test...\n")
  
  # Filter to just the comparison between the two different bullets
  comparison_data <- features %>%
    dplyr::filter(
      (features$bulletA == bulletA & features$bulletB == bulletB) |
        (features$bulletA == bulletB & features$bulletB == bulletA)
    )
  
  if (nrow(comparison_data) == 0) {
    warning("No comparison data found between the two bullets")
    return(NULL)
  }
  
  # Get the A vs B comparison (not B vs A to avoid duplication)
  comparison_data <- comparison_data %>%
    dplyr::filter(bulletA == !!bulletA & bulletB == !!bulletB)
  
  # Run phase test using CCF scores (same as bulletAnalyzrApp)
  result <- tryCatch(
    {
      df <- data.frame(
        land1 = comparison_data$landA,
        land2 = comparison_data$landB,
        score = comparison_data$ccf
      )
      
      df <- df %>%
        dplyr::mutate(phase = bulletxtrctr::get_phases(land1, land2))
      
      n <- max(df$phase)
      avgs <- df %>%
        dplyr::group_by(phase) %>%
        dplyr::summarize(means = mean(score, na.rm = TRUE)) %>%
        dplyr::arrange(means)
      
      est1 <- avgs$means[n]
      est2 <- avgs$means[floor(n / 2)]
      
      # Calculate pooled variance
      df_labeled <- df %>%
        dplyr::left_join(avgs %>% dplyr::mutate(ordered = dplyr::row_number()), by = "phase") %>%
        dplyr::mutate(inphase = ordered == n)
      
      sigmas <- df_labeled %>%
        dplyr::group_by(inphase) %>%
        dplyr::summarize(
          sd = stats::sd(score, na.rm = TRUE),
          nu = sum(!is.na(score)) - 1
        ) %>%
        dplyr::ungroup() %>%
        dplyr::summarize(pooled = sum(nu * sd) / sum(nu))
      
      sigma_0 <- sigmas$pooled[1]
      
      test_statistic <- est1 - est2
      p_value <- bulletxtrctr::F_T(test_statistic, sigma = sigma_0, n = n, lower.tail = FALSE)
      
      # Return same structure as bulletAnalyzrApp
      res <- list(
        estimate = est1 - est2,
        estimate1 = est1,
        estimate2 = est2,
        statistic = test_statistic,
        p.value = p_value,
        parameter = sigma_0,
        n = n,
        data = df[, c("land1", "land2", "score")]
      )
      class(res) <- c("phase.test", "list")
      res
    },
    error = function(e) {
      warning(paste("Phase test failed:", e$message))
      return(NULL)
    }
  )
  
  return(result)
}


# Batch Comparison Helpers ------------------------------------------------

#' Create Pairs Data Frame from Bullet Directories
#' @param bullets Vector of bullet directory paths
#' @returns Data frame with bullet1, bullet2, bullet1_name, bullet2_name columns
make_pairs_df <- function(bullets) {
  pairs <- combn(bullets, 2)
  df <- data.frame(bullet1 = pairs[1, ], bullet2 = pairs[2, ])
  df$bullet1_name <- sapply(df$bullet1, parse_filepath)
  df$bullet2_name <- sapply(df$bullet2, parse_filepath)
  return(df)
}

#' Create Output File Path for Comparison Results
#' @param outdir Output directory path
#' @param bullet1_name Name of bullet 1
#' @param bullet2_name Name of bullet 2
#' @returns File path string
make_outfile <- function(outdir, bullet1_name, bullet2_name) {
  return(file.path(outdir, paste0(bullet1_name, "_", bullet2_name, ".rds")))
}
