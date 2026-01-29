#!/usr/bin/env Rscript
# Manual Bullet Comparison Pipeline Script
#
# This script compares two bullets using the same workflow as bulletAnalyzrApp()
# but reads crosscut and groove locations from groove CSV files in each bullet
# directory (created by manual_groove_selection.R).
#
# Usage:
#   Rscript manual_bullet_comparison_pipeline.R
#
# Or interactively in R:
#   source("manual_bullet_comparison_pipeline.R")
#   results <- compare_bullets("examples/Hamby-44/Barrel 1/Bullet 1", "examples/Hamby-44/Barrel 1/Bullet 2")

library(x3ptools)
library(bulletxtrctr)
library(randomForest)
library(dplyr)
library(tidyr)


# ============================================================================
# PARALLEL HELPERS
# ============================================================================

par_lapply <- function(X, FUN, cores = 1L, ...) {
  if (cores > 1L) parallel::mclapply(X, FUN, mc.cores = cores, ...)
  else lapply(X, FUN, ...)
}

par_mapply <- function(FUN, ..., cores = 1L, MoreArgs = NULL, SIMPLIFY = FALSE) {
  if (cores > 1L) parallel::mcmapply(FUN, ..., mc.cores = cores, MoreArgs = MoreArgs, SIMPLIFY = SIMPLIFY)
  else mapply(FUN, ..., MoreArgs = MoreArgs, SIMPLIFY = SIMPLIFY)
}


# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

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

  # Store filename for matching with groove CSV
  bullet$filename <- basename(bullet$source)

  return(bullet)
}

#' Read Groove Locations from CSV
#' @param bullet_dir Path to bullet directory containing a groove CSV file
#' @returns Data frame with groove locations or NULL if not found
read_grooves_csv <- function(bullet_dir) {
  groove_files <- list.files(bullet_dir, pattern = "groove.*\\.csv$",
                             ignore.case = TRUE, full.names = TRUE)

  if (length(groove_files) == 0) {
    return(NULL)
  }

  if (length(groove_files) > 1) {
    warning(paste("Multiple groove CSV files found in", bullet_dir,
                  "- using:", basename(groove_files[1])))
  }

  grooves_data <- read.csv(groove_files[1], stringsAsFactors = FALSE)
  return(grooves_data)
}

#' Get Crosscut Locations from groove CSV
#' @param bullets A data frame containing bullet data with x3p objects
#' @param bullet1_dir Path to bullet 1 directory
#' @param bullet2_dir Path to bullet 2 directory
#' @param bullet1_name Name of bullet 1
#' @param bullet2_name Name of bullet 2
#' @returns Data frame with crosscut column added
get_crosscuts_from_csv <- function(bullets, bullet1_dir, bullet2_dir, bullet1_name, bullet2_name) {
  cat("Reading crosscut locations from groove CSV files...\n")

  # Read groove data from both directories
  grooves1 <- read_grooves_csv(bullet1_dir)
  grooves2 <- read_grooves_csv(bullet2_dir)

  if (is.null(grooves1)) {
    stop(paste("No groove CSV file found in:", bullet1_dir))
  }
  if (is.null(grooves2)) {
    stop(paste("No groove CSV file found in:", bullet2_dir))
  }
  
  grooves <- rbind(grooves1, grooves2)
  
  # Check for missing crosscuts
  if (any(is.na(grooves$crosscut_y))) {
    missing <- grooves$filename[is.na(grooves$crosscut_y)]
    stop(paste("Could not find crosscut locations for:", paste(missing, collapse = ", ")))
  }
  
  # Check for missing grooves
  if (any(is.na(grooves$left_groove))) {
    missing <- grooves$filename[is.na(grooves$left_groove)]
    stop(paste("Could not find crosscut locations for:", paste(missing, collapse = ", ")))
  }
  if (any(is.na(grooves$right_groove))) {
    missing <- grooves$filename[is.na(grooves$right_groove)]
    stop(paste("Could not find crosscut locations for:", paste(missing, collapse = ", ")))
  }

  # Match crosscut locations by filename
  bullets <- bullets %>%
    dplyr::left_join(grooves, by = dplyr::join_by(filename)) %>%
    dplyr::rename(crosscut = `crosscut_y`)
  
  # Create groove column in same format as cc_locate_grooves() output
  bullets$grooves <- NA
  for (i in 1:nrow(bullets)) {
    bullets$grooves[[i]] <- list(groove = c(bullets$left_groove[i], bullets$right_groove[i]))
  }
  bullets <- bullets %>% dplyr::select(-tidyselect::any_of(c("left_groove", "right_groove")))
  
  for (i in seq_len(nrow(bullets))) {
    cat("  Bullet", bullets$bullet[i], "Land", bullets$land[i], ": crosscut =", bullets$crosscut[i], "\n")
  }

  return(bullets)
}

#' Extract Crosscut Data
#' @param bullets Data frame with x3p objects and crosscut locations
#' @param cores Number of cores for parallel processing (default 1L)
#' @returns Data frame with ccdata column added
extract_crosscut_data <- function(bullets, cores = 1L) {
  cat("Extracting crosscut data...\n")

  bullets$ccdata <- par_mapply(
    function(x3p, y) {
      res <- bulletxtrctr::x3p_crosscut(x3p = x3p, y = y, range = 1e-5)
      if (nrow(res) == 0) {
        res <- bulletxtrctr::x3p_crosscut(x3p = x3p, y = NULL, range = 1e-5)
      }
      return(res)
    },
    bullets$x3p,
    bullets$crosscut,
    cores = cores, SIMPLIFY = FALSE
  )

  return(bullets)
}

#' Get Groove Locations from groove CSV
#' @param bullets Data frame with ccdata
#' @param bullet1_dir Path to bullet 1 directory
#' @param bullet2_dir Path to bullet 2 directory
#' @param bullet1_name Name of bullet 1
#' @param bullet2_name Name of bullet 2
#' @returns Data frame with grooves column added
get_grooves_from_csv <- function(bullets, bullet1_dir, bullet2_dir, bullet1_name, bullet2_name) {
  cat("Reading groove locations from groove CSV files...\n")

  # Read groove data from both directories
  grooves1 <- read_grooves_csv(bullet1_dir)
  grooves2 <- read_grooves_csv(bullet2_dir)

  # Initialize grooves column as list
  bullets$grooves <- vector("list", nrow(bullets))

  # Match groove locations by filename
  for (i in seq_len(nrow(bullets))) {
    filename <- bullets$filename[i]
    bullet_name <- bullets$bullet[i]

    left_groove <- NA_real_
    right_groove <- NA_real_

    if (bullet_name == bullet1_name) {
      match_idx <- which(grooves1$filename == filename)
      if (length(match_idx) > 0) {
        left_groove <- grooves1$left_groove[match_idx[1]]
        right_groove <- grooves1$right_groove[match_idx[1]]
      }
    } else if (bullet_name == bullet2_name) {
      match_idx <- which(grooves2$filename == filename)
      if (length(match_idx) > 0) {
        left_groove <- grooves2$left_groove[match_idx[1]]
        right_groove <- grooves2$right_groove[match_idx[1]]
      }
    }

    # Create groove object in same format as cc_locate_grooves output
    bullets$grooves[[i]] <- list(groove = c(left_groove, right_groove))
  }

  # Check for missing grooves
  missing_grooves <- sapply(bullets$grooves, function(g) any(is.na(g$groove)))
  if (any(missing_grooves)) {
    missing <- bullets$filename[missing_grooves]
    stop(paste("Could not find groove locations for:", paste(missing, collapse = ", ")))
  }

  for (i in seq_len(nrow(bullets))) {
    cat("  Bullet", bullets$bullet[i], "Land", bullets$land[i],
        ": grooves =", bullets$grooves[[i]]$groove[1], ",", bullets$grooves[[i]]$groove[2], "\n")
  }

  return(bullets)
}

#' Extract Signals from Crosscut Data
#' @param bullets Data frame with ccdata and grooves
#' @param cores Number of cores for parallel processing (default 1L)
#' @returns Data frame with sigs column added
extract_signals <- function(bullets, cores = 1L) {
  cat("Extracting signals...\n")

  bullets$sigs <- par_mapply(
    function(ccdata, grooves) {
      bulletxtrctr::cc_get_signature(ccdata, grooves, span1 = 0.75, span2 = 0.03)
    },
    bullets$ccdata,
    bullets$grooves,
    cores = cores, SIMPLIFY = FALSE
  )

  return(bullets)
}

#' Align Signals Between All Land Pairs
#' @param bullets Data frame with signals
#' @param cores Number of cores for parallel processing (default 1L)
#' @returns List with bullets and comparisons data frames
align_signals <- function(bullets, cores = 1L) {
  cat("Aligning signals between all land pairs...\n")

  bullets$bulletland <- paste0(bullets$bullet, "-", bullets$land)
  lands <- unique(bullets$bulletland)

  comparisons <- data.frame(
    expand.grid(land1 = lands, land2 = lands),
    stringsAsFactors = FALSE
  )

  # Pre-build named lookup for O(1) access instead of O(n) scan per iteration
  sig_lookup <- setNames(
    lapply(seq_len(nrow(bullets)), function(i) bullets$sigs[[i]]$sig),
    bullets$bulletland
  )

  comparisons$aligned <- par_mapply(
    function(x, y, lookup) {
      bulletxtrctr::sig_align(lookup[[x]], lookup[[y]])
    },
    comparisons$land1,
    comparisons$land2,
    MoreArgs = list(lookup = sig_lookup),
    cores = cores, SIMPLIFY = FALSE
  )

  cat("  Created", nrow(comparisons), "land-to-land comparisons\n")

  return(list(bullets = bullets, comparisons = comparisons))
}

#' Extract All Features
#' @param comparisons Data frame with aligned signals
#' @param resolution Scan resolution
#' @param cores Number of cores for parallel processing (default 1L)
#' @returns List with comparisons and features data frames
extract_features <- function(comparisons, resolution, cores = 1L) {
  cat("Extracting features...\n")

  # Calculate CCF
  cat("  Calculating cross-correlation...\n")
  comparisons$ccf0 <- unlist(par_lapply(comparisons$aligned, function(x) bulletxtrctr::extract_feature_ccf(x$lands), cores = cores))

  # Evaluate striation marks
  cat("  Evaluating striation marks...\n")
  comparisons$striae <- par_lapply(comparisons$aligned, bulletxtrctr::sig_cms_max, span = 75, cores = cores)

  # Extract bullet/land identifiers
  comparisons$bulletA <- sapply(strsplit(as.character(comparisons$land1), "-"), "[[", 1)
  comparisons$bulletB <- sapply(strsplit(as.character(comparisons$land2), "-"), "[[", 1)
  comparisons$landA <- sapply(strsplit(as.character(comparisons$land1), "-"), "[[", 2)
  comparisons$landB <- sapply(strsplit(as.character(comparisons$land2), "-"), "[[", 2)

  # Extract all features
  cat("  Extracting all features...\n")
  comparisons$features <- par_mapply(
    bulletxtrctr::extract_features_all,
    comparisons$aligned,
    comparisons$striae,
    MoreArgs = list(resolution = resolution),
    cores = cores, SIMPLIFY = FALSE
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
  result <- tryCatch({
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
  }, error = function(e) {
    warning(paste("Phase test failed:", e$message))
    return(NULL)
  })

  return(result)
}

# ============================================================================
# MAIN COMPARISON FUNCTION
# ============================================================================

#' Compare Two Bullets Using Manual Groove Locations
#'
#' @param bullet1_dir Path to directory containing bullet 1 x3p files and a groove CSV
#' @param bullet2_dir Path to directory containing bullet 2 x3p files and a groove CSV
#' @param outfile Optional path to save results as an RDS file (default: NULL)
#' @param cores Number of cores for parallel processing (default: all but one)
#' @returns A list containing all comparison results
compare_bullets <- function(bullet1_dir, bullet2_dir, outfile = NULL,
                            cores = max(1L, parallel::detectCores() - 1L, na.rm = TRUE)) {
  
  if (file.exists(outfile)) {
    cat("Outfile already exists. Skipping comparison. \n")
    return()
  }
  
  bullet1_name <- parse_filepath(bullet1_dir)
  bullet2_name <- parse_filepath(bullet2_dir)

  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("MANUAL BULLET COMPARISON PIPELINE\n")
  cat(paste("Comparing", bullet1_name, "and", bullet2_name, "\n"))
  cat("(Using groove locations from groove CSV files)\n")
  cat(paste(rep("=", 60), collapse = ""), "\n\n")

  # Check directories exist
  if (!dir.exists(bullet1_dir)) {
    stop(paste("Bullet 1 directory not found:", bullet1_dir))
  }
  if (!dir.exists(bullet2_dir)) {
    stop(paste("Bullet 2 directory not found:", bullet2_dir))
  }

  # Check groove CSV files exist
  if (length(list.files(bullet1_dir, pattern = "groove.*\\.csv$", ignore.case = TRUE)) == 0) {
    stop(paste("No groove CSV file found in:", bullet1_dir,
               "\nRun manual_groove_selection.R first to create groove locations."))
  }
  if (length(list.files(bullet2_dir, pattern = "groove.*\\.csv$", ignore.case = TRUE)) == 0) {
    stop(paste("No groove CSV file found in:", bullet2_dir,
               "\nRun manual_groove_selection.R first to create groove locations."))
  }

  # Step 1: Read bullets
  cat("Step 1: Reading bullet data...\n")
  bullet1 <- bulletxtrctr::read_bullet(bullet1_dir)
  bullet2 <- bulletxtrctr::read_bullet(bullet2_dir)
  cat("  Bullet 1:", nrow(bullet1), "lands\n")
  cat("  Bullet 2:", nrow(bullet2), "lands\n")

  # Step 2: Preprocess bullets
  cat("\nStep 2: Preprocessing bullets...\n")
  bullet1 <- preprocess_bullet_standalone(bullet1, bullet1_name)
  bullet2 <- preprocess_bullet_standalone(bullet2, bullet2_name)

  # Combine bullets
  bullets <- rbind(bullet1, bullet2)

  # Get resolution (assuming same resolution for all)
  resolution <- x3ptools::x3p_get_scale(bullets$x3p[[1]])
  cat("  Resolution:", resolution, "microns\n")

  # Step 3: Get crosscut and groove locations from groove CSV
  cat("\nStep 3: Reading crosscut and groove locations from groove CSV...\n")
  bullets <- get_crosscuts_from_csv(bullets, bullet1_dir, bullet2_dir, bullet1_name, bullet2_name)
  
  cat("  Using", cores, "core(s) for parallel processing\n")

  # Step 4: Extract crosscut data
  cat("\nStep 4: Extracting crosscut profiles...\n")
  bullets <- extract_crosscut_data(bullets, cores = cores)

  # Step 6: Extract signals
  cat("\nStep 6: Extracting signals...\n")
  bullets <- extract_signals(bullets, cores = cores)

  # Step 7: Align signals
  cat("\nStep 7: Aligning signals...\n")
  aligned_results <- align_signals(bullets, cores = cores)
  bullets <- aligned_results$bullets
  comparisons <- aligned_results$comparisons

  # Step 8: Extract features
  cat("\nStep 8: Extracting features...\n")
  feature_results <- extract_features(comparisons, resolution, cores = cores)
  comparisons <- feature_results$comparisons
  features <- feature_results$features

  # Step 9: Calculate RF scores
  cat("\nStep 9: Calculating scores...\n")
  features <- calculate_rf_scores(features)

  # Step 10: Calculate bullet scores
  cat("\nStep 10: Calculating bullet-level scores...\n")
  bullet_scores <- calculate_bullet_scores(features)

  # Step 11: Run phase test
  cat("\nStep 11: Running phase test...\n")
  phase_test_results <- run_phase_test(features, bullet1_name, bullet2_name)

  # Print results
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("RESULTS\n")
  cat(paste("Comparing", bullet1_name, "and", bullet2_name, "\n"))
  cat(paste(rep("=", 60), collapse = ""), "\n\n")

  # Bullet-to-bullet score
  b2b_score <- bullet_scores %>%
    dplyr::filter(bulletA == bullet1_name & bulletB == bullet2_name)

  if (nrow(b2b_score) > 0) {
    cat("Bullet-to-Bullet Score:", round(b2b_score$bullet_score, 4), "\n")
  }

  # Phase test results
  if (!is.null(phase_test_results)) {
    cat("\nPhase Test Results:\n")
    cat("  Same-source estimate (estimate1):", round(phase_test_results$estimate1, 4), "\n")
    cat("  Different-source estimate (estimate2):", round(phase_test_results$estimate2, 4), "\n")
    cat("  Test statistic:", round(phase_test_results$statistic, 4), "\n")
    cat("  P-value:", format(phase_test_results$p.value, scientific = TRUE, digits = 4), "\n")
    cat("  The probability of a false positive is:", format(100*phase_test_results$p.value, scientific = FALSE, digits = 4), "\n")
  }

  cat("\n", paste(rep("=", 60), collapse = ""), "\n")


  # Prepare minimal results to reduce file size
  # Extract only essential land info (no x3p, ccdata, sigs, etc.)
  lands <- data.frame(
    bullet = bullets$bullet,
    land = bullets$land,
    filename = bullets$filename,
    crosscut = bullets$crosscut,
    left_groove = sapply(bullets$grooves, function(g) g$groove[1]),
    right_groove = sapply(bullets$grooves, function(g) g$groove[2]),
    stringsAsFactors = FALSE
  )

  # Extract just bullet scores (no nested data)
  scores <- data.frame(
    bulletA = bullet_scores$bulletA,
    bulletB = bullet_scores$bulletB,
    bullet_score = bullet_scores$bullet_score,
    stringsAsFactors = FALSE
  )

  # Prepare compact results list
  results <- list(
    lands = lands,
    features = features,
    bullet_scores = scores,
    phase_test = phase_test_results,
    bullet1_name = bullet1_name,
    bullet2_name = bullet2_name
  )

  # Save to RDS file if outfile is specified
  if (!is.null(outfile)) {
    saveRDS(results, file = outfile)
    cat("Results saved to:", outfile, "\n")
  }

  # Return all results invisibly (use results$bullets, results$features, etc. to access)
  invisible(results)
}

# ============================================================================
# RUN COMPARISON (when script is executed directly)
# ============================================================================

if (!interactive()) {
  cat("Usage: Provide bullet directories as arguments or call compare_bullets() interactively.\n")
} else {
  cat("\n=== Manual Bullet Comparison Pipeline Loaded ===\n")
  cat("\nThis script uses groove locations from groove CSV files in each bullet directory.\n")
  cat("Run manual_groove_selection.R first to create the groove CSV files.\n")
  cat("\nTo run a comparison, call:\n")
  cat('  results <- compare_bullets("path/to/bullet1", "path/to/bullet2")\n')
  cat('\nTo save results, call:\n')
  cat('  results <- compare_bullets("path/to/bullet1", "path/to/bullet2", outfile = "results.rds")\n')
}
