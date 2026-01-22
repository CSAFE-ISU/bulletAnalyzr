#!/usr/bin/env Rscript
# Bullet Comparison Pipeline Script
#
# This script compares two bullets using the same workflow as bulletAnalyzrApp()
# but with automatic crosscut and groove locations (no manual intervention).
#
# Usage:
#   Rscript bullet_comparison_pipeline.R
#
# Or interactively in R:
#   source("bullet_comparison_pipeline.R")
#   results <- compare_bullets("examples/Hamby-44/Barrel 1/Bullet 1", "examples/Hamby-44/Barrel 1/Bullet 2")

library(x3ptools)
library(bulletxtrctr)
library(randomForest)
library(dplyr)
library(tidyr)


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

  return(bullet)
}

#' Get Default Crosscut Locations
#' @param bullets A data frame containing bullet data with x3p objects
#' @param ylimits Y-axis limits for optimization
#' @returns Data frame with crosscut column added
get_default_crosscuts <- function(bullets, ylimits = c(150, NA)) {
  cat("Finding optimal crosscut locations...\n")

  bullets$crosscut <- sapply(bullets$x3p, bulletxtrctr::x3p_crosscut_optimize, ylimits = ylimits)

  # Handle NA values by lowering minccf threshold
 if (any(is.na(bullets$crosscut))) {
    missing_idx <- which(is.na(bullets$crosscut))

    for (i in missing_idx) {
      current_minccf <- 0.85

      while (current_minccf >= 0.6) {
        cc <- bulletxtrctr::x3p_crosscut_optimize(
          x3p = bullets$x3p[[i]],
          ylimits = ylimits,
          minccf = current_minccf
        )

        if (is.na(cc) && current_minccf == 0.6) {
          stop(paste("Could not find stable crosscut for bullet", bullets$bullet[i], "land", bullets$land[i]))
        } else if (is.na(cc)) {
          current_minccf <- current_minccf - 0.05
        } else {
          bullets$crosscut[i] <- cc
          cat("  Found crosscut for land", bullets$land[i], "with minccf =", current_minccf, "\n")
          break
        }
      }
    }
  }

  for (i in seq_len(nrow(bullets))) {
    cat("  Bullet", bullets$bullet[i], "Land", bullets$land[i], ": crosscut =", bullets$crosscut[i], "\n")
  }

  return(bullets)
}

#' Extract Crosscut Data
#' @param bullets Data frame with x3p objects and crosscut locations
#' @returns Data frame with ccdata column added
extract_crosscut_data <- function(bullets) {
  cat("Extracting crosscut data...\n")

  bullets$ccdata <- mapply(
    function(x3p, y) {
      res <- bulletxtrctr::x3p_crosscut(x3p = x3p, y = y, range = 1e-5)
      if (nrow(res) == 0) {
        res <- bulletxtrctr::x3p_crosscut(x3p = x3p, y = NULL, range = 1e-5)
      }
      return(res)
    },
    bullets$x3p,
    bullets$crosscut,
    SIMPLIFY = FALSE
  )

  return(bullets)
}

#' Locate Grooves Automatically
#' @param bullets Data frame with ccdata
#' @returns Data frame with grooves column added
locate_grooves <- function(bullets) {
  cat("Locating groove positions...\n")

  bullets$grooves <- lapply(
    bullets$ccdata,
    function(x) bulletxtrctr::cc_locate_grooves(x, method = "middle", adjust = 30, return_plot = FALSE)
  )

  for (i in seq_len(nrow(bullets))) {
    cat("  Bullet", bullets$bullet[i], "Land", bullets$land[i],
        ": grooves =", bullets$grooves[[i]]$groove[1], ",", bullets$grooves[[i]]$groove[2], "\n")
  }

  return(bullets)
}

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
#' @param features Data frame with RF scores
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

  # Run phase test
  result <- tryCatch({
    df <- data.frame(
      land1 = comparison_data$landA,
      land2 = comparison_data$landB,
      score = comparison_data$rfscore
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

    list(
      same_source_estimate = est1,
      different_source_estimate = est2,
      test_statistic = test_statistic,
      p_value = p_value,
      sigma = sigma_0,
      n_phases = n
    )
  }, error = function(e) {
    warning(paste("Phase test failed:", e$message))
    return(NULL)
  })

  return(result)
}

# ============================================================================
# MAIN COMPARISON FUNCTION
# ============================================================================

#' Compare Two Bullets
#'
#' @param bullet1_dir Path to directory containing bullet 1 x3p files
#' @param bullet2_dir Path to directory containing bullet 2 x3p files
#' @param bullet1_name Optional name for bullet 1 (default: "Bullet1")
#' @param bullet2_name Optional name for bullet 2 (default: "Bullet2")
#' @param outfile Optional path to save results as an RDS file (default: NULL)
#' @returns A list containing all comparison results
compare_bullets <- function(bullet1_dir, bullet2_dir,
                            bullet1_name = "Bullet1",
                            bullet2_name = "Bullet2",
                            outfile = NULL) {

  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("BULLET COMPARISON PIPELINE\n")
  cat(paste(rep("=", 60), collapse = ""), "\n\n")

  # Check directories exist
  if (!dir.exists(bullet1_dir)) {
    stop(paste("Bullet 1 directory not found:", bullet1_dir))
  }
  if (!dir.exists(bullet2_dir)) {
    stop(paste("Bullet 2 directory not found:", bullet2_dir))
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

  # Step 3: Get crosscut locations
  cat("\nStep 3: Finding crosscut locations...\n")
  bullets <- get_default_crosscuts(bullets)

  # Step 4: Extract crosscut data
  cat("\nStep 4: Extracting crosscut profiles...\n")
  bullets <- extract_crosscut_data(bullets)

  # Step 5: Locate grooves
  cat("\nStep 5: Locating grooves...\n")
  bullets <- locate_grooves(bullets)

  # Step 6: Extract signals
  cat("\nStep 6: Extracting signals...\n")
  bullets <- extract_signals(bullets)

  # Step 7: Align signals
  cat("\nStep 7: Aligning signals...\n")
  aligned_results <- align_signals(bullets)
  bullets <- aligned_results$bullets
  comparisons <- aligned_results$comparisons

  # Step 8: Extract features
  cat("\nStep 8: Extracting features...\n")
  feature_results <- extract_features(comparisons, resolution)
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
    cat("  Same-source estimate:", round(phase_test_results$same_source_estimate, 4), "\n")
    cat("  Different-source estimate:", round(phase_test_results$different_source_estimate, 4), "\n")
    cat("  Test statistic:", round(phase_test_results$test_statistic, 4), "\n")
    cat("  P-value:", format(phase_test_results$p_value, scientific = TRUE, digits = 4), "\n")

    if (phase_test_results$p_value < 0.05) {
      cat("\n  Conclusion: Evidence suggests bullets are from the SAME SOURCE (p < 0.05)\n")
    } else {
      cat("\n  Conclusion: Insufficient evidence that bullets are from the same source (p >= 0.05)\n")
    }
  }

  cat("\n", paste(rep("=", 60), collapse = ""), "\n")

  # Prepare results list
  results <- list(
    bullets = bullets,
    comparisons = comparisons,
    features = features,
    bullet_scores = bullet_scores,
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
  results <- compare_bullets(bullet1_dir, bullet2_dir)
} else {
  cat("\n=== Bullet Comparison Pipeline Loaded ===\n")
  cat("\nTo run a comparison, call:\n")
  cat('  results <- compare_bullets("examples/Hamby-44/Barrel 1/Bullet 1", "examples/Hamby-44/Barrel 1/Bullet 2")\n')
  cat('\nTo save results, call:\n')
  cat('  results <- compare_bullets("examples/Hamby-44/Barrel 1/Bullet 1", "examples/Hamby-44/Barrel 1/Bullet 2", outfile = "results.rds")\n')
}
