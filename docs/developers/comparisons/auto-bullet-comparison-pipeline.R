#!/usr/bin/env Rscript
# Bullet Comparison Pipeline Script
#
# This script compares two bullets using the same workflow as bulletAnalyzrApp()
# but with automatic crosscut and groove locations (no manual intervention).
#
# Usage: Rscript auto-bullet-comparison-pipeline.R
#
# Or interactively in R:
#   source("docs/developers/comparisons/auto-bullet-comparison-pipeline.R")
#   results <- compare_bullets_auto("examples/Hamby-44/Barrel 1/Bullet 1",
#                                   "examples/Hamby-44/Barrel 1/Bullet 2")

library(x3ptools)
library(bulletxtrctr)
library(randomForest)
library(dplyr)
library(tidyr)

source("docs/developers/comparisons/comparison-utils.R")

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
    cat(
      "  Bullet", bullets$bullet[i], "Land", bullets$land[i],
      ": grooves =", bullets$grooves[[i]]$groove[1], ",", bullets$grooves[[i]]$groove[2], "\n"
    )
  }

  return(bullets)
}

# Main Automatic Comparison Function --------------------------------------

#' Compare Two Bullets
#'
#' @param bullet1_dir Path to directory containing bullet 1 x3p files
#' @param bullet2_dir Path to directory containing bullet 2 x3p files
#' @param bullet1_name Optional name for bullet 1 (default: "Bullet1")
#' @param bullet2_name Optional name for bullet 2 (default: "Bullet2")
#' @param outfile Optional path to save results as an RDS file (default: NULL)
#' @returns A list containing all comparison results
compare_bullets_auto <- function(bullet1_dir, bullet2_dir,
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
    cat("  Same-source estimate (estimate1):", round(phase_test_results$estimate1, 4), "\n")
    cat("  Different-source estimate (estimate2):", round(phase_test_results$estimate2, 4), "\n")
    cat("  Test statistic:", round(phase_test_results$statistic, 4), "\n")
    cat("  P-value:", format(phase_test_results$p.value, scientific = TRUE, digits = 4), "\n")

    if (phase_test_results$p.value < 0.05) {
      cat("\n  Conclusion: Evidence suggests bullets are from the SAME SOURCE (p < 0.05)\n")
    } else {
      cat("\n  Conclusion: Insufficient evidence that bullets are from the same source (p >= 0.05)\n")
    }
  }

  cat("\n", paste(rep("=", 60), collapse = ""), "\n")

  # Prepare minimal results to reduce file size
  # Extract only essential land info (no x3p, ccdata, sigs, etc.)
  lands <- data.frame(
    bullet = bullets$bullet,
    land = bullets$land,
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

# Run Comparison (when script is executed directly) -----------------------

if (!interactive()) {
  results <- compare_bullets_auto(bullet1_dir, bullet2_dir)
} else {
  cat("\n=== Bullet Comparison Pipeline Loaded ===\n")
  cat("\nTo run a comparison, call:\n")
  cat('  results <- compare_bullets_auto("examples/Hamby-44/Barrel 1/Bullet 1", "examples/Hamby-44/Barrel 1/Bullet 2")\n')
  cat("\nTo save results, call:\n")
  cat('  results <- compare_bullets_auto("examples/Hamby-44/Barrel 1/Bullet 1", "examples/Hamby-44/Barrel 1/Bullet 2", outfile = "results.rds")\n')
}
