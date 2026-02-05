#!/usr/bin/env Rscript
# Align Two Lands and Calculate Features
#
# This script aligns two bullet land scans and calculates comparison features.
# It reuses functions from view-pipeline.R and manual-bullet-comparison-pipeline.R.
#
# Usage:
#   source("docs/developers/align-two-lands.R")
#   result <- align_two_lands(land1_path, land2_path)
#   result <- align_two_lands(land1_path, land2_path, grooves_csv1, grooves_csv2)

library(x3ptools)
library(bulletxtrctr)
library(dplyr)
library(tidyr)

# Source helper functions
source("docs/developers/comparisons/comparison-utils.R")
source("docs/developers/view-pipeline.R")


# Helper Functions --------------------------------------------------------

#' Get Crosscut and Grooves for a Single Land
#'
#' Reads an x3p file and extracts crosscut data and groove locations.
#' Uses grooves_csv if provided, otherwise auto-detects.
#'
#' @param filepath Path to an x3p file
#' @param grooves_csv Optional path to a CSV file with groove locations
#' @returns A list with x3p, ccdata, grooves, and signal
get_land_data <- function(filepath, grooves_csv = NULL) {
  cat("Processing:", basename(filepath), "\n")

  # Read x3p
  cat("  Reading x3p file...\n")
  x3p <- x3ptools::read_x3p(filepath)

  # Convert to microns if needed
  x3p <- cond_x3p_m_to_mum(x3p)

  # Look up CSV row if grooves_csv is provided
  csv_row <- NULL
  if (!is.null(grooves_csv) && file.exists(grooves_csv)) {
    csv_data <- read.csv(grooves_csv, stringsAsFactors = FALSE)
    match_idx <- which(csv_data$filename == basename(filepath))
    if (length(match_idx) > 0) {
      csv_row <- csv_data[match_idx[1], ]
      cat("  Using groove locations from CSV\n")
    } else {
      cat("  Warning: No matching row in CSV, using auto-detection\n")
    }
  }

  # Get crosscut location
  cat("  Finding crosscut...\n")
  if (!is.null(csv_row)) {
    crosscut_y <- csv_row$crosscut_y
  } else {
    crosscut_y <- get_optimal_crosscut(x3p)
  }
  cat("  Crosscut Y:", crosscut_y, "\n")

  # Extract crosscut data
  ccdata <- bulletxtrctr::x3p_crosscut(x3p, y = crosscut_y, range = 1e-5)
  ccdata$x <- ccdata$x - min(ccdata$x, na.rm = TRUE)

  # Get groove locations
  cat("  Locating grooves...\n")
  if (!is.null(csv_row)) {
    grooves <- list(groove = c(csv_row$left_groove, csv_row$right_groove))
  } else {
    grooves <- bulletxtrctr::cc_locate_grooves(ccdata, method = "middle", adjust = 30, return_plot = FALSE)
  }
  cat("  Grooves: left =", grooves$groove[1], ", right =", grooves$groove[2], "\n")

  # Extract signal
  cat("  Extracting signal...\n")
  signal <- bulletxtrctr::cc_get_signature(ccdata, grooves, span1 = 0.75, span2 = 0.03)

  return(list(
    filepath = filepath,
    x3p = x3p,
    ccdata = ccdata,
    grooves = grooves,
    signal = signal
  ))
}


# Main Function -----------------------------------------------------------

#' Align Two Lands and Calculate Features
#'
#' @param land1_path Path to first land x3p file
#' @param land2_path Path to second land x3p file
#' @param grooves_csv1 Optional path to CSV with groove locations for land 1
#' @param grooves_csv2 Optional path to CSV with groove locations for land 2
#' @param resolution Scan resolution in microns (default 1.5625)
#' @returns A list with alignment results, features, and land data
#'
#' @examples
#' result <- align_two_lands(
#'   "path/to/Barrel_1-Bullet_1-Land_1.x3p",
#'   "path/to/Barrel_2-Bullet_1-Land_1.x3p"
#' )
#' result$features # View features
#' result$aligned # View aligned signals
align_two_lands <- function(land1_path, land2_path,
                            grooves_csv1 = NULL, grooves_csv2 = NULL,
                            resolution = 1.5625) {
  cat("\n== Aligning Two Lands ==\n\n")

  # Process both lands
  land1 <- get_land_data(land1_path, grooves_csv1)
  cat("\n")
  land2 <- get_land_data(land2_path, grooves_csv2)
  cat("\n")

  # Align signals
  cat("Aligning signals...\n")
  aligned <- bulletxtrctr::sig_align(land1$signal$sig, land2$signal$sig)
  cat("  Alignment complete\n\n")

  # Calculate features
  cat("Calculating features...\n")

  # CCF
  cat("  Cross-correlation...\n")
  ccf <- bulletxtrctr::extract_feature_ccf(aligned$lands)

  # Striation marks
  cat("  Striation marks...\n")
  striae <- bulletxtrctr::sig_cms_max(aligned, span = 75)

  # All features
  cat("  Extracting all features...\n")
  features <- bulletxtrctr::extract_features_all(aligned, striae, resolution = resolution)

  # Add CCF to features
  features$ccf <- ccf

  # Add identifiers
  features$land1 <- basename(land1_path)
  features$land2 <- basename(land2_path)

  cat("\n== Done ==\n\n")

  # Print summary
  cat("Feature Summary:\n")
  cat("  CCF:", round(ccf, 4), "\n")
  cat("  CMS per mm:", round(features$cms_per_mm, 2), "\n")
  cat("  Matches per mm:", round(features$matches_per_mm, 2), "\n")
  cat("  Mismatches per mm:", round(features$mismatches_per_mm, 2), "\n")
  cat("  Non-CMS per mm:", round(features$non_cms_per_mm, 2), "\n")
  cat("  Sum peaks:", features$sum_peaks, "\n")

  # Plot the two signals on the same axes
  cat("\nPlotting signals...\n")
  df <- data.frame(
    x = seq_along(aligned$lands$sig1),
    sig1 = aligned$lands$sig1,
    sig2 = aligned$lands$sig2
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(x = x)) +
    ggplot2::geom_line(ggplot2::aes(y = sig1), color = "blue", alpha = 0.7) +
    ggplot2::geom_line(ggplot2::aes(y = sig2), color = "red", alpha = 0.7) +
    ggplot2::labs(
      title = "Aligned Signals",
      subtitle = paste0(
        "Land 1 (blue): ", basename(land1_path),
        "\nLand 2 (red): ", basename(land2_path),
        "\nCCF: ", round(ccf, 4)
      ),
      x = "Position",
      y = "Height (microns)"
    ) +
    ggplot2::theme_bw()
  print(p)

  return(list(
    land1 = land1,
    land2 = land2,
    aligned = aligned,
    striae = striae,
    features = features
  ))
}


#' Plot Aligned Signals
#'
#' @param result Result from align_two_lands()
#' @returns A ggplot object
plot_aligned_signals <- function(result) {
  aligned <- result$aligned

  df <- data.frame(
    x = seq_along(aligned$lands$sig1),
    sig1 = aligned$lands$sig1,
    sig2 = aligned$lands$sig2
  )

  ggplot2::ggplot(df, ggplot2::aes(x = x)) +
    ggplot2::geom_line(ggplot2::aes(y = sig1), color = "blue", alpha = 0.7) +
    ggplot2::geom_line(ggplot2::aes(y = sig2), color = "red", alpha = 0.7) +
    ggplot2::labs(
      title = "Aligned Signals",
      subtitle = paste(
        "Land 1 (blue):", basename(result$land1$filepath),
        "\nLand 2 (red):", basename(result$land2$filepath)
      ),
      x = "Position",
      y = "Height (microns)"
    ) +
    ggplot2::theme_bw()
}


# Bullet-Level Functions (All 6 Lands) ------------------------------------

#' Align All Land Pairs for a Bullet
#'
#' Runs align_two_lands() on all 6 choose 2 = 15 pairs of lands for a bullet.
#'
#' @param bullet_dir Path to a bullet directory containing 6 land x3p files
#' @param grooves_csv Optional path to CSV with groove locations for all lands
#' @param resolution Scan resolution in microns (default 1.5625)
#' @param verbose If TRUE, print progress messages (default TRUE)
#' @returns A list with land_data (processed lands) and comparisons (alignment results)
#'
#' @examples
#' results <- align_all_land_pairs("path/to/Barrel_1/Bullet_1")
#' results <- align_all_land_pairs("path/to/Barrel_1/Bullet_1", grooves_csv = "path/to/grooves.csv")
#' plot_alignment_matrix(results)
align_all_land_pairs <- function(bullet_dir, grooves_csv = NULL, resolution = 1.5625, verbose = TRUE) {
  # Find all x3p files in the bullet directory
  land_files <- list.files(bullet_dir, pattern = "\\.x3p$", full.names = TRUE)

  if (length(land_files) != 6) {
    warning("Expected 6 land files, found ", length(land_files))
  }

  # Sort files to ensure consistent ordering

  land_files <- sort(land_files)

  if (verbose) {
    cat("\n== Aligning All Land Pairs ==\n")
    cat("Bullet directory:", bullet_dir, "\n")
    cat("Found", length(land_files), "land files\n\n")
  }

  # Process all lands first
  if (verbose) cat("Processing all lands...\n")
  land_data <- list()
  for (i in seq_along(land_files)) {
    if (verbose) cat("\n[Land ", i, "/", length(land_files), "]\n", sep = "")
    land_data[[i]] <- get_land_data(land_files[i], grooves_csv)
  }
  names(land_data) <- paste0("land", seq_along(land_files))

  # Generate all pairs (upper triangle only: i < j)
  n_lands <- length(land_files)
  pairs <- combn(n_lands, 2)

  if (verbose) cat("\n\nAligning", ncol(pairs), "land pairs...\n")

  # Align all pairs
  comparisons <- list()
  for (k in seq_len(ncol(pairs))) {
    i <- pairs[1, k]
    j <- pairs[2, k]
    pair_name <- paste0(i, "_", j)

    if (verbose) cat("\n[Pair ", k, "/", ncol(pairs), "] Land ", i, " vs Land ", j, "\n", sep = "")

    # Align signals
    aligned <- bulletxtrctr::sig_align(land_data[[i]]$signal$sig, land_data[[j]]$signal$sig)

    # Calculate CCF
    ccf <- bulletxtrctr::extract_feature_ccf(aligned$lands)

    # Calculate striation marks
    striae <- bulletxtrctr::sig_cms_max(aligned, span = 75)

    # Extract all features
    features <- bulletxtrctr::extract_features_all(aligned, striae, resolution = resolution)
    features$ccf <- ccf
    features$land1 <- i
    features$land2 <- j

    comparisons[[pair_name]] <- list(
      land1_idx = i,
      land2_idx = j,
      aligned = aligned,
      ccf = ccf,
      striae = striae,
      features = features
    )

    if (verbose) cat("  CCF:", round(ccf, 4), "\n")
  }

  if (verbose) cat("\n== Done ==\n\n")

  return(list(
    bullet_dir = bullet_dir,
    land_files = land_files,
    land_data = land_data,
    comparisons = comparisons,
    n_lands = n_lands
  ))
}


# Default color palette for 6 lands
LAND_COLORS <- c(
  "#E41A1C", # Land 1 - red

  "#377EB8", # Land 2 - blue
  "#4DAF4A", # Land 3 - green
  "#984EA3", # Land 4 - purple
  "#FF7F00", # Land 5 - orange
  "#A65628" # Land 6 - brown
)


#' Create a Single Alignment Plot (minimal, for grid use)
#'
#' @param aligned Aligned signal data from sig_align()
#' @param ccf CCF value
#' @param land1_idx Land 1 index
#' @param land2_idx Land 2 index
#' @param colors Color palette for lands (length 6)
#' @param highlight If TRUE, make the title bold and red (default FALSE)
#' @returns A ggplot object
#' @keywords internal
make_alignment_plot <- function(aligned, ccf, land1_idx, land2_idx,
                                colors = LAND_COLORS, highlight = FALSE) {
  df <- data.frame(
    x = seq_along(aligned$lands$sig1),
    sig1 = aligned$lands$sig1,
    sig2 = aligned$lands$sig2
  )

  color1 <- colors[land1_idx]
  color2 <- colors[land2_idx]

  title_color <- if (highlight) "red" else "black"
  title_face <- if (highlight) "bold" else "plain"

  ggplot2::ggplot(df, ggplot2::aes(x = x)) +
    ggplot2::geom_line(ggplot2::aes(y = sig1), color = color1, alpha = 0.7, linewidth = 0.3) +
    ggplot2::geom_line(ggplot2::aes(y = sig2), color = color2, alpha = 0.7, linewidth = 0.3) +
    ggplot2::labs(title = paste0("L", land1_idx, " vs L", land2_idx, " (", round(ccf, 3), ")")) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 8, hjust = 0.5, color = title_color, face = title_face),
      plot.margin = ggplot2::margin(2, 2, 2, 2)
    )
}


#' Create an Empty Plot (for diagonal and lower triangle)
#'
#' @returns A ggplot object
#' @keywords internal
make_empty_plot <- function() {
  ggplot2::ggplot() +
    ggplot2::theme_void()
}


#' Create a Color Legend Plot for Lands
#'
#' @param n Number of lands
#' @param colors Color palette for lands
#' @returns A ggplot object showing the legend
#' @keywords internal
make_land_legend <- function(n, colors) {
  legend_df <- data.frame(
    land = factor(paste0("Land ", seq_len(n)), levels = paste0("Land ", seq_len(n))),
    x = seq_len(n),
    y = 1
  )

  ggplot2::ggplot(legend_df, ggplot2::aes(x = x, y = y, color = land)) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_color_manual(values = colors[seq_len(n)], name = NULL) +
    ggplot2::guides(color = ggplot2::guide_legend(nrow = 1)) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 10)
    )
}


#' Plot Alignment Matrix (6x6 grid)
#'
#' Creates a 6x6 matrix plot of aligned signals where the diagonal and below
#' are empty, and the upper triangle shows the aligned signal comparisons.
#' Each land has a consistent color across all plots.
#'
#' @param results Result from align_all_land_pairs()
#' @param title Optional title for the plot
#' @param subtitle Optional subtitle for the plot
#' @param colors Color palette for lands (default LAND_COLORS)
#' @param show_legend If TRUE, add a color legend (default TRUE)
#' @returns A combined ggplot object (using patchwork)
#'
#' @examples
#' results <- align_all_land_pairs("path/to/Barrel_1/Bullet_1")
#' plot_alignment_matrix(results)
#' plot_alignment_matrix(results, title = "Barrel 1 Bullet 1 - Land Comparisons")
plot_alignment_matrix <- function(results, title = NULL, subtitle = NULL,
                                  colors = LAND_COLORS, show_legend = TRUE) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Package 'patchwork' is required for this function. Install with: install.packages('patchwork')")
  }

  n <- results$n_lands
  comparisons <- results$comparisons

  # Find the pair with the highest CCF
  best_pair <- names(which.max(sapply(comparisons, function(x) x$ccf)))

  # Create a list to hold all plots in row-major order
  plot_list <- list()

  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      idx <- (i - 1) * n + j

      if (j <= i) {
        # Diagonal and lower triangle: empty plot
        plot_list[[idx]] <- make_empty_plot()
      } else {
        # Upper triangle: alignment plot
        pair_name <- paste0(i, "_", j)
        comp <- comparisons[[pair_name]]
        plot_list[[idx]] <- make_alignment_plot(
          comp$aligned, comp$ccf, comp$land1_idx, comp$land2_idx,
          colors = colors, highlight = (pair_name == best_pair)
        )
      }
    }
  }

  # Combine plots using patchwork
  grid <- patchwork::wrap_plots(plot_list, ncol = n, nrow = n)

  # Add legend if requested
  if (show_legend) {
    legend_plot <- make_land_legend(n, colors)
    # Extract just the legend using cowplot if available, otherwise use the full plot
    if (requireNamespace("cowplot", quietly = TRUE)) {
      legend_grob <- cowplot::get_legend(legend_plot)
      legend_only <- cowplot::ggdraw(legend_grob)
      combined <- grid / legend_only + patchwork::plot_layout(heights = c(10, 1))
    } else {
      # Fallback: use the legend plot directly (includes empty plot area)
      combined <- grid / legend_plot + patchwork::plot_layout(heights = c(10, 1))
    }
  } else {
    combined <- grid
  }

  # Add title and subtitle
  combined <- combined + patchwork::plot_annotation(
    title = title,
    subtitle = subtitle
  )

  print(combined)
  return(combined)
}


#' Get Features Data Frame from All Comparisons
#'
#' Extracts features from all comparisons into a single data frame.
#'
#' @param results Result from align_all_land_pairs()
#' @returns A data frame with features for all land pairs
get_all_features <- function(results) {
  features_list <- lapply(results$comparisons, function(x) x$features)
  do.call(rbind, features_list)
}
