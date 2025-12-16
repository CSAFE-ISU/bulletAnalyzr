#!/usr/bin/env Rscript
# Analyze Manual Groove Selection Data
#
# This script provides functions to analyze the collected groove location data
# and generate summary statistics and visualizations.
#
# Usage:
#   source("analyze_groove_data.R")
#   analyze_groove_data("groove_data.csv")

library(ggplot2)
library(dplyr)

#' Analyze groove selection data
#' 
#' @param csv_file Path to the CSV file containing groove data
#' @return A list containing summary statistics and plots
analyze_groove_data <- function(csv_file) {
  
  if (!file.exists(csv_file)) {
    stop("File not found: ", csv_file)
  }
  
  # Read data
  cat("Reading data from:", csv_file, "\n")
  data <- read.csv(csv_file, stringsAsFactors = FALSE)
  
  cat("\n=== SUMMARY STATISTICS ===\n\n")
  
  # Basic counts
  cat("Total files processed:", nrow(data), "\n")
  cat("Manual adjustments:", sum(data$manual_selection), 
      sprintf("(%.1f%%)", 100 * mean(data$manual_selection)), "\n")
  cat("Automatic accepted:", sum(!data$manual_selection),
      sprintf("(%.1f%%)", 100 * mean(!data$manual_selection)), "\n\n")
  
  # Groove position statistics
  cat("Left Groove Statistics:\n")
  print(summary(data$left_groove))
  cat("\nRight Groove Statistics:\n")
  print(summary(data$right_groove))
  
  # Land width
  data$land_width <- data$right_groove - data$left_groove
  cat("\nLand Width Statistics (right - left):\n")
  print(summary(data$land_width))
  
  # Crosscut location
  cat("\nCrosscut Y Location Statistics:\n")
  print(summary(data$crosscut_y))
  
  cat("\n=== GENERATING PLOTS ===\n\n")
  
  # Plot 1: Manual vs Automatic
  p1 <- ggplot(data, aes(x = manual_selection, fill = manual_selection)) +
    geom_bar() +
    geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
    scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "gray70")) +
    labs(
      title = "Manual vs Automatic Groove Selection",
      x = "Manual Selection",
      y = "Count",
      fill = "Manual\nSelection"
    ) +
    theme_bw() +
    theme(text = element_text(size = 14))
  
  print(p1)
  
  # Plot 2: Distribution of left groove positions
  p2 <- ggplot(data, aes(x = left_groove, fill = manual_selection)) +
    geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
    scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "gray70")) +
    labs(
      title = "Distribution of Left Groove Positions",
      x = "Left Groove Position (microns)",
      y = "Count",
      fill = "Manual\nSelection"
    ) +
    theme_bw() +
    theme(text = element_text(size = 14))
  
  print(p2)
  
  # Plot 3: Distribution of right groove positions
  p3 <- ggplot(data, aes(x = right_groove, fill = manual_selection)) +
    geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
    scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "gray70")) +
    labs(
      title = "Distribution of Right Groove Positions",
      x = "Right Groove Position (microns)",
      y = "Count",
      fill = "Manual\nSelection"
    ) +
    theme_bw() +
    theme(text = element_text(size = 14))
  
  print(p3)
  
  # Plot 4: Land width distribution
  p4 <- ggplot(data, aes(x = land_width, fill = manual_selection)) +
    geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
    scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "gray70")) +
    labs(
      title = "Distribution of Land Widths",
      x = "Land Width (microns)",
      y = "Count",
      fill = "Manual\nSelection"
    ) +
    theme_bw() +
    theme(text = element_text(size = 14))
  
  print(p4)
  
  # Plot 5: Scatter plot of left vs right groove
  p5 <- ggplot(data, aes(x = left_groove, y = right_groove, color = manual_selection)) +
    geom_point(alpha = 0.6, size = 3) +
    geom_abline(slope = 1, intercept = mean(data$land_width), 
                linetype = "dashed", color = "red") +
    scale_color_manual(values = c("TRUE" = "steelblue", "FALSE" = "gray70")) +
    labs(
      title = "Left vs Right Groove Positions",
      subtitle = "Red dashed line shows mean land width",
      x = "Left Groove Position (microns)",
      y = "Right Groove Position (microns)",
      color = "Manual\nSelection"
    ) +
    theme_bw() +
    theme(text = element_text(size = 14))
  
  print(p5)
  
  # If there are manual selections, compare them to automatic
  if (sum(data$manual_selection) > 0) {
    cat("\n=== COMPARISON: MANUAL vs AUTOMATIC ===\n\n")
    
    manual_data <- data[data$manual_selection == TRUE, ]
    auto_data <- data[data$manual_selection == FALSE, ]
    
    cat("Manual selections:\n")
    cat("  Mean land width:", mean(manual_data$land_width), "\n")
    cat("  SD land width:", sd(manual_data$land_width), "\n")
    cat("  Mean left groove:", mean(manual_data$left_groove), "\n")
    cat("  Mean right groove:", mean(manual_data$right_groove), "\n\n")
    
    cat("Automatic selections:\n")
    cat("  Mean land width:", mean(auto_data$land_width), "\n")
    cat("  SD land width:", sd(auto_data$land_width), "\n")
    cat("  Mean left groove:", mean(auto_data$left_groove), "\n")
    cat("  Mean right groove:", mean(auto_data$right_groove), "\n\n")
    
    # T-test for land width difference
    if (nrow(manual_data) > 1 && nrow(auto_data) > 1) {
      t_test <- t.test(manual_data$land_width, auto_data$land_width)
      cat("T-test for land width difference:\n")
      cat("  t-statistic:", t_test$statistic, "\n")
      cat("  p-value:", t_test$p.value, "\n")
      cat("  Significant difference:", ifelse(t_test$p.value < 0.05, "YES", "NO"), "\n\n")
    }
  }
  
  # Return summary data
  invisible(list(
    data = data,
    n_total = nrow(data),
    n_manual = sum(data$manual_selection),
    pct_manual = 100 * mean(data$manual_selection),
    mean_land_width = mean(data$land_width),
    sd_land_width = sd(data$land_width)
  ))
}

#' Export summary report to text file
#' 
#' @param csv_file Path to the CSV file containing groove data
#' @param output_file Path to output text file
export_summary_report <- function(csv_file, output_file = "groove_summary.txt") {
  
  # Capture output
  sink(output_file)
  
  cat("MANUAL GROOVE SELECTION SUMMARY REPORT\n")
  cat("======================================\n")
  cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Input file:", csv_file, "\n\n")
  
  # Run analysis (this will print to the file)
  result <- analyze_groove_data(csv_file)
  
  sink()
  
  cat("Summary report saved to:", output_file, "\n")
  
  invisible(result)
}

#' Compare groove locations from multiple annotators
#' 
#' This function is useful if multiple people annotated the same files
#' to check inter-rater reliability.
#' 
#' @param csv_files Vector of CSV file paths from different annotators
#' @return Summary of agreement statistics
compare_annotators <- function(csv_files) {
  
  if (length(csv_files) < 2) {
    stop("Need at least 2 CSV files to compare")
  }
  
  cat("=== INTER-RATER RELIABILITY ===\n\n")
  
  # Read all files
  all_data <- lapply(csv_files, function(f) {
    data <- read.csv(f, stringsAsFactors = FALSE)
    data$annotator <- basename(f)
    return(data)
  })
  
  # Combine data
  combined <- do.call(rbind, all_data)
  
  # Find files that appear in all datasets
  file_counts <- table(combined$filename)
  common_files <- names(file_counts)[file_counts == length(csv_files)]
  
  cat("Files annotated by all annotators:", length(common_files), "\n")
  
  if (length(common_files) == 0) {
    cat("No common files found between annotators.\n")
    return(NULL)
  }
  
  # Filter to common files only
  common_data <- combined[combined$filename %in% common_files, ]
  
  # Calculate agreement
  for (file in common_files) {
    file_data <- common_data[common_data$filename == file, ]
    
    cat("\nFile:", file, "\n")
    cat("  Left groove range:", 
        paste(range(file_data$left_groove), collapse = " - "), "\n")
    cat("  Right groove range:", 
        paste(range(file_data$right_groove), collapse = " - "), "\n")
    cat("  Left groove SD:", sd(file_data$left_groove), "\n")
    cat("  Right groove SD:", sd(file_data$right_groove), "\n")
  }
  
  # Overall statistics
  cat("\n=== OVERALL AGREEMENT ===\n")
  cat("Mean SD of left grooves:", 
      mean(tapply(common_data$left_groove, common_data$filename, sd)), "\n")
  cat("Mean SD of right grooves:", 
      mean(tapply(common_data$right_groove, common_data$filename, sd)), "\n")
  
  invisible(common_data)
}

# Command line interface
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args) == 0) {
    cat("Usage: Rscript analyze_groove_data.R <csv_file> [output_report]\n")
    cat("\nExamples:\n")
    cat("  Rscript analyze_groove_data.R groove_data.csv\n")
    cat("  Rscript analyze_groove_data.R groove_data.csv summary_report.txt\n")
    quit(status = 1)
  }
  
  csv_file <- args[1]
  
  if (length(args) >= 2) {
    output_file <- args[2]
    export_summary_report(csv_file, output_file)
  } else {
    analyze_groove_data(csv_file)
  }
}

cat("\n=== Groove Data Analysis Tool Loaded ===\n")
cat("Functions available:\n")
cat("  analyze_groove_data(csv_file)               - Analyze and plot groove data\n")
cat("  export_summary_report(csv_file, output)     - Save analysis to text file\n")
cat("  compare_annotators(csv_files)               - Compare multiple annotators\n")
cat("\nExample usage:\n")
cat('  analyze_groove_data("groove_data.csv")\n')
cat('  export_summary_report("groove_data.csv", "summary.txt")\n')
cat('  compare_annotators(c("person1.csv", "person2.csv"))\n')
