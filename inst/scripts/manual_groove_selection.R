#!/usr/bin/env Rscript
# Manual Groove Selection Tool
# 
# This script allows manual selection of groove locations from x3p bullet scan files
# and saves the results to a CSV file for later analysis.
#
# Usage:
#   Rscript manual_groove_selection.R <path_to_x3p_file> [output_csv]
#   
# Or interactively in R:
#   source("manual_groove_selection.R")
#   process_file("path/to/file.x3p", "groove_data.csv")

library(x3ptools)
library(bulletxtrctr)
library(ggplot2)

#' Process a single x3p file for manual groove selection
#' 
#' @param x3p_path Path to the x3p file
#' @param output_csv Path to output CSV file (default: "groove_locations.csv")
#' @param crosscut_y Optional crosscut location. If NULL, automatically determined
#' @return Data frame with the groove locations
process_file <- function(x3p_path, output_csv = "groove_locations.csv", crosscut_y = NULL) {
  
  cat("\n=== Processing:", basename(x3p_path), "===\n")
  
  # Read the x3p file
  x3p <- x3p_read(x3p_path)
  
  # Check orientation and ensure LONG axis is in X direction
  # x3p_crosscut extracts along the X axis, so we want X = circumference (long)
  # If the scan has Y longer than X, we need to transpose it
  x_length <- x3p$header.info$sizeX * x3p$header.info$incrementX
  y_length <- x3p$header.info$sizeY * x3p$header.info$incrementY
  
  cat("Scan dimensions: X =", x_length * 1e6, "microns, Y =", y_length * 1e6, "microns\n")
  
  if (y_length > x_length) {
    cat("Rotating scan 90 degrees to put long axis in X direction\n")
    x3p <- x3p_transpose(x3p)
    # Update lengths after transpose
    x_length <- x3p$header.info$sizeX * x3p$header.info$incrementX
    y_length <- x3p$header.info$sizeY * x3p$header.info$incrementY
    cat("After rotation: X =", x_length * 1e6, "microns, Y =", y_length * 1e6, "microns\n")
  } else {
    cat("Orientation correct: long axis already in X direction\n")
  }

  # Convert from meters to micrometers if needed (same as bulletAnalyzrApp)
  scale <- x3ptools::x3p_get_scale(x3p)
  if (scale < 0.1) {
    cat("Converting from meters to micrometers...\n")
    x3p <- x3ptools::x3p_m_to_mum(x3p)
  }

  # Get crosscut location if not specified
  if (is.null(crosscut_y)) {
    # Use x3p_crosscut_optimize to find optimal crosscut location (same as bulletAnalyzrApp)
    cat("Finding optimal crosscut location...\n")
    crosscut_y <- bulletxtrctr::x3p_crosscut_optimize(x3p, ylimits = c(150, NA))

    # Fallback with progressively lower minccf if optimization fails
    if (is.na(crosscut_y)) {
      current_minccf <- 0.85
      while (current_minccf >= 0.6) {
        crosscut_y <- bulletxtrctr::x3p_crosscut_optimize(
          x3p = x3p,
          ylimits = c(150, NA),
          minccf = current_minccf
        )
        if (is.na(crosscut_y) && current_minccf == 0.6) {
          # Fall back to middle of scan if optimization fails completely
          y_min <- 0
          y_max <- (nrow(x3p$surface.matrix) - 1) * x3p$header.info$incrementY
          crosscut_y <- (y_min + y_max) / 2
          cat("Warning: Could not find optimal crosscut, falling back to middle of scan\n")
          break
        } else if (is.na(crosscut_y)) {
          current_minccf <- current_minccf - 0.05
          next
        } else {
          cat("Found stable region with minccf =", current_minccf, "\n")
          break
        }
      }
    }

    cat("Using optimized crosscut at y =", crosscut_y, "microns\n")
  }
  
  # Extract crosscut data as a 1D profile
  # The range parameter controls the Y-range (thickness of the slice to average)
  # Use a small range for a thin horizontal slice
  y_increment <- x3p$header.info$incrementY
  thin_range <- y_increment * 5  # Average 5 rows for a thin but stable slice
  
  ccdata <- x3p_crosscut(x3p, y = crosscut_y, range = thin_range)
  
  if (nrow(ccdata) == 0) {
    cat("Warning: Empty crosscut data, trying with larger range\n")
    ccdata <- x3p_crosscut(x3p, y = crosscut_y, range = thin_range * 10)
  }
  
  # Ensure x coordinates start from 0 and span the full width
  ccdata$x <- ccdata$x - min(ccdata$x, na.rm = TRUE)
  
  # Get automatic groove locations as starting point
  grooves_auto <- cc_locate_grooves(ccdata, method = "middle", adjust = 30, return_plot = FALSE)
  cat("Automatic groove locations:", grooves_auto$groove[1], ",", grooves_auto$groove[2], "\n")
  
  # Plot the crosscut profile
  p <- ggplot(ccdata, aes(x = x, y = value)) +
    geom_line() +
    geom_vline(xintercept = grooves_auto$groove[1], color = "red", linetype = "dashed", linewidth = 1) +
    geom_vline(xintercept = grooves_auto$groove[2], color = "red", linetype = "dashed", linewidth = 1) +
    labs(
      title = paste("Crosscut Profile -", basename(x3p_path)),
      subtitle = "Red dashed lines show automatic groove locations",
      x = "Position (microns)",
      y = "Height (microns)"
    ) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    theme_bw() +
    theme(text = element_text(size = 14), axis.text.x=element_text(angle = 30, hjust = 1))
  
  print(p)
  
  # Interactive input for groove locations
  cat("\n--- Manual Groove Selection ---\n")
  cat("Automatic left groove:", grooves_auto$groove[1], "\n")
  cat("Automatic right groove:", grooves_auto$groove[2], "\n")
  cat("\nOptions:\n")
  cat("  1. Press ENTER to accept automatic grooves\n")
  cat("  2. Enter custom values as: left,right (e.g., 500,2500)\n")
  cat("  3. Enter 's' to skip this file\n")
  
  user_input <- readline(prompt = "Your choice: ")
  
  if (tolower(user_input) == "s") {
    cat("Skipping file.\n")
    return(NULL)
  }
  
  if (user_input == "") {
    # Use automatic values
    left_groove <- grooves_auto$groove[1]
    right_groove <- grooves_auto$groove[2]
    manual_flag <- FALSE
  } else {
    # Parse manual input with confirmation loop
    confirmed <- FALSE
    while (!confirmed) {
      parts <- strsplit(user_input, ",")[[1]]
      if (length(parts) != 2) {
        cat("Invalid input format. Please enter values as: left,right (e.g., 500,2500)\n")
        user_input <- readline(prompt = "Enter groove locations: ")
        next
      } else {
        left_groove <- as.numeric(trimws(parts[1]))
        right_groove <- as.numeric(trimws(parts[2]))
        manual_flag <- TRUE

        # Show updated plot with proposed manual grooves in blue
        p_manual <- p +
          geom_vline(xintercept = left_groove, color = "blue", linetype = "solid", linewidth = 1.2) +
          geom_vline(xintercept = right_groove, color = "blue", linetype = "solid", linewidth = 1.2) +
          labs(subtitle = "Red dashed = auto, Blue solid = proposed manual")
        print(p_manual)

        # Ask for confirmation
        cat("\n--- Confirm Groove Locations ---\n")
        cat("Proposed left groove:", left_groove, "\n")
        cat("Proposed right groove:", right_groove, "\n")
        cat("\nOptions:\n")
        cat("  1. Press ENTER to confirm these groove locations\n")
        cat("  2. Enter new values as: left,right (e.g., 500,2500)\n")

        confirm_input <- readline(prompt = "Your choice: ")

        if (confirm_input == "") {
          # User confirmed the locations
          confirmed <- TRUE
          cat("Groove locations confirmed.\n")
        } else {
          # User wants to try new values
          user_input <- confirm_input
        }
      }
    }
  }
  
  # Prepare data for output
  result <- data.frame(
    filename = basename(x3p_path),
    filepath = x3p_path,
    left_groove = left_groove,
    right_groove = right_groove,
    crosscut_y = crosscut_y,
    manual_selection = manual_flag,
    timestamp = Sys.time(),
    stringsAsFactors = FALSE
  )
  
  # Append to CSV file (or update if file was already processed)
  if (file.exists(output_csv)) {
    existing_data <- read.csv(output_csv, stringsAsFactors = FALSE)
    # Remove any existing entry for this specific filename (allows re-processing)
    # This means: new files are APPENDED, existing files are UPDATED
    existing_data <- existing_data[existing_data$filename != basename(x3p_path), ]
    combined_data <- rbind(existing_data, result)
  } else {
    # First file - create new CSV
    combined_data <- result
  }
  
  write.csv(combined_data, output_csv, row.names = FALSE)
  cat("\nSaved to", output_csv, "\n")
  cat("Left groove:", left_groove, "\n")
  cat("Right groove:", right_groove, "\n")
  
  return(result)
}

#' Process multiple x3p files in a directory
#' 
#' @param directory Path to directory containing x3p files
#' @param output_csv Path to output CSV file
#' @param pattern File pattern to match (default: "\\.x3p$")
#' @return Data frame with all groove locations
process_directory <- function(directory, output_csv = "groove_locations.csv", pattern = "\\.x3p$") {
  
  x3p_files <- list.files(directory, pattern = pattern, full.names = TRUE, recursive = FALSE)
  
  if (length(x3p_files) == 0) {
    cat("No x3p files found in", directory, "\n")
    return(NULL)
  }
  
  cat("\nFound", length(x3p_files), "x3p files\n")
  cat("Output will be saved to:", output_csv, "\n\n")
  
  results <- list()
  
  for (i in seq_along(x3p_files)) {
    cat("\n[", i, "/", length(x3p_files), "]\n")
    result <- process_file(x3p_files[i], output_csv)
    if (!is.null(result)) {
      results[[i]] <- result
    }
  }
  
  # Combine all results
  if (length(results) > 0) {
    all_results <- do.call(rbind, results)
    return(all_results)
  } else {
    return(NULL)
  }
}

# Command line interface
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args) == 0) {
    cat("Usage: Rscript manual_groove_selection.R <x3p_file_or_directory> [output_csv]\n")
    cat("\nExamples:\n")
    cat("  Rscript manual_groove_selection.R bullet1_land1.x3p\n")
    cat("  Rscript manual_groove_selection.R bullet1_land1.x3p my_grooves.csv\n")
    cat("  Rscript manual_groove_selection.R examples/Hamby-44/Barrel_1/Bullet_1/ grooves.csv\n")
    quit(status = 1)
  }
  
  input_path <- args[1]
  output_csv <- if (length(args) >= 2) args[2] else "groove_locations.csv"
  
  if (dir.exists(input_path)) {
    # Process directory
    process_directory(input_path, output_csv)
  } else if (file.exists(input_path)) {
    # Process single file
    process_file(input_path, output_csv)
  } else {
    cat("Error: File or directory not found:", input_path, "\n")
    quit(status = 1)
  }
}

cat("\n=== Manual Groove Selection Tool Loaded ===\n")
cat("Functions available:\n")
cat("  process_file(x3p_path, output_csv)      - Process a single file\n")
cat("  process_directory(directory, output_csv) - Process all x3p files in a directory\n")
cat("\nExample usage:\n")
cat('  process_file("examples/Hamby-44/barrel 1/Bullet 1/Barrel_1-Bullet_1-Land_1.x3p", "grooves.csv")\n')
cat('  process_directory("examples/Hamby-44/barrel 1/Bullet 1/", "grooves.csv")\n')
