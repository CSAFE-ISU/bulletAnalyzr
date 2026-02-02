#' Get Optimal Crosscut Location
#'
#' Finds the optimal crosscut Y location for an x3p scan using
#' bulletxtrctr::x3p_crosscut_optimize with progressive fallback.
#'
#' @param x3p An x3p object (in micrometers)
#' @returns The optimal crosscut Y location in microns
get_optimal_crosscut <- function(x3p) {
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
  return(crosscut_y)
}

#' View Bullet Processing Pipeline
#'
#' Interactively visualizes each step of the bullet analysis pipeline for a
#' single x3p land scan: the land image, crosscut profile, groove locations,
#' and extracted signal. When view_grooves is TRUE, prompts for manual groove
#' selection.
#'
#' @param filepath Path to an x3p file
#' @param view_land Logical; display the x3p land image (default TRUE)
#' @param view_cc_profile Logical; display the crosscut profile plot (default TRUE)
#' @param view_grooves Logical; display groove locations and prompt for manual
#'   selection (default TRUE)
#' @param view_signal Logical; display the extracted signal plot (default TRUE)
#' @param grooves_csv Optional path to a CSV file with columns \code{filename},
#'   \code{crosscut_y}, \code{left_groove}, and \code{right_groove}. When
#'   provided, the matching row (by \code{basename(filepath)}) supplies the
#'   crosscut and groove values instead of auto-computing them.
#' @returns NULL (called for side effects: plots and interactive input)
#' @examples
#' x3ppath <- "examples/Hamby Set 44 Final/Barrel 1/Bullet 1/Barrel_1-Bullet_1-Land_1.x3p"
#' view_pipeline(x3ppath)
#' view_pipeline(x3ppath, view_land = FALSE, view_cc_profile = TRUE)
#' view_pipeline(x3ppath, view_land = TRUE, view_cc_profile = TRUE)
#' view_pipeline(x3ppath, view_grooves = TRUE)
#' view_pipeline(x3ppath, view_land = FALSE, view_signal = TRUE)
#' # Use a grooves CSV to supply crosscut and groove values:
#' view_pipeline(
#'   x3ppath, 
#'   grooves_csv = file.path(dirname(x3ppath), "grooves.csv"), 
#'   view_grooves = TRUE
#' )
#' view_pipeline(
#'   x3ppath, 
#'   grooves_csv = file.path(dirname(x3ppath), "grooves.csv"), 
#'   view_grooves = TRUE,
#'   view_signal = TRUE
#' )
#'
view_pipeline <- function(filepath, view_land = TRUE, view_cc_profile = FALSE, view_grooves = FALSE, view_signal = FALSE, grooves_csv = NULL) {
  # Determine the last pipeline step needed based on view flags
  # Step mapping: 1=read/land, 2=preprocess, 3=crosscut, 4=grooves, 5=signal
  last_step <- max(c(
    0,
    if (view_land) 1 else NULL,
    if (view_cc_profile) 3 else NULL,
    if (view_grooves) 4 else NULL,
    if (view_signal) 5 else NULL
  ))

  if (last_step == 0) return()

  cat("\n== Processing:", basename(filepath), "==\n")

  # Step 1: Read x3p
  cat(sprintf("[1/%d] Reading x3p file...\n", last_step))
  x3p <- x3ptools::read_x3p(filepath)

  if (view_land) {
    x3ptools::x3p_image(x3p)
  }

  if (last_step <= 1) {
    cat("== Done:", basename(filepath), "==\n\n")
    return()
  }

  # Step 2: Preprocessing
  cat(sprintf("[2/%d] Preprocessing...\n", last_step))
  # Convert from meters to micrometers if needed (same as bulletAnalyzrApp)
  scale <- x3ptools::x3p_get_scale(x3p)
  if (scale < 0.1) {
    cat("Converting from meters to micrometers...\n")
    x3p <- x3ptools::x3p_m_to_mum(x3p)
  }

  # Look up CSV row if grooves_csv is provided
  csv_row <- NULL
  if (!is.null(grooves_csv)) {
    csv_data <- read.csv(grooves_csv, stringsAsFactors = FALSE)
    match_idx <- which(csv_data$filename == basename(filepath))
    if (length(match_idx) == 0) {
      cat("Warning: No matching row for", basename(filepath), "in CSV. Falling back to auto-detection.\n")
    } else {
      csv_row <- csv_data[match_idx[1], ]
    }
  }

  # Step 3: Crosscut
  cat(sprintf("[3/%d] Finding crosscut...\n", last_step))
  if (!is.null(csv_row)) {
    crosscut_y <- csv_row$crosscut_y
    cat("Using crosscut_y from CSV:", crosscut_y, "\n")
  } else {
    crosscut_y <- get_optimal_crosscut(x3p = x3p)
  }
  ccdata <- bulletxtrctr::x3p_crosscut(x3p, y = crosscut_y, range = 1e-5)

  # Ensure x coordinates start from 0 and span the full width
  ccdata$x <- ccdata$x - min(ccdata$x, na.rm = TRUE)

  if (view_cc_profile || view_grooves) {
    p <- ggplot2::ggplot(ccdata, ggplot2::aes(x = x, y = value)) +
      ggplot2::geom_line() +
      ggplot2::labs(
        title = paste("Crosscut Profile -", basename(filepath)),
        x = "Position (microns)",
        y = "Height (microns)"
      ) +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
      ggplot2::theme_bw() +
      ggplot2::theme(text = ggplot2::element_text(size = 14),
                     axis.text.x=ggplot2::element_text(angle = 30, hjust = 1))
  }

  if (view_cc_profile) {
    print(p)
  }

  if (last_step <= 3) {
    cat("== Done:", basename(filepath), "==\n\n")
    return()
  }

  # Step 4: Grooves
  cat(sprintf("[4/%d] Locating grooves...\n", last_step))
  if (!is.null(csv_row)) {
    grooves_auto <- list(groove = c(csv_row$left_groove, csv_row$right_groove))
    cat("Using groove locations from CSV: left =", csv_row$left_groove, ", right =", csv_row$right_groove, "\n")
  } else {
    grooves_auto <- bulletxtrctr::cc_locate_grooves(ccdata, method = "middle", adjust = 30, return_plot = FALSE)
  }

  if (view_grooves) {
    # Plot the crosscut profile with grooves
    p <- p +
      ggplot2::geom_vline(xintercept = grooves_auto$groove[1], color = "red", linetype = "dashed", linewidth = 1) +
      ggplot2::geom_vline(xintercept = grooves_auto$groove[2], color = "red", linetype = "dashed", linewidth = 1) +
      ggplot2::labs(
        title = paste("Crosscut Profile -", basename(filepath)),
        subtitle = "Red dashed lines show automatic groove locations",
        x = "Position (microns)",
        y = "Height (microns)"
      )

    print(p)

    if (!is.null(csv_row)) {
      # CSV supplied — use groove values directly, no interactive override
      grooves <- grooves_auto
    } else {
      # Interactive input for groove locations
      cat("\n--- Manual Groove Selection ---\n")
      cat("Automatic left groove:", grooves_auto$groove[1], "\n")
      cat("Automatic right groove:", grooves_auto$groove[2], "\n")
      cat("\nOptions:\n")
      cat("  1. Press ENTER to accept automatic grooves\n")
      cat("  2. Enter custom values as: left,right (e.g., 500,2500)\n")

      user_input <- readline(prompt = "Your choice: ")

      if (user_input == "") {
        # Use automatic values
        left_groove <- grooves_auto$groove[1]
        right_groove <- grooves_auto$groove[2]
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

            # Check that both values are valid numbers
            if (is.na(left_groove) || is.na(right_groove)) {
              cat("Invalid input. Both values must be numbers. Please enter values as: left,right (e.g., 500,2500)\n")
              user_input <- readline(prompt = "Enter groove locations: ")
              next
            }

            # Show updated plot with proposed manual grooves in blue
            p_manual <- p +
              ggplot2::geom_vline(xintercept = left_groove, color = "blue", linetype = "solid", linewidth = 1.2) +
              ggplot2::geom_vline(xintercept = right_groove, color = "blue", linetype = "solid", linewidth = 1.2) +
              ggplot2::labs(subtitle = "Red dashed = auto, Blue solid = proposed manual")
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
      grooves <- list(groove = c(left_groove, right_groove))
    }
  } else {
    grooves <- grooves_auto
  }

  if (last_step <= 4) {
    cat("== Done:", basename(filepath), "==\n\n")
    return()
  }

  # Step 5: Signal
  cat(sprintf("[5/%d] Extracting signal...\n", last_step))
  signal <- bulletxtrctr::cc_get_signature(ccdata, grooves, span1 = 0.75, span2 = 0.03)

  if (view_signal) {
    p <- ggplot2::ggplot(signal, ggplot2::aes(x = x)) +
      ggplot2::geom_line(ggplot2::aes(y = raw_sig), colour = "grey70") +
      ggplot2::geom_line(ggplot2::aes(y = sig), colour = "grey30") +
      ggplot2::labs(
        title = paste("Signal -", basename(filepath)),
        subtitle = "Dark line shows Loess-smoothed signal. Light line shows raw signal.",
        x = "Position (microns)",
        y = "Height (microns)"
      ) +
      ggplot2::theme_bw()
    print(p)
  }

  cat("== Done:", basename(filepath), "==\n\n")
  return()
}
