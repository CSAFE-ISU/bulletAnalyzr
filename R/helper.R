#' Copy to Temporary Directory
#' 
#' Copy a file to a new temporary directory
#'
#' @param filepath A string of the filepath to copy
#' @param filename A string of the filename to copy
#'
#' @returns A path to a new temp directory
#' @noRd
copy_to_tempdir <- function(filepath, filename) {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  file.copy(filepath, file.path(temp_dir, filename))
  return(temp_dir)
}


#' Get Bullet-Land Score Data
#'
#' Extracts and sorts land-wise comparison data from bullet scores.
#'
#' @param bullet_scores A data frame containing bullet-level comparison scores
#'
#' @returns A sorted data frame of land-wise comparison data
#' @noRd
get_bsldata <- function(bullet_scores) {
  # Prevent no visible binding for global variable note
  samesource <- rfscore <- NULL
  
  # Collect land-wise data ----
  bsldata <- bullet_scores$data[[1]]
  # Sort in descending order ----
  # just re-order the data - that will be safer and quicker
  bsldata <- bsldata %>% 
    dplyr::mutate(samesource = factor(samesource, levels = c(TRUE, FALSE))) %>%
    dplyr::group_by(samesource) %>% 
    dplyr::arrange(dplyr::desc(rfscore), .by_group = TRUE)
  return(bsldata)
}

#' Get Maximum Y Coordinate in Microns
#'
#' Calculates the maximum Y coordinate range for each bullet land in microns.
#'
#' @param bullets A data frame containing bullet data with x3p objects
#'
#' @returns A numeric vector of maximum Y coordinates in microns
#' @noRd
get_max_microns <- function(bullets) {
  bullet_y_ranges <- sapply(bullets$x3p, function(x3p) {
    # Get the Y coordinate range from the x3p header info
    y_max <- floor(x3p$header.info$incrementY * (x3p$header.info$sizeY - 1))
    return(y_max)
  })
  return(bullet_y_ranges)
}

#' Get Panel Name for Comparison
#'
#' Creates a formatted panel name for a land-to-land comparison including the
#' random forest score.
#'
#' @param bsldata A data frame containing bullet-land comparison data
#' @param odridx A numeric vector of ordered indices
#' @param idx A numeric index for the current comparison
#'
#' @returns A character string containing the panel name
#' @noRd
get_panel_name <- function(bsldata, odridx, idx) {
  panel_name <- paste0(bsldata$land1[odridx[idx]], " vs ", bsldata$land2[odridx[idx]]," (RF Score = ", round(bsldata$rfscore[odridx[idx]],4), ")")
  return(panel_name)
}


#' Get Random Forest Score Order
#'
#' Returns indices that would sort the data by the random forest score in
#' descending order.
#'
#' @param bsldata A data frame containing bullet-land comparison data with
#'   rfscore column
#'
#' @returns A numeric vector of indices in descending RF score order
#' @noRd
get_rf_order <- function(bsldata) {
  return(order(bsldata$rfscore, decreasing = TRUE))
}

#' Identify Common Bullet Name
#'
#' Identifies common characters across multiple file names to suggest a
#' bullet name.
#'
#' @param words A character vector of file names
#'
#' @returns A string containing the common prefix or a default prompt
#' @noRd
identify_bullet <- function(words) {
  # create a list of the same elements between names
  if (length(words) == 1) return(words)
  
  # split each word by character and transpose  
  list <- strsplit(words, split="")
  tlist <- purrr::list_transpose(list)
  # toss out everything that's different
  samelist <- tlist %>% purrr::map(.f = function(l) { if (length(unique(l)) == 1) return(l); NULL})
  samelist <- purrr::discard(samelist, is.null) 
  # transpose back and make 'word'
  if (length(samelist) == 0) return("Enter name of Bullet")
  samelist <- samelist %>% purrr::list_transpose() 
  make.names(paste(samelist[[1]], collapse="")) # delete all forbidden characters
}

#' Check if Current Stage is Crosscut
#'
#' Checks whether the crosscut stage is in the stages vector.
#'
#' @param stages A character vector of current processing stages
#' @param strict Logical; if TRUE, checks if crosscut is the most recent stage
#'
#' @returns Logical; TRUE if in crosscut stage
#' @noRd
is_crosscut <- function(stages, strict = FALSE) {
  return(is_stage(current = "crosscut", stages = stages, strict = strict))
}

#' Check if Current Stage is Groove
#'
#' Checks whether the groove stage is in the stages vector.
#'
#' @param stages A character vector of current processing stages
#' @param strict Logical; if TRUE, checks if groove is the most recent stage
#'
#' @returns Logical; TRUE if in groove stage
#' @noRd
is_groove <- function(stages, strict = FALSE) {
  return(is_stage(current = "groove", stages = stages, strict = strict))
}

#' Check if Current Stage is Report
#'
#' Checks whether the report stage is in the stages vector.
#'
#' @param stages A character vector of current processing stages
#' @param strict Logical; if TRUE, checks if report is the most recent stage
#'
#' @returns Logical; TRUE if in report stage
#' @noRd
is_report <- function(stages, strict = FALSE) {
  return(is_stage(current = "report", stages = stages, strict = strict))
}

#' Check if Current Stage Matches Target
#'
#' Generic function to check if a specific stage is in the stages vector.
#'
#' @param current A string containing the target stage name
#' @param stages A character vector of current processing stages
#' @param strict Logical; if TRUE, checks if current is the most recent stage
#'
#' @returns Logical; TRUE if current stage matches
#' @noRd
is_stage <- function(current, stages, strict = FALSE) {
  
  if (current %in% stages) {
    if (strict && (current != stages[length(stages)])) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  } else {
    return(FALSE)
  }
  
}

#' Check if Current Stage is Upload
#'
#' Checks whether the upload stage is in the stages vector.
#'
#' @param stages A character vector of current processing stages
#' @param strict Logical; if TRUE, checks if upload is the most recent stage
#'
#' @returns Logical; TRUE if in upload stage
#' @noRd
is_upload <- function(stages, strict = FALSE) {
  return(is_stage(current = "upload", stages = stages, strict = strict))
}

#' Prepare Data Frame for Export
#'
#' Modifies a data frame for export by removing large x3p objects and converting
#' file paths to file names for consistent testing.
#'
#' @param df A data frame to prepare for export
#'
#' @returns A modified data frame suitable for export
#' @noRd
make_export_df <- function(df) {
  # Modify data frame for export for testing. Drop the x3p column because it
  # makes the snapshots 100+ MB. Change source column from filepath to filename
  # because the temp directory filepath will change every time, but the
  # filenames should remain consistent.
  if (is.null(df)) {
    return(NULL)
  }
  
  # The x3p column makes the snapshots massive. Keep a record that the column
  # exists, but change the entries to NA to save space.
  if ("x3p" %in% colnames(df)) {
    df$x3p <- NA
  }
  
  # The x3pimg column makes the snapshots massive. Keep a record that the column
  # exists, but change the entries to NA to save space.
  if ("x3pimg" %in% colnames(df)) {
    df$x3pimg <- NA
  }
  
  if ("source" %in% colnames(df)) {
    df <- df %>%
      dplyr::mutate(source = basename(source))
  }
  
  return(df)
}

#' Show Modal Dialog
#'
#' Displays a modal dialog with a title and message if alerts are enabled.
#'
#' @param title A string containing the modal dialog title
#' @param message A string containing the modal dialog message
#' @param show_alert Logical; if TRUE, displays the modal
#' @param session The Shiny session object
#'
#' @returns NULL (displays modal as side effect)
#' @noRd
show_modal <- function(title, message, show_alert, session) {
  if (show_alert) {
    shiny::showModal(shiny::modalDialog(
      title = title,
      message,
      easyClose = TRUE,
      footer = shiny::modalButton("OK")
    ), session = session)
  }
}
