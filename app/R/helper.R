#' Copy to Temporary Directory
#'
#' @param filepath A string of the current filepath.
#' @param filename A string of the current filename.
#'
#' @returns A path to a new temp directory
#' @noRd
copy_to_tempdir <- function(filepath, filename) {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  file.copy(filepath, file.path(temp_dir, filename))
  return(temp_dir)
}

filter_preview_bullet <- function(allbull, preview_bull_name) {
  bull <- allbull[allbull$bullet == preview_bull_name,]
  return(bull)
}

get_bsldata <- function(bullet_scores) {
  # Collect land-wise data ----
  bsldata <- bullet_scores$data[[1]]
  # Sort in descending order ----
  # just re-order the data - that will be safer and quicker
  bsldata <- bsldata %>% 
    mutate(samesource = factor(samesource, levels = c(TRUE, FALSE))) %>%
    group_by(samesource) %>% 
    arrange(desc(rfscore), .by_group = TRUE)
  return(bsldata)
}

get_max_microns <- function(bullets) {
  bullet_y_ranges <- sapply(bullets$x3p, function(x3p) {
    # Get the Y coordinate range from the x3p header info
    y_max <- floor(x3p$header.info$incrementY * (x3p$header.info$sizeY - 1))
    return(y_max)
  })
  return(bullet_y_ranges)
}

get_panel_name <- function(bsldata, odridx, idx) {
  panel_name <- paste0(bsldata$land1[odridx[idx]], " vs ", bsldata$land2[odridx[idx]]," (RF Score = ", round(bsldata$rfscore[odridx[idx]],4), ")")
  return(panel_name)
}

get_rf_order <- function(bsldata) {
  return(order(bsldata$rfscore, decreasing = TRUE))
}

identify_lands <- function(words) {
  # create a list of distinguishing elements between names
  
  if (length(words) == 1) return(1)

  # split each word by character and transpose  
  list <- strsplit(words, split="")
  tlist <- purrr::list_transpose(list)
  # toss out everything that matches
  difflist <- tlist %>% purrr::map(.f = function(l) { if (length(unique(l)) > 1) return(l); NULL})
  difflist <- purrr::discard(difflist, is.null) 
  # transpose back and make 'word'
  difflist <- difflist %>% purrr::list_transpose() %>% purrr::map_chr(paste, collapse="")
  make.unique(difflist) # make sure that something is there and it is different
}

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

show_modal <- function(title, message, show_alert, session) {
  if (show_alert) {
    showModal(modalDialog(
      title = title,
      message,
      easyClose = TRUE,
      footer = modalButton("OK")
    ), session = session)
  }
}
