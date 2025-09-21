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
  
  df <- df %>% 
    dplyr::select(-tidyselect::any_of(c("x3p", "x3pimg"))) 
  
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

try_x3p_crosscut <- function(x3p, y = NULL, range = 1e-5) {
  res <- x3p_crosscut(x3p=x3p, y = y, range = range)
  if (nrow(res) == 0) {
    res <- x3p_crosscut(x3p=x3p, y = NULL, range = range)
  }
  return(res)
}
