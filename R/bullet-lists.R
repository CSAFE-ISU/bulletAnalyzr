#' Add Current Bullet to All Bullets Data Frame
#'
#' Adds the current bullet data to the collection of all bullets, removing
#' any existing data for the same bullet name first.
#'
#' @param cbull A data frame containing the current bullet data
#' @param cbull_name A string containing the name of the current bullet
#' @param allbull A data frame containing all previously added bullets
#'
#' @returns A data frame combining all bullets with the current bullet
#' @noRd
add_cbull_to_allbull <- function(cbull, cbull_name, allbull) {
  # If current bullet is already in allbull, remove it
  allbull <- allbull[!(allbull$bullet %in% cbull_name),]
  
  return(rbind(allbull, cbull))
}


#' Filter Grooves and Crosscut Data for Selected Bullets
#'
#' Extracts groove locations and crosscut data for two selected bullet lands.
#'
#' @param BullCompBulls A data frame containing bullet comparison data
#' @param selected1 A string containing the name of the first selected bullet
#' @param selected2 A string containing the name of the second selected bullet
#' @param bsldata A data frame containing bullet-land comparison data
#' @param odridx A numeric vector of ordered indices
#' @param cidx A numeric index for the current comparison
#'
#' @returns A list containing CCDataL, CCDataR, GroovesL, and GroovesR
#' @noRd
filter_grooves_ccdata <- function(BullCompBulls, selected1, selected2, bsldata, odridx, cidx) {
  GroovePlotLidx <- which(BullCompBulls$bullet == selected1 & BullCompBulls$land == bsldata$landA[odridx[cidx]])
  GroovePlotRidx <- which(BullCompBulls$bullet == selected2 & BullCompBulls$land == bsldata$landB[odridx[cidx]])
  GroovesL <- as.numeric(BullCompBulls$grooves[[GroovePlotLidx]]$groove)
  GroovesR <- as.numeric(BullCompBulls$grooves[[GroovePlotRidx]]$groove)
  CCDataL <- BullCompBulls$ccdata[[GroovePlotLidx]] - GroovesL[1]
  CCDataR <- BullCompBulls$ccdata[[GroovePlotRidx]] - GroovesR[1]
  return(list(CCDataL = CCDataL, CCDataR = CCDataR, GroovesL = GroovesL, GroovesR = GroovesR))
}


#' Filter Data Frame by Bullet Column
#'
#' Filters a data frame to only include rows matching the selected bullet(s) in
#' the bullet column, with optional unnesting of nested columns.
#'
#' @param df A data frame with a bullet column to filter
#' @param selected A character vector of bullet names to select
#' @param unnest_data Optional column name to unnest after filtering
#'
#' @returns A filtered data frame
#' @noRd
filter_bullet_col <- function(df, selected, unnest_data = NULL) {
  # Prevent no visible binding for global variable note
  bullet <- NULL
  
  df <- df %>%
    dplyr::filter(bullet %in% selected)
  
  if (!is.null(unnest_data)) {
    df <- df %>%
      tidyr::unnest(tidyselect::all_of(unnest_data))
  }
  
  return(df)
}


#' Filter Data Frame by BulletA and BulletB Columns
#'
#' Filters a data frame to only include rows matching the selected bulletA and
#' bulletB columns, with optional unnesting of nested columns.
#'
#' @param df A data frame to filter
#' @param selected1 A string containing the name of bulletA
#' @param selected2 A string containing the name of bulletB
#' @param unnest_data Optional column name to unnest after filtering
#'
#' @returns A filtered data frame
#' @noRd
filter_bulletA_bulletB_cols <- function(df, selected1, selected2, unnest_data = NULL) {
  df <- df[df$bulletA == selected1 & df$bulletB == selected2,]
  
  if (!is.null(unnest_data)) {
    df <- df %>%
      tidyr::unnest(tidyselect::all_of(unnest_data))
  }
  
  return(df)
}


#' Filter Data Frame by Bullet and Land Columns
#'
#' Filters a data frame to only include rows matching the selected bullet and land columns,
#' with optional unnesting of nested columns.
#'
#' @param df A data frame to filter
#' @param sel_bullet A string containing the selected bullet name
#' @param sel_land A string containing the selected land name
#' @param unnest_data Optional column name to unnest after filtering
#'
#' @returns A filtered data frame
#' @noRd
filter_bullet_land_cols <- function(df, sel_bullet, sel_land, unnest_data = NULL) {
  # Prevent no visible binding for global variable note
  bullet <- land <- NULL
  
  df <- df %>%
    dplyr::filter(bullet == sel_bullet, land == sel_land)
  
  if (!is.null(unnest_data)) {
    df <- df %>% 
      tidyr::unnest(unnest_data)
  }
  return(df)
}


#' Filter Signal Plot Data for Selected Comparison
#'
#' Extracts and formats aligned signal data for plotting a specific land comparison.
#'
#' @param BullCompComps A data frame containing bullet comparison data
#' @param selected1 A string containing the name of the first selected bullet
#' @param selected2 A string containing the name of the second selected bullet
#' @param bsldata A data frame containing bullet-land comparison data
#' @param odridx A numeric vector of ordered indices
#' @param cidx A numeric index for the current comparison
#'
#' @returns A data frame formatted for signal plotting
#' @noRd
filter_sig_plot_data <- function(BullCompComps, selected1, selected2, bsldata, odridx, cidx) {
  # Prevent no visible binding for global variable note
  Signal <- value <- sig1 <- sig2 <- NULL
  
  sig_plot_data <- BullCompComps$aligned[
    (BullCompComps$bulletA == selected1) &
      (BullCompComps$bulletB == selected2) &
      (BullCompComps$landA == bsldata$landA[odridx[cidx]]) &
      (BullCompComps$landB == bsldata$landB[odridx[cidx]])
  ][[1]]$lands
  sig_plot_data <- tidyr::gather(sig_plot_data, Signal, value, sig1, sig2)
  
  sig_plot_data$Signal[sig_plot_data$Signal == "sig1"] <- "Left LEA"
  sig_plot_data$Signal[sig_plot_data$Signal == "sig2"] <- "Right LEA"
  
  return(sig_plot_data)
}


#' Filter x3p Images for Selected Comparison
#'
#' Extracts the x3p image snapshots for two selected bullet lands.
#'
#' @param BullCompBulls A data frame containing bullet comparison data
#' @param selected1 A string containing the name of the first selected bullet
#' @param selected2 A string containing the name of the second selected bullet
#' @param bsldata A data frame containing bullet-land comparison data
#' @param odridx A numeric vector of ordered indices
#' @param cidx A numeric index for the current comparison
#'
#' @returns A list containing rglL and rglR image paths
#' @noRd
filter_x3pimg <- function(BullCompBulls, selected1, selected2, bsldata, odridx, cidx) {
  rglLidx <- which(BullCompBulls$bullet == selected1 & BullCompBulls$land == bsldata$landA[odridx[cidx]])
  rglRidx <- which(BullCompBulls$bullet == selected2 & BullCompBulls$land == bsldata$landB[odridx[cidx]])
  rglL <- BullCompBulls$x3pimg[[rglLidx]]
  rglR <- BullCompBulls$x3pimg[[rglRidx]]
  return(list(rglL = rglL, rglR = rglR))
}


#' Create Comparison Features Table
#'
#' Creates a formatted data table displaying features for a bullet land comparison.
#'
#' @param BullCompBulls A data frame containing bullet comparison data
#' @param selected1 A string containing the name of the first selected bullet
#' @param selected2 A string containing the name of the second selected bullet
#' @param bsldata A data frame containing bullet-land comparison data
#' @param odridx A numeric vector of ordered indices
#' @param idx A numeric index for the current comparison
#' @param instrument A string containing the instrument name
#' @param scale A numeric value for the scan resolution
#'
#' @returns A DT::datatable object
#' @noRd
make_temptable <- function(BullCompBulls, selected1, selected2, bsldata, odridx, idx, instrument, scale) {
  
  temptable <- data.frame(
    Feature = c("Left Land File", "Left Land MD5", "Left Land Instrument (resolution [µm/px])", 
                "Right Land File", "Right Land MD5", "Left Land Instrument (resolution [µm/px])", 
                "Cross Correlation Function", "Mean Distance btw Signals [µm]",
                "Signal Length [mm]", "# Matching Striae Per Millimeter",
                "# Mis-Matching Striae Per Millimeter", "CMS Per Millimeter",
                "Non-CMS Per Millimeter", "Peak Sum"),
    Value = c(
      BullCompBulls$filename[BullCompBulls$bullet == selected1 & BullCompBulls$land == bsldata$landA[odridx[idx]]],
      BullCompBulls$md5sum[BullCompBulls$bullet == selected1 & BullCompBulls$land == bsldata$landA[odridx[idx]]],
      sprintf("%s (%s)", instrument, scale),
      BullCompBulls$filename[BullCompBulls$bullet == selected2 & BullCompBulls$land == bsldata$landB[odridx[idx]]],
      BullCompBulls$md5sum[BullCompBulls$bullet == selected2 & BullCompBulls$land == bsldata$landB[odridx[idx]]],
      sprintf("%s (%s)", instrument, scale),
      round(bsldata$ccf[odridx[idx]],3),
      round(bsldata$D[odridx[idx]],3),
      round(bsldata$length_mm[odridx[idx]],3),
      round(bsldata$matches_per_mm[odridx[idx]],3),
      round(bsldata$mismatches_per_mm[odridx[idx]],3),
      round(bsldata$cms_per_mm[odridx[idx]],3),
      round(bsldata$non_cms_per_mm[odridx[idx]],3),
      round(bsldata$sum_peaks[odridx[idx]],3)
    )
  )
  temptable_dt <- DT::datatable(
    temptable, 
    rownames = FALSE, 
    options = list(paging = FALSE, ordering = FALSE, searching = FALSE, bLengthChange = FALSE, bInfo = FALSE)
  )
  return(temptable_dt)
}