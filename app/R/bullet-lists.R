add_cbull_to_allbull <- function(cbull, bul_x3p_name, allbull) {
  # If current bullet is already in allbull, remove it
  allbull <- allbull[!(allbull$bullet %in% bul_x3p_name),]
  
  # Add bullet and land columns to current bullet
  cbull$bullet <- bul_x3p_name
  cbull$land <- factor(cbull$land_names, levels = cbull$land_names)
  
  return(rbind(allbull, cbull))
}

filter_selected_bullet <- function(bullets, selected) {
  bullets <- bullets[bullets$bullet == selected,]
  return(bullets)
}

filter_selected_bullets <- function(bullet_scores, selected1, selected2) {
  bullet_scores <- bullet_scores[bullet_scores$bulletA == selected1 & bullet_scores$bulletB == selected2,]
  return(bullet_scores)
}

make_temptable <- function(BullCompBulls, selected1, selected2, bsldata, odridx, idx, instrument, scale) {
  
  temptable <- data.frame(
    Feature = c("Left Land File", "Left Land MD5", "Left Land Instrument (resolution [µm/px])", 
                "Right Land File", "Right Land MD5", "Left Land Instrument (resolution [µm/px])", 
                "Cross Correlation Function", "Mean Distance btw Signals [Âµm]",
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
  temptable_dt <- datatable(
    temptable, 
    rownames = FALSE, 
    options = list(paging = FALSE, ordering = FALSE, searching = FALSE, bLengthChange = FALSE, bInfo = FALSE)
  )
  return(temptable_dt)
}