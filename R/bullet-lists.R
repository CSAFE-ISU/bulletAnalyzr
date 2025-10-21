add_cbull_to_allbull <- function(cbull, cbull_name, allbull) {
  # If current bullet is already in allbull, remove it
  allbull <- allbull[!(allbull$bullet %in% cbull_name),]
  
  return(rbind(allbull, cbull))
}

filter_grooves_ccdata <- function(BullCompBulls, selected1, selected2, bsldata, odridx, cidx) {
  GroovePlotLidx <- which(BullCompBulls$bullet == selected1 & BullCompBulls$land == bsldata$landA[odridx[cidx]])
  GroovePlotRidx <- which(BullCompBulls$bullet == selected2 & BullCompBulls$land == bsldata$landB[odridx[cidx]])
  GroovesL <- as.numeric(BullCompBulls$grooves[[GroovePlotLidx]]$groove)
  GroovesR <- as.numeric(BullCompBulls$grooves[[GroovePlotRidx]]$groove)
  CCDataL <- BullCompBulls$ccdata[[GroovePlotLidx]] - GroovesL[1]
  CCDataR <- BullCompBulls$ccdata[[GroovePlotRidx]] - GroovesR[1]
  return(list(CCDataL = CCDataL, CCDataR = CCDataR, GroovesL = GroovesL, GroovesR = GroovesR))
}

filter_selected_bullet <- function(bullets, selected) {
  bullets <- bullets[bullets$bullet == selected,]
  return(bullets)
}

filter_selected_bullets <- function(bullet_scores, selected1, selected2) {
  bullet_scores <- bullet_scores[bullet_scores$bulletA == selected1 & bullet_scores$bulletB == selected2,]
  return(bullet_scores)
}

filter_selected_bullet_land <- function(bullets, sel_bullet, sel_land) {
  bullets <- bullets %>%
    dplyr::filter(bullet == sel_bullet, land == sel_land)
  return(bullets)
}

filter_sig_plot_data <- function(BullCompComps, selected1, selected2, bsldata, odridx, cidx) {
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

filter_x3pimg <- function(BullCompBulls, selected1, selected2, bsldata, odridx, cidx) {
  rglLidx <- which(BullCompBulls$bullet == selected1 & BullCompBulls$land == bsldata$landA[odridx[cidx]])
  rglRidx <- which(BullCompBulls$bullet == selected2 & BullCompBulls$land == bsldata$landB[odridx[cidx]])
  rglL <- BullCompBulls$x3pimg[[rglLidx]]
  rglR <- BullCompBulls$x3pimg[[rglRidx]]
  return(list(rglL = rglL, rglR = rglR))
}

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
  temptable_dt <- datatable(
    temptable, 
    rownames = FALSE, 
    options = list(paging = FALSE, ordering = FALSE, searching = FALSE, bLengthChange = FALSE, bInfo = FALSE)
  )
  return(temptable_dt)
}
