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