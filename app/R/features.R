get_features <- function(comparisons, resolution, progress) {
  # Calculate CCF0 ----
  progress$set(message = "Evaluating Features", value = .2)
  comparisons$ccf0 <- sapply(comparisons$aligned, function(x) extract_feature_ccf(x$lands))
  
  # Evaluate striation marks ----
  progress$set(message = "Evaluating Striation Marks", value = .25)
  comparisons$striae <- lapply(comparisons$aligned, sig_cms_max, span = 75)
  
  # Extract features ----
  progress$set(message = "Extracting Features", value = .35)
  comparisons$bulletA <- sapply(strsplit(as.character(comparisons$land1),"-"),"[[",1)
  comparisons$bulletB <- sapply(strsplit(as.character(comparisons$land2),"-"),"[[",1)
  comparisons$landA <- sapply(strsplit(as.character(comparisons$land1),"-"),"[[",2)
  comparisons$landB <- sapply(strsplit(as.character(comparisons$land2),"-"),"[[",2)
  comparisons$features <- mapply(
    extract_features_all,
    comparisons$aligned,
    comparisons$striae,
    MoreArgs = list(resolution = resolution),
    SIMPLIFY = FALSE
  )
  
  # Scale features ----
  progress$set(message = "Scaling Features", value = .4)
  features <- tidyr::unnest(
    comparisons[,c("land1", "land2", "ccf0", "bulletA", "bulletB", "landA", "landB", "features")], 
    cols = features
  )
  features <- features %>% 
    mutate(cms = cms_per_mm,matches = matches_per_mm, mismatches = mismatches_per_mm, non_cms = non_cms_per_mm)
  
  return(list(comparisons = comparisons, features = features))
}