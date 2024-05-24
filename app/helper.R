

identify_lands <- function(words) {
  # create a list of distinguishing elements between names
  list <- strsplit(words, split="")
  # compare to first word: 
  tlist <- purrr::list_transpose(list)
  difflist <- tlist %>% purrr::map(.f = function(l) { if (length(unique(l)) > 1) return(l); NULL})
  purrr::discard(difflist, is.null) %>% purrr::list_transpose() %>% purrr::map_chr(paste, collapse="") 
}
