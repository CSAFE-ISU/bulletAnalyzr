

identify_lands <- function(words) {
  # create a list of distinguishing elements between names

  # split each word by character and transpose  
  list <- strsplit(words, split="")
  tlist <- purrr::list_transpose(list)
  # toss out everything that matches
  difflist <- tlist %>% purrr::map(.f = function(l) { if (length(unique(l)) > 1) return(l); NULL})
  difflist <- purrr::discard(difflist, is.null) 
  # transpose back and make 'word
  difflist %>% purrr::list_transpose() %>% purrr::map_chr(paste, collapse="")
  make.names(difflist, unique=TRUE) # make sure that something is there and it is different
}
