

identify_lands <- function(words) {
  # create a list of distinguishing elements between names
  if (length(words == 1)) return(1)

  # split each word by character and transpose  
  list <- strsplit(words, split="")
  tlist <- purrr::list_transpose(list)
  # toss out everything that matches
  difflist <- tlist %>% purrr::map(.f = function(l) { if (length(unique(l)) > 1) return(l); NULL})
  difflist <- purrr::discard(difflist, is.null) 
  # transpose back and make 'word'
  difflist <- difflist %>% purrr::list_transpose() %>% purrr::map_chr(paste, collapse="")
  make.names(difflist, unique=TRUE) # make sure that something is there and it is different
}


identify_bullet <- function(words) {
  # create a list of the same elements between names
  if (length(words == 1)) return(words)
  
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
