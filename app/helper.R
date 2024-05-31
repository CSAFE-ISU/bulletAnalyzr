

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
#  if (length(words) != length(difflist)) browser()
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


groove_plot <- function(ccdata, grooves) {
    ccdata %>% 
    ggplot(aes(x = x, y = value)) + 
#    theme_bw()+
    geom_vline(xintercept = 0, colour = "grey50") + 
    geom_vline(xintercept = grooves[2]-grooves[1], colour = "grey50") +
    geom_line(linewidth = .5) + # put signal in front
    annotate("rect", fill="grey50", alpha = 0.15, xmax = 0, xmin = -Inf, ymin = -Inf, ymax = Inf) +
    annotate("rect", fill="grey50", alpha = 0.15, xmax = Inf, xmin = grooves[2]-grooves[1], ymin = -Inf, ymax = Inf) +
    geom_line(linewidth = 1, data = filter(ccdata, between(x, 0, grooves[2]-grooves[1]))) + # put signal in front
    scale_x_continuous(
      breaks=c(0,round(as.numeric(grooves[2]-grooves[1]),0),round(seq(min(ccdata$x),max(ccdata$x),by=500),-2)),
      labels=c("\n0",paste0("\n",round(as.numeric(grooves[2]-grooves[1]),0)),round(seq(min(ccdata$x),max(ccdata$x),by=500),-2))
    ) +
    xlab("Position along width of Land [µm]") +
    ylab("Surface Height [µm]") 
}

