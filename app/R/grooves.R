get_grooves <- function(ccdata) {
  grooves <- lapply(
    ccdata, 
    function(x) cc_locate_grooves(x, method = "middle", adjust = 30, return_plot = FALSE)
  )
  return(grooves)
}