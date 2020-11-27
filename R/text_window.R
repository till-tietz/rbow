#' Utility function that returns the subset of elements in a vector between a start and end position
#'
#'@param text input vector
#'@param start start position
#'@param end end position
#'@export

text_window <- function(text, start, end){
  loop <- function(x){
    text_window_i <- text[c(start[x]:end[x])]
    return(text_window_i)
  }
  out <- purrr::map(1:length(start), ~loop(.x))
  return(out)
}
