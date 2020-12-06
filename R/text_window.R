#' Utility function that returns the subset of elements in a vector between a start and end position
#'
#'@param text input vector
#'@param start start position
#'@param end end position
#'@param position vector of word positions around which a window is constructed
#'@export

text_window <- function(text, start, end, position) {
  loop <- function(x) {
    position_vec <- c(start[x]:end[x])
    position_vec <- position_vec[!position_vec %in% position[x]]
    text_window_i <- text[position_vec]
    return(text_window_i)
  }
  out <- purrr::map(1:length(start), ~ loop(.x))
  return(out)
}
