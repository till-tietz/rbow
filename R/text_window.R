#' Utility function that returns the subset of elements in a vector between a start and end position
#'
#'@param text input vector
#'@param start start position
#'@param end end position
#'@param position vector of word positions around which a window is constructed
#'@param out_of_window if TRUE function returns set of words within and outside of window
#'@export

text_window <- function(text, start, end, position, out_of_window = FALSE) {
    loop_window <- function(x) {
      position_vec <- c(start[x]:end[x])
      position_vec <- position_vec[!position_vec %in% position[x]]
      text_window_i <- text[position_vec]
      return(text_window_i)
    }
    loop_w_out <- purrr::map(1:length(start), ~ loop_window(.x))

    if (out_of_window == TRUE) {
      loop_out_of_window <- function(x) {
        position_vec <- c(start[x]:end[x])
        return(position_vec)
      }
      loop_ofw_out <- purrr::map(1:length(start), ~ loop_out_of_window(.x))
      ofw_index <- unique(unlist(loop_ofw_out))
      ofw_text <- text[-ofw_index]
      out <- list(w_text = loop_w_out, ofw_text = ofw_text)
    } else {
      out <- loop_w_out
    }
    return(out)
  }
