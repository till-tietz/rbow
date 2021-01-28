#' utility function that creates regular expression for grep calls in bow and dfm analysis
#' @param text_input a character vector or list of character vectors to be turned into regular expression
#' @param collapse when TRUE function collapses text_input into a single regex string. when FALSE function creates a vector or regex strings. defaults to FALSE
#' @return a list of regular expressions if input was a list or a regular expression character vector if input was vector
#' @export


grep_construct <- function(text_input, collapse = FALSE){

  if(is.list(text_input)){
    loop <- function(x){
      text_i <- text_input[[x]]

      text_i[-grep("*", text_i, fixed = TRUE)] <- paste(text_i[-grep("*", text_i, fixed = TRUE)], "\\b")
      text_i <- gsub("\\*", "", text_i)
      text_i <- gsub(" ", "", text_i)
      if(collapse == TRUE){
        text_i <- paste(text_i, sep = "", collapse = "|\\b")
      }
      text_i <- paste0("\\b", text_i)
      return(text_i)
    }
    out <- purrr::map(1:length(text_input), ~loop(.x))
  } else {
    text_i <- text_input
    text_i[-grep("*", text_i, fixed = TRUE)] <- paste(text_i[-grep("*", text_i, fixed = TRUE)], "\\b")
    text_i <- gsub("\\*", "", text_i)
    text_i <- gsub(" ", "", text_i)
    if(collapse == TRUE){
      text_i <- paste(text_i, sep = "", collapse = "|\\b")
    }
    text_i <- paste0("\\b", text_i)
    out <- text_i
  }
  return(out)
}


