#' utility function that creates regular expression for grep calls in bow and dfm analysis
#' @param text_input a character vector or list of character vectors to be turned into regular expression
#' @return a list of regular expressions if input was a list or a regular expression character vector if input was vector
#' @export


grep_construct <- function(text_input){

  if(is.list(text_input)){
    loop <- function(x){
      text_i <- text_input[[x]]

      text_i[grep("*", text_i, fixed = TRUE)] <- paste(text_i[grep("*", text_i, fixed = TRUE)], "$")
      text_i <- gsub("\\*", "", text_i)
      text_i <- gsub(" ", "", text_i)
      text_i <- paste(text_i, sep = "", collapse = "|^")
      text_i <- paste0("^", text_i)
      return(text_i)
    }
    out <- purrr::map(1:length(text_input), ~loop(.x))
  } else {
    text_i <- text_input
    text_i[grep("*", text_i, fixed = TRUE)] <- paste(text_i[grep("*", text_i, fixed = TRUE)], "$")
    text_i <- gsub("\\*", "", text_i)
    text_i <- gsub(" ", "", text_i)
    text_i <- paste(text_i, sep = "", collapse = "|^")
    text_i <- paste0("^", text_i)
    out <- text_i
  }
  return(out)
}
