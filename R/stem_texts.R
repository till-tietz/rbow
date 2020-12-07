# apply SnowballC stemming function to texts
#'
#'@param texts list of character vectors
#'@param language a language supported by SnowballC
#'@return list of stemmed character vectors
#'@export


stem_texts <- function(texts, language){
  each_text <- function(x){
    stem_vec <- SnowballC::wordStem(words, language = language)
    return(stem_vec)
  }
  out <- purrr::map(1:length(texts), ~each_text(.x))
}