# apply SnowballC stemming function to texts
#'
#'@name stem_texts
#'@title stem_texts
#'@param texts list of character vectors
#'@param language a language supported by SnowballC defaults to english
#'@return list of stemmed character vectors
#'@export


stem_texts <- function(texts, language = "english"){
  each_text <- function(x){
    stem_vec <- SnowballC::wordStem(texts[[x]], language = language)
    return(stem_vec)
  }
  out <- purrr::map(1:length(texts), ~each_text(.x))
  names(out) <- names(texts)
  return(out)
}
