# clean text data (remove punctuation, special characters, numbers, english stopwords)
#'
#'@param texts list of character vectors
#'@param lexicon stop word lexicon to use
#'@return list of cleaned character vectors
#'@export


clean_text <- function(texts, lexicon = "SMART"){
  stop.words <- tidytext::stop_words
  stop.words <- as.data.frame(stop.words[which(stop.words[,"lexicon"] == lexicon),])[,"word"]

  each_text <- function(x){
    text_i <- as.character(texts[[x]])
    clean_text <- gsub('[[:punct:] ]+',' ', text_i)
    clean_text <- gsub('[[:digit:]]+', '', clean_text)
    clean_text <- tolower(clean_text)
    clean_text <- unlist(strsplit(clean_text, split=" "))
    clean_text <- clean_text[-(which(clean_text %in% c("","\n","\f\n")))]
    clean_text <- trimws(clean_text, which = c("both"))

    clean_text <- clean_text[-(which(clean_text %in% stop.words))]
    return(clean_text)
  }
  out <- furrr::future_map(1:length(texts), ~each_text(.x), .progress = TRUE)
  return(out)
}
