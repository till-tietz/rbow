#'clean text data (remove punctuation, special characters, numbers, english stopwords)
#'
#'@name clean_text
#'
#'@param texts list of character vectors
#'@param rm_stopwords if TRUE clean_text removes stopwords (default FALSE)
#'@param stopwords_language stopword dictionary language ISO 639-1 code as character vector
#'@return list of cleaned character vectors
#'@export


clean_text <- function(texts, rm_stopwords = FALSE, stopwords_language = NULL){

  iso_codes <- as.character(ISOcodes::ISO_639_2[["Alpha_2"]])
  iso_codes <- iso_codes[!is.na(iso_codes)]

  if(rm_stopwords == TRUE){

    if(is.null(stopwords_language)){
      stop("no stopwords_language specified")
    }

    if(!stopwords_language %in% iso_codes){
      stop("did not specify valid ISO 639-1 code for stopwords_language")
    }

    stop_words <- stopwords::stopwords(language = stopwords_language,
                                       source = "snowball",
                                       simplify = TRUE)
  }

  each_text <- function(x){
    text_i <- as.character(texts[[x]])
    clean_text <- tolower(text_i)
    clean_text <- gsub("[^a-zA-Z\\s]", " ", clean_text)
    clean_text <- trimws(clean_text, which = "both")
    clean_text <- gsub("\\s+", " ", clean_text)
    clean_text <- unlist(strsplit(clean_text, split=" "))

    if(rm_stopwords == TRUE){
      clean_text <- clean_text[!clean_text %in% stop_words]
    }

    return(clean_text)
  }
  out <- furrr::future_map(1:length(texts), ~each_text(.x), .progress = TRUE)
  names(out) <- names(texts)
  return(out)
}


