#' find the most relevant document for a queery by computing tf-idf for a set of terms within a corpus
#' @param corpus the text or texts to be analyzed as a list of character vectors
#' @param terms a character vector (or a regular expression if own_regex == TRUE) of terms to determine document relevance with
#' @return a data frame with document number and mean tf-idf
#' @export

tf_idf <- function(corpus, terms, own_regex = FALSE) {

  if(own_regex == TRUE){
    grep_terms <- terms
  } else {
    grep_terms <-
      rbow::grep_construct(text_input = terms, collapse = FALSE)
  }

  n_doc <- length(corpus)

  idf <- function(x) {
    grep_terms_i <- grep_terms[x]
    each_doc <- function(x) {
      doc_i <- corpus[[x]]
      tindoc <- sum(length(grep(grep_terms_i, doc_i)) > 0)
      return(tindoc)
    }
    each_doc_out <- purrr::map_dbl(1:n_doc, ~ each_doc(.x))
    idf_i <- log(n_doc / (sum(each_doc_out) + 1))
    return(idf_i)
  }
  idf_out <- purrr::map_dbl(1:length(grep_terms), ~ idf(.x))

  tf <- function(x) {
    doc_i <- corpus[[x]]
    length_doc_i <- length(doc_i)
    each_term <- function(x) {
      grep_terms_i <- grep_terms[x]
      idf_i <- idf_out[x]

      tf_idf_i <-
        (length(grep(grep_terms_i, doc_i)) / length_doc_i) * idf_i
      return(tf_idf_i)
    }
    each_term_out <-
      purrr::map_dbl(1:length(grep_terms), ~ each_term(.x))
    tf_idf_avg <- mean(each_term_out, na.rm = TRUE)
    return(tf_idf_avg)
  }
  tf_out <- purrr::map_dbl(1:n_doc, ~ tf(.x))
  tf_idf <- data.frame("doc" = c(1:n_doc),
                       "tf-idf" = tf_out)
  tf_idf <- tf_idf[order(tf_idf[, 2], decreasing = TRUE), ]
  return(tf_idf)
}



