#' Create a document frequency matrix (sorted in descending order) of terms that occur within some window around a given set of words (phenomenon)
#'
#' @param corpus the text or texts to be analyzed as a list of character vectors
#' @param phenomenon a list of character vectors with terms around which words will be counted for the dfm
#' @param window number of words left and right of a phenomenon term to be considered for the dfm
#' @param n_terms number of terms displayed in dfm
#' @return list of dfms (one dfm per text in corpus)
#' @export

dfm_analysis <- function(corpus, phenomenon, window, n_terms) {
  phenomenon_grep <-
    lapply(phenomenon, function(x)
      paste(x, sep = "", collapse = "|"))

  #set up loop over each text
  each_text <- function(x) {
    corpus_i <- corpus[[x]]

    #set up loop over each phenomenon
    each_phenomenon <- function(x) {
      phenomenon_i <- phenomenon_grep[[x]]

      #get position of occurences of phenomenon terms in text
      #(i.e "bla bla bla spy bla bla inspect" = c(4,7))
      positions <- grep(phenomenon_i, corpus_i)

      #set up control flow i.e only execute rest of analysis if phenomenon terms occur in text
      if (length(positions) > 0) {
        #define window around phenomenon occurences
        #define lower bound of window
        min_position <- positions - window
        #limit lower bound to 0
        #(otherwise a phenomenon at text position 2 with window = 20 would have a lower window bound
        # -18 thus causing an error in subsetting the text vector )
        min_position[which(min_position <= 0)] <- 1
        #define upper bound of window
        max_position <- positions + window
        #limit upper bound to length of text (same reason as above)
        max_position[which(max_position > length(corpus_i))] <-
          length(corpus_i)
        #get text snippets around phenomena
        analysis_text <-
          unlist(
            text_window(
              text = corpus_i,
              start = min_position,
              end = max_position,
              position = positions
            )
          )

        #get the number of occurences for each term
        dfm <-
          sort(table(analysis_text), decreasing = TRUE)[c(1:n_terms)]
      } else {
        dfm <- NA
      }
      return(dfm)
    }
    #execute loop over phenomena
    each_phenomenon_out <-
      purrr::map(1:length(phenomenon), ~ each_phenomenon(.x))
    names(each_phenomenon_out) <- names(phenomenon)
    return(each_phenomenon_out)
  }
  #execute loop over texts
  each_text_out <- purrr::map(1:length(corpus), ~ each_text(.x))
  return(each_text_out)
}
