#' Create a document frequency matrix (sorted in descending order) of terms that occur within some window around a given set of words (phenomenon)
#'
#' @name dfm_analysis
#'
#' @param corpus the text or texts to be analyzed as a list of character vectors
#' @param phenomenon a list of character vectors (or list of regular expressions if own_regex == TRUE) with terms around which words will be counted for the dfm
#' @param window number of words left and right of a phenomenon term to be considered for the dfm
#' @param n_terms number of terms displayed in dfm
#' @param filter_dictionary a character vector (or regular expression if own_regex == TRUE) of words to select from the dfm
#' @param tf_idf if TRUE function computes tf-idf metric instead of raw counts
#' @param filter_ps if TRUE enables filtering of results by part of speech (i.e only adjectives and adverbs)
#' @param ps character vector of parts of speech to filter. see selection with unique(tidytext::parts_of_speech[,"pos"])
#' @param own_regex when TRUE allows you to add custom regular expressions for phenomenon and filter_dictionary. when FALSE rbow will construct regular expression from the character vectors you supplied. defaults to FALSE
#' @return list of dfms (one dfm per text in corpus)
#' @export

dfm_analysis <-
  function(corpus,
           phenomenon,
           window = 10,
           n_terms = 10,
           filter_dictionary = NULL,
           tf_idf = FALSE ,
           filter_ps = FALSE,
           ps = NULL,
           own_regex = FALSE) {

    if(own_regex == TRUE){
      phenomenon_grep <- phenomenon
    } else {
      phenomenon_grep <- rbow::grep_construct(text_input = phenomenon, collapse = TRUE)
    }

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
            text_window(
              text = corpus_i,
              start = min_position,
              end = max_position,
              position = positions,
              out_of_window = TRUE
            )


          if (tf_idf == FALSE) {
            #get the number of occurences for each term
            dfm <-
              as.data.frame(sort(table(unlist(
                analysis_text[[1]]
              )), decreasing = TRUE))

          } else {
            tf <-
              as.data.frame(table(unlist(analysis_text[[1]])))
            tf[, 2] <-
              tf[, 2] / length(unlist(analysis_text[[1]]))

            df <- as.numeric(unique(unlist(analysis_text[[1]])) %in% analysis_text[[2]]) + 1

            idf <-
              data.frame("term" = unique(unlist(analysis_text[[1]])),
                         "idf" = log(length(analysis_text) / df))

            dfm <-
              merge(tf,
                    idf,
                    by.x = colnames(tf)[1],
                    by.y = colnames(idf)[1])
            dfm[, "tf-idf"] <- dfm[, 2] * dfm[, 3]
            dfm <- dfm[, c(1,4)]
            dfm <- dfm[order(-dfm[,"tf-idf"]),]
          }

          if (!is.null(filter_dictionary)){

            if(own_regex == TRUE){
              filter_dict_grep <- filter_dictionary
            } else {
              filter_dict_grep <- rbow::grep_construct(text_input = filter_dictionary, collapse = TRUE)
            }

            filter_index <- grep(filter_dict_grep, dfm[,1])
            dfm <- dfm[filter_index,]
          }

          if (filter_ps == TRUE) {
            terms <-
              merge(dfm,
                    tidytext::parts_of_speech,
                    by.x = colnames(dfm)[1],
                    by.y = "word")
            terms <- unique(terms[which(terms[, "pos"] %in% ps), 1])

            dfm <- dfm[which(dfm[, colnames(dfm)[1]] %in% terms), ]
          }

          dfm <- dfm[c(1:n_terms),]

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
    names(each_text_out) <- names(corpus)
    return(each_text_out)
  }
