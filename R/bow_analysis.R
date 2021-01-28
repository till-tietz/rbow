#' Compute how often a given set of words (descriptors) occur within a given window around
#' another set of words (phenomena) within a text. This can be computed as a ration of n occurrences descriptor/n total words in window or
#' n occurrences descriptor / n occurrences phenomena.
#'
#' @name bow_analysis
#' @title bow_analysis
#' @param corpus the text or texts to be analyzed as a list of character vectors
#' @param phenomenon a list of character vectors (or list of regular expressions if own_regex == TRUE) with terms around which descriptor terms are searched
#' @param descriptors a list of characters vectors (or list of regular expressions if own_regex == TRUE) with descriptor terms to search
#' @param window number of words left and right of a phenomenon term to be searched for a descriptor
#' @param per_occurrence when TRUE divide the number of descriptor occurrences by the number of phenomena occurrences. when FALSE divide the number of descriptor occurrences by the total number of words within the windows around phenomena. Default TRUE.
#' @param own_regex when TRUE allows you to add custom regular expressions for phenomenon and descriptors. when FALSE rbow will construct regular expression from the character vectors you supplied. defaults to FALSE
#' @return list of analyzed texts. Each text list contains a list of results for each analyzed phenomenon. each phenomenon list contains a data frame summarizing the average number of descriptors occurrences for that phenomenon and a list of numeric vectors for each descriptor indicating the number of descriptor terms within a window for each phenomenon occurrence.
#' @export

bow_analysis <-
  function(corpus,
           phenomenon,
           descriptors,
           window = 10,
           per_occurrence = TRUE,
           own_regex = FALSE) {

    if(own_regex == TRUE){
      descriptors_grep <- descriptors

      phenomenon_grep <- phenomenon
    } else {
      #turn descriptors into regex
      descriptors_grep <- rbow::grep_construct(text_input = descriptors, collapse = TRUE)

      #turn phenomena into regex
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
          # -18 thus causing an error in subsetting the text vector)
          min_position[which(min_position <= 0)] <- 1
          #define upper bound of window
          max_position <- positions + window
          #limit upper bound to length of text (same reason as above)
          max_position[which(max_position > length(corpus_i))] <-
            length(corpus_i)
          #get text snippets around phenomena
          analysis_text <-
            text_window(text = corpus_i,
                        start = min_position,
                        end = max_position,
                        position = positions)

          #set up loop over each descriptor
          each_descriptor <- function(x) {
            descriptor_i <- descriptors_grep[[x]]

            #loop over each text snippet around a phenomenon
            #this is slightly less efficient than concatenating the text and running the analysis
            #but it gives us simple numeric vectors of descriptor occurences to sample from
            #for the bootstrap thus making the bootstrap function much faster
            loop_analysis_text <- function(x) {
              count_i <- length(grep(descriptor_i, analysis_text[[x]]))
              return(count_i)
            }
            count_vec <-
              purrr::map_dbl(1:length(analysis_text), ~ loop_analysis_text(.x))
            count <- sum(count_vec, na.rm = TRUE)

            #we can choose to output the descriptor counts as a ratio of
            #phenomenon occurrences or total text length
            if (per_occurrence == TRUE) {
              ratio <- count / length(analysis_text)
            } else {
              ratio <- count / ((length(analysis_text)) * (window * 2))
            }
            return(list(ratio = ratio, counts = count_vec))
          }
          #execute loop over descriptors
          each_descriptor_out <-
            purrr::map(1:length(descriptors), ~ each_descriptor(.x))

          #get the descriptor occurrence ratios
          each_descriptor_ratio <-
            unlist(lapply(each_descriptor_out, `[[`, 1))
          #turn descriptor occurrence ratios into data frame columns
          each_descriptor_ratio <-
            as.data.frame(t(each_descriptor_ratio))
          #get a vector of descriptor counts for each phenomenon occurrence (this is used for the bootstrap)
          each_descriptor_counts <-
            lapply(each_descriptor_out, function(x)
              x[c(2:length(x))])
        } else {
          #fill the above outputs with NA when no phenomena occur in the text
          each_descriptor_ratio <-
            as.data.frame(t(rep(NA, length(
              descriptors
            ))))
          each_descriptor_counts <-
            as.list(rep(NA, length(descriptors)))
        }
        colnames(each_descriptor_ratio) <- names(descriptors)
        each_descriptor_ratio$phenomenon <- names(phenomenon[x])
        names(each_descriptor_counts) <- names(descriptors)

        output <- list(descriptor_mean = each_descriptor_ratio,
                       descriptor_counts = each_descriptor_counts)
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

