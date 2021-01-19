#' Compute bootstrap CIs for the average descriptor occurrence metric of the bow_analysis function
#'
#' @param bow_analysis_output result list outputted by the bow_analysis function
#' @param bootstrap number of bootstrap samples to draw (default = 1000)
#' @param alpha alpha value to compute CIs with
#' @param window window size used in the bow_analysis function (default = 10)
#' @param per_occurrence per occurrence value used in the bow_analysis function (default = TRUE)
#' @param bootstrap_terms if TRUE the bootstrap function samples the set of all terms within a window of all phenomenon occurrences. If FALSE the boostrap function samples from a vector of descriptor counts for each phenomenon occurrence.
#' @return list of analzed texts. Each text list contains a data frame of observed means and CI bounds for each descriptor and phenomenon pair.
#' @export

bow_ci <-
  function(bow_analysis_output,
           bootstraps = 1000,
           alpha = 0.95,
           window = 10,
           per_occurrence = TRUE,
           bootstrap_terms = TRUE) {
    #convert window size to number of words captured by window
    window <- window * 2
    #set up loop over each text
    each_text <- function(x) {
      text_i <- bow_analysis_output[[x]]

      #set up loop over each phenomenon
      each_phenomenon <- function(x) {
        phenomenon_i <- text_i[[x]]

        #set up loop over each descriptor
        each_descriptor <- function(x) {
          #descriptor i is a vector of descriptor counts per occurrence of a phenomenon
          descriptor_i <- phenomenon_i[[2]][[x]][[1]]
          #extract observed mean
          mean_obs_i <- phenomenon_i[[1]][, x]
          descriptor_name <- names(phenomenon_i[[2]])[x]

          #control flow >> only compute bootstrap if there are observations to bootstrap
          if (all(c(!is.nan(c(
            descriptor_i, mean_obs_i
          )), !anyNA(c(
            descriptor_i, mean_obs_i
          )))) == TRUE) {
            #we can choose to output the descriptor counts as a ratio of
            #phenomenon occurrences or total text length
            if (per_occurrence == TRUE) {
              divisor <- length(descriptor_i)
            } else {
              divisor <- length(descriptor_i) * (window)
            }

            #control flow >> controls type of bootstrapping to performs
            #bootstrap terms samples from a vector of all words within the window of all phenomenon occurrenes
            #boostrap occurrence sum samples from a vector of descriptor counts for each phenomenon occurrence
            if (bootstrap_terms == TRUE) {
              #get the number of occurrences of descriptors
              n_one <- sum(descriptor_i, na.rm = TRUE)
              length_vec <- (length(descriptor_i)) * window
              prob_one <- n_one/length_vec
            } else {
              n_one <- sum(descriptor_i > 0, na.rm = TRUE)
              length_vec <- length(descriptor_i)
              prob_one <- n_one/length_vec
            }

            bootstrap_occurrence <- function(pr, length) {
              sum_vec <- stats::rbinom(1, length, pr)/divisor
              return(sum_vec)
            }
            #execute bootstrap
            bstrap_out <-
              furrr::future_map_dbl(1:bootstraps,
                                    ~ bootstrap_occurrence(pr = prob_one, length = length_vec))

            #define upper and lower percentiles based on alpha value
            lperc <- (1 - alpha) / 2
            uperc <- (alpha + (1 - alpha) / 2)

            #get quantiles of bootstrap distribution
            bstrap_calc <-
              quantile(as.vector(bstrap_out - mean_obs_i), c(lperc, uperc))
            #compute upper and lower ci bounds
            ci_low <- mean_obs_i - bstrap_calc[[2]]
            ci_high <- mean_obs_i - bstrap_calc[[1]]

            #define df with output
            bstrap_i_out <- data.frame(
              "observed_mean" = mean_obs_i,
              "ci_low_bound" = ci_low,
              "ci_high_bound" = ci_high
            )

          } else {
            #define df with ouput for texts without observations
            bstrap_i_out <- data.frame(
              "observed_mean" = mean_obs_i,
              "ci_low_bound" = NA,
              "ci_high_bound" = NA
            )
          }
          bstrap_i_out$descriptor <- descriptor_name
          return(bstrap_i_out)
        }
        #execute loop over descriptors
        each_descriptor_out <-
          purrr::map_dfr(1:length(phenomenon_i[[2]]), ~ each_descriptor(.x))
        return(each_descriptor_out)
      }
      #execute loop over phenomena
      each_phenomenon_out <-
        purrr::map_dfr(1:length(text_i), ~ each_phenomenon(.x))
      each_phenomenon_out$phenomenon <-
        rep(names(text_i), each = (nrow(each_phenomenon_out) / length(text_i)))
      return(each_phenomenon_out)
    }
    #execute loop over texts
    if (length(bow_analysis_output) > 1) {
      each_text_out <-
        furrr::future_map(1:length(bow_analysis_output),
                          ~ each_text(.x),
                          .progress = TRUE)
    } else {
      each_text_out <-
        purrr::map(1:length(bow_analysis_output), ~ each_text(.x))
    }
    names(each_text_out) <- names(bow_analysis_output)
    return(each_text_out)
  }
