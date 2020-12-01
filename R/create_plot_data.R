#'re-format output of bow_ci function to plottable format
#'
#'@param bstrap_output output of bow_ci function
#'@param meta_data a data frame of meta data for each analyzed text i.e year, author etc.
#'@param combine if TRUE combines plotting data for each text into one data frame; if false returns a data frame for each text; default FALSE
#'@return data frame or list of data frames
#'@export

create_plot_data <- function(bstrap_output, meta_data = NULL, combine = FALSE){
  each_text <- function(x){
    bstrap_output_i <- bstrap_output[[x]]
    n_descriptors <- unique(bstrap_output_i[,"descriptor"])
    n_phenomena <- unique(bstrap_output_i[,"phenomenon"])

    if(length(n_phenomena) %% 2 == 0){
      step_size <- (length(n_phenomena)/2)/10
      steps <- seq(from = 1 - step_size, to = 1 + step_size, by = 0.1)
      steps <- steps[!steps %in% 1]
    } else {
      step_size <- ((length(n_phenomena)-1)/2)/10
      steps <- seq(from = 1 - step_size, to = 1 + step_size, by = 0.1)
    }

    steps <- rep(steps, each = length(n_descriptors))
    pseudo <- rep(c(0:(length(n_descriptors)-1)), length(n_phenomena))
    pseudo_y <- steps + pseudo

    out <- bstrap_output_i
    out[,"pseudo_y"] <- pseudo_y

    if(!is.null(meta_data)){
      meta_df <- as.data.frame(sapply(meta_data[x,], function(x) rep(x, nrow(out))))
      out <- cbind(out, meta_df)
    } else {
      out <- out
    }
    return(out)
  }

  plot_data <- purrr::map(1:length(bstrap_output), ~each_text(.x))

  if(combine == TRUE){
    plot_data <- do.call(rbind, plot_data)
  } else {
    plot_data <- plot_data
  }
  return(plot_data)
}
