#'re-format output of bow_ci function to plottable format
#'
#'@param bstrap_output output of bow_ci function
#'@return data frame of plot data
#'@export

create_plot_data <- function(bstrap_output){
  n_descriptors <- unique(bstrap_output$descriptor)
  n_phenomena <- unique(bstrap_output$phenomenon)

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

  out <- bstrap_output
  out$pseudo_y <- pseudo_y
  return(out)
}
