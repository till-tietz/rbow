#'plot results of bow_analysis with bow_ci confidence intervals
#'
#'@param plot_data output of create_plot_data function
#'@return a ggplot object
#'@export



ci_plot <- function(plot_data){

  plot <- ggplot2::ggplot(plot_data)+
    ggplot2::geom_point(ggplot2::aes(x = observed_mean, y = pseudo_y, shape = factor(phenomenon)), size = 1.5)+
    ggplot2::geom_segment(ggplot2::aes(x = plot_data[,2], y = plot_data[,6], xend = plot_data[,3], yend = plot_data[,6]), size = 0.2) +
    ggplot2::xlab("mean occurence") +
    ggplot2::theme_bw() +
    ggplot2::scale_y_continuous(limit = c((min(plot_data[,"pseudo_y"])-0.1),(max(plot_data[,"pseudo_y"])+0.1)),
                       breaks = c(1:length(unique(plot_data[,"descriptor"]))),
                       labels = unique(plot_data[,"descriptor"]))+
    ggplot2::theme(axis.title.y = ggplot2::element_blank(),
          legend.title = ggplot2::element_blank())

  return(plot)
}