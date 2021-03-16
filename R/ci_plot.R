#'plot results of bow_analysis with bow_ci confidence intervals
#'
#'@name ci_plot
#'
#'@param plot_data output of create_plot_data function
#'@param scale character vector specifying whether phenomena should be plotted with different colours or shapes. options are "shape" or "colour".
#'@return a ggplot object
#'@export



ci_plot <- function(plot_data, scale = "shape"){

  plot <- ggplot2::ggplot(plot_data)+
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = plot_data[,2], y = plot_data[,6], xmax = plot_data[,3], height = 0.09)) +
    ggplot2::xlab("mean occurence") +
    ggplot2::theme_bw() +
    ggplot2::scale_y_continuous(limit = c((min(plot_data[,"pseudo_y"])-0.1),(max(plot_data[,"pseudo_y"])+0.1)),
                       breaks = c(1:length(unique(plot_data[,"descriptor"]))),
                       labels = unique(plot_data[,"descriptor"]))+
    ggplot2::theme(axis.title.y = ggplot2::element_blank(),
          legend.title = ggplot2::element_blank())+
    ggplot2::ggtitle(plot_data[["text"]][1])

  if(scale == "shape"){
    plot <- plot+
      ggplot2::geom_point(ggplot2::aes(x = plot_data[,"observed_mean"], y = plot_data[,"pseudo_y"], shape = as.factor(plot_data[,"phenomenon"])), size = 1.5)
  }

  if(scale == "colour"){
    plot <- plot+
      ggplot2::geom_point(ggplot2::aes(x = plot_data[,"observed_mean"], y = plot_data[,"pseudo_y"], colour = as.factor(plot_data[,"phenomenon"])), size = 1.5)
  }

  return(plot)
}
