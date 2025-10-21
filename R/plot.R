groove_plot <- function(ccdata, grooves) {
  ccdata %>% 
    ggplot2::ggplot(ggplot2::aes(x = x, y = value)) + 
    ggplot2::geom_vline(xintercept = 0, colour = "grey50") + 
    ggplot2::geom_vline(xintercept = grooves[2]-grooves[1], colour = "grey50") +
    ggplot2::geom_line(linewidth = .5) + # put signal in front
    ggplot2::annotate("rect", fill="grey50", alpha = 0.15, xmax = 0, xmin = -Inf, ymin = -Inf, ymax = Inf) +
    ggplot2::annotate("rect", fill="grey50", alpha = 0.15, xmax = Inf, xmin = grooves[2]-grooves[1], ymin = -Inf, ymax = Inf) +
    ggplot2::geom_line(linewidth = 1, data = dplyr::filter(ccdata, dplyr::between(x, 0, grooves[2]-grooves[1]))) + # put signal in front
    ggplot2::scale_x_continuous(
      breaks=c(0,round(as.numeric(grooves[2]-grooves[1]),0),round(seq(min(ccdata$x),max(ccdata$x),by=500),-2)),
      labels=c("\n0",paste0("\n",round(as.numeric(grooves[2]-grooves[1]),0)),round(seq(min(ccdata$x),max(ccdata$x),by=500),-2))
    ) +
    ggplot2::xlab("Position along width of Land [µm]") +
    ggplot2::ylab("Surface Height [µm]") 
}

plot_signal <- function(sig_plot_data, scale) {
  ggplot2::ggplot(sig_plot_data, ggplot2::aes(x = x*scale, y = value, colour = Signal, linetype = Signal)) + 
    ggplot2::geom_line(na.rm = TRUE, alpha = 0.9, linewidth = 1) +
    ggplot2::scale_color_manual(values = c("darkorange", "purple4")) + 
    ggplot2::xlab("Position along width of Land [µm]") +
    ggplot2::ylab("Signal [µm]") +
    ggplot2::ggtitle("Aligned signals of LEAs")+
    ggplot2::theme(legend.position = "bottom") 
}
