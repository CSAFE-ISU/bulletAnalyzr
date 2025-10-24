groove_plot <- function(ccdata, grooves) {
  # Prevent no visible binding for global variable note
  x <- value <- NULL
  
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

plot_bullet_score_matrix <- function(bullet_scores) {
  # Prevent no visible binding for global variable note
  bulletA <- bulletB <- bullet_score <- selsource <- NULL
  
  p <- bullet_scores %>% 
    ggplot2::ggplot(ggplot2::aes(x = bulletA, y = bulletB, fill = bullet_score, colour = selsource)) +
    ggplot2::geom_tile() +
    ggplot2::labs(fill = "Bullet Score") +
    ggplot2::scale_fill_gradient2(low = "grey80", high = "darkorange", midpoint = .5, limits = c(0,1)) +
    ggplot2::scale_colour_manual(values = c("black", "black")) +
    ggplot2::geom_tile(linewidth = 1, data = bullet_scores %>% dplyr::filter(selsource)) +
    ggplot2::geom_text(ggplot2::aes(label = round(bullet_score, 2)), size = 6) +
    ggplot2::ggtitle("Bullet-to-Bullet Score Matrix") +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::guides(colour = "none") +
    ggplot2::coord_equal() 
  return(p)
}

plot_all_signals <- function(bullets) {
  # Prevent no visible binding for global variable note
  source <- bullet <- land <- sigs <- x <- sig <- raw_sig <- NULL
  
  signatures <- bullets %>% dplyr::select(source, bullet, land, sigs) %>% tidyr::unnest(sigs)
  signatures$x <- signatures$x / 1000
  Sigplot <- signatures %>% 
    dplyr::filter(!is.na(sig), !is.na(raw_sig)) %>%
    ggplot2::ggplot(ggplot2::aes(x = x)) + 
    ggplot2::geom_line(ggplot2::aes(y = raw_sig), colour = "grey70", show.legend = T) +
    ggplot2::geom_line(ggplot2::aes(y = sig), colour = "grey30", show.legend = T) +
    ggplot2::facet_grid(bullet ~ land, labeller = "label_both") +
    ggplot2::ylim(c(-5, 5)) +
    ggplot2::xlab("Position along width of Land [mm]") +
    ggplot2::ylab("Signal [µm]") +
    ggplot2::ggtitle("Raw and LOESS-smoothed Signal for Bullet Profile")
  return(Sigplot)
}

plot_profile <- function(df, left_groove, right_groove) {
  # Prevent no visible binding for global variable note
  x <- value <- NULL
  
  p <- df %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = value)) + 
    ggplot2::geom_line() +
    ggplot2::geom_vline(xintercept = left_groove, color = "red") +
    ggplot2::geom_vline(xintercept = right_groove, color = "red") +
    ggplot2::facet_grid(bullet~land, labeller="label_both") +
    ggplot2::theme_bw()
  return(p)
}

plot_signal <- function(sig_plot_data, scale) {
  # Prevent no visible binding for global variable note
  x <- value <- Signal <- NULL
  
  ggplot2::ggplot(sig_plot_data, ggplot2::aes(x = x*scale, y = value, colour = Signal, linetype = Signal)) + 
    ggplot2::geom_line(na.rm = TRUE, alpha = 0.9, linewidth = 1) +
    ggplot2::scale_color_manual(values = c("darkorange", "purple4")) + 
    ggplot2::xlab("Position along width of Land [µm]") +
    ggplot2::ylab("Signal [µm]") +
    ggplot2::ggtitle("Aligned signals of LEAs")+
    ggplot2::theme(legend.position = "bottom") 
}
