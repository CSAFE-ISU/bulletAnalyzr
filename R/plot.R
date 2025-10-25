#' Plot Crosscut Profile with Groove Locations
#'
#' Creates a ggplot showing the crosscut profile with groove locations marked
#' and excluded areas shaded.
#'
#' @param ccdata A data frame containing crosscut data with x and value columns
#' @param grooves A numeric vector of length 2 containing left and right groove positions
#'
#' @returns A ggplot object
#' @noRd
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


#' Plot All Crosscuts
#'
#' Creates a faceted plot showing crosscut profiles for all bullet lands.
#'
#' @param crosscuts A data frame containing crosscut data for multiple bullets and lands
#'
#' @returns A ggplot object with faceted crosscut profiles
#' @noRd
plot_all_crosscuts <- function(crosscuts) {
  # Prevent no visible binding for global variable note
  x <- value <- bullet <- land <- NULL
  
  crosscuts$x <- crosscuts$x / 1000
  CCplot <- crosscuts %>% 
    ggplot2::ggplot(ggplot2::aes(x = x, y = value)) + 
    ggplot2::geom_line() +
    ggplot2::facet_grid(bullet ~ land, labeller = "label_both") +
    ggplot2::xlab("Position along width of Land [mm]") +
    ggplot2::ylab("Surface Height [µm]") + 
    ggplot2::ggtitle("Cross-section of the bullet land at a suitable cross-section location") 
  return(CCplot)
}

#' Plot All Bullet Signals
#'
#' Creates a faceted plot showing raw and smoothed signals for all bullet lands.
#'
#' @param bullets A data frame containing bullet data with nested signal information
#'
#' @returns A ggplot object with faceted signal plots
#' @noRd
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

#' Plot Bullet-to-Bullet Score Matrix
#'
#' Creates a heatmap showing bullet-to-bullet comparison scores.
#'
#' @param bullet_scores A data frame containing bullet comparison scores
#'
#' @returns A ggplot object showing the score matrix
#' @noRd
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

#' Plot Land-to-Land Score Matrix
#'
#' Creates a heatmap showing land-to-land comparison scores for two bullets.
#'
#' @param features A data frame containing land comparison features and scores
#'
#' @returns A ggplot object showing the land score matrix
#' @noRd
plot_land_score_matrix <- function(features) {
  # Prevent no visible binding for global variable note
  landA <- landB <- rfscore <- samesource <- NULL
  
  p <- features %>% 
    ggplot2::ggplot(ggplot2::aes(x = landA, y = landB, fill = rfscore, colour = samesource)) +
    ggplot2::geom_tile() +
    ggplot2::labs(fill = "Land Score") +
    ggplot2::scale_fill_gradient2(low = "grey80", high = "darkorange", midpoint = .5, limits = c(0,1)) +
    ggplot2::scale_colour_manual(values = c("black", "black")) +
    ggplot2::geom_tile(linewidth = 1, data = features %>% dplyr::filter(samesource == TRUE)) +
    ggplot2::geom_text(ggplot2::aes(label = round(rfscore, 2)), size = 6) +
    ggplot2::xlab(sprintf("Lands on %s", features$bulletA[1])) +
    ggplot2::ylab(sprintf("Lands on %s", features$bulletB[1])) + 
    ggplot2::ggtitle("Land-to-Land Score Matrix",
                     subtitle = sprintf("Bullet: %s vs %s", features$bulletA[1], features$bulletB[1])) + 
    ggplot2::guides(colour = "none") +
    ggplot2::coord_equal()
  return(p)
}

#' Plot Crosscut Profile with Groove Lines
#'
#' Creates a faceted plot of crosscut profiles with vertical lines marking groove
#' locations.
#'
#' @param df A data frame containing profile data with x and value columns
#' @param left_groove A numeric value for the left groove position
#' @param right_groove A numeric value for the right groove position
#'
#' @returns A ggplot object showing the profile with groove markers
#' @noRd
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

#' Plot Aligned Signals
#'
#' Creates a plot showing two aligned land engraved area (LEA) signals.
#'
#' @param sig_plot_data A data frame containing signal data for two lands
#' @param scale A numeric value for the x-axis scale
#'
#' @returns A ggplot object showing aligned signals
#' @noRd
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
