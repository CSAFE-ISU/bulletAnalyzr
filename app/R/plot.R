groove_plot <- function(ccdata, grooves) {
  ccdata %>% 
    ggplot(aes(x = x, y = value)) + 
    #    theme_bw()+
    geom_vline(xintercept = 0, colour = "grey50") + 
    geom_vline(xintercept = grooves[2]-grooves[1], colour = "grey50") +
    geom_line(linewidth = .5) + # put signal in front
    annotate("rect", fill="grey50", alpha = 0.15, xmax = 0, xmin = -Inf, ymin = -Inf, ymax = Inf) +
    annotate("rect", fill="grey50", alpha = 0.15, xmax = Inf, xmin = grooves[2]-grooves[1], ymin = -Inf, ymax = Inf) +
    geom_line(linewidth = 1, data = filter(ccdata, between(x, 0, grooves[2]-grooves[1]))) + # put signal in front
    scale_x_continuous(
      breaks=c(0,round(as.numeric(grooves[2]-grooves[1]),0),round(seq(min(ccdata$x),max(ccdata$x),by=500),-2)),
      labels=c("\n0",paste0("\n",round(as.numeric(grooves[2]-grooves[1]),0)),round(seq(min(ccdata$x),max(ccdata$x),by=500),-2))
    ) +
    xlab("Position along width of Land [µm]") +
    ylab("Surface Height [µm]") 
}
