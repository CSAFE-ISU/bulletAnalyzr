library(tidyverse)

csafe <- read_csv("bullet_degradation_simulation_results.csv")

# Add confidence interval bounds
csafe_match <- csafe %>%
    mutate(scaledN = (1/3)*(N * M)/N, scaledM = (1/3)*(N * M)/M) %>%
    mutate(
        lower_bound = pmax(mean_score - sd_score / (scaledN * scaledM), 0),
        upper_bound = pmin(mean_score + sd_score / (scaledN * scaledM), 1),
        confidence = 1 / sd_score # Inverse SD to represent confidence
    ) %>%
    mutate(N = factor(N, levels = max(N):1)) %>%
    mutate(confidence = ifelse(is.infinite(confidence), 15, confidence)) %>%
    filter(match)

# Create the plot
ggplot(csafe_match, aes(x = factor(1), y = mean_score, color = log(confidence))) +
    geom_point() +
    geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound), linewidth = 0.7, width = .2) +
    scale_color_viridis_c(option = "D", name = "Confidence", guide = FALSE) +  # Pretty color scale
    #scale_size_continuous(name = "Confidence", range = c(1, 6), guide = FALSE) +
    #scale_y_reverse() +
    ylim(c(0, 1)) +
    geom_hline(yintercept = tail(csafe_match$mean_score, 1), linetype = "dashed", color = "red") +
    facet_grid(N ~ M) +  # 6x6 grid
    theme_minimal(base_size = 14) +
    theme(
        strip.text = element_text(size = 10, face = "bold"),
        axis.text.x = element_blank(),
        strip.text.y = element_text(angle = 0)
    ) +
    labs(
        title = "Score Confidence Intervals for NxM Results",
        subtitle = "Red dashed line showing 6x6 score",
        x = "Lands in Bullet",
        y = "NxM Score"
    )
