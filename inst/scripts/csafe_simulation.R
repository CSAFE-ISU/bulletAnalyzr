library(dplyr)
library(tidyr)
library(bulletxtrctr)
library(x3ptools)
library(randomForest)
library(ggplot2)
library(readr)
library(nbtrd)
library(purrr)

# Helper function to download data if not already present
prepare_data <- function(data_dir) {
    if (!dir.exists(dirname(data_dir))) {
        dir.create(dirname(data_dir), recursive = TRUE)
    }
    
    NRBTDsample_download("README_files/data")
}

compute_bullet_comparisons <- function(b1_path, b2_path, b1_lands = 1, b2_lands = 1) {
    print(paste0("Beginning comparison of ", b1_path, " and ", b2_path))
    # Load and prepare data
    # prepare_data(b1_path)
    b1 <- read_bullet(b1_path)
    b2 <- read_bullet(b2_path)
    b1$bullet <- 1
    b2$bullet <- 2
    b1$land <- 1:b1_lands
    b2$land <- 1:b2_lands
    
    # Continue with the rest of the analysis
    bullets <- bind_rows(b1, b2)
    
    # Processing the bullet data
    bullets <- bullets %>%
        mutate(
            x3p = purrr::map(x3p, x3p_m_to_mum),
            x3p = purrr::map(x3p, ~ .x %>% rotate_x3p(angle = -90) %>% y_flip_x3p()),
            crosscut = purrr::map_dbl(x3p, x3p_crosscut_optimize),
            ccdata = purrr::map2(x3p, crosscut, x3p_crosscut),
            grooves = purrr::map(ccdata, cc_locate_grooves, method = "middle", adjust = 30, return_plot = TRUE),
            sigs = purrr::map2(ccdata, grooves, ~ cc_get_signature(ccdata = .x, grooves = .y, span1 = 0.75, span2 = 0.03)),
            bulletland = paste0(bullet, "-", land)
        )
    
    # Prepare for comparisons
    lands <- unique(bullets$bulletland)
    comparisons <- expand.grid(land1 = lands, land2 = lands, stringsAsFactors = FALSE) %>%
        mutate(
            aligned = purrr::map2(land1, land2, ~ {
                land1_sig <- bullets$sigs[bullets$bulletland == .x][[1]]
                land2_sig <- bullets$sigs[bullets$bulletland == .y][[1]]
                sig_align(land1_sig$sig, land2_sig$sig)
            }),
            ccf0 = purrr::map_dbl(aligned, ~ extract_feature_ccf(.x$lands)),
            lag0 = purrr::map_dbl(aligned, ~ extract_feature_lag(.x$lands)),
            D0 = purrr::map_dbl(aligned, ~ extract_feature_D(.x$lands)),
            length0 = purrr::map_dbl(aligned, ~ extract_feature_length(.x$lands)),
            overlap0 = purrr::map_dbl(aligned, ~ extract_feature_overlap(.x$lands)),
            striae = purrr::map(aligned, sig_cms_max, span = 75)
        )
    
    # Calculate resolution
    resolution <- bullets$x3p[[1]] %>% x3p_get_scale()
    
    # Extract features for comparisons
    comparisons <- comparisons %>%
        mutate(
            cms_per_mm = purrr::map2_dbl(striae, aligned, ~ extract_feature_cms_per_mm(.x$lines, .y$lands, resolution)),
            matches0 = purrr::map_dbl(striae, ~ bulletxtrctr:::extract_helper_feature_n_striae(.x$lines, type = "peak", match = TRUE)),
            mismatches0 = purrr::map_dbl(striae, ~ bulletxtrctr:::extract_helper_feature_n_striae(.x$lines, type = "peak", match = FALSE)),
            bulletA = gsub("([1-2])-([1-6])","\\1", land1),
            bulletB = gsub("([1-2])-([1-6])","\\1", land2),
            landA = gsub("([1-2])-([1-6])","\\2", land1),
            landB = gsub("([1-2])-([1-6])","\\2", land2),
            features = purrr::map2(aligned, striae, extract_features_all, resolution),
            legacy_features = purrr::map(striae, extract_features_all_legacy, resolution)
        )
    
    # Scaling and preparing features
    features <- comparisons %>%
        select(land1, land2, ccf0, bulletA, bulletB, landA, landB, features) %>%
        unnest(features) %>%
        mutate(
            cms = cms_per_mm,
            matches = matches_per_mm,
            mismatches = mismatches_per_mm,
            non_cms = non_cms_per_mm
        ) %>%
        filter(bulletA != bulletB)
    
    # Use random forest to score
    features$rfscore <- predict(rtrees, newdata = features, type = "prob")[,2]
    
    return(features)
}

# Main function to analyze bullet comparisons
analyze_bullet_comparisons <- function(features, barrel1 = NULL, barrel2 = NULL, bullet1 = NULL, bullet2 = NULL, indices_b1 = NULL, indices_b2 = NULL) {
    print(paste0("Processing ", barrel1[1], bullet1[1], " vs. ", barrel2[1], bullet2[1], " comparison between N=", paste(indices_b1, collapse = ","), " and M=", paste(indices_b2, collapse = ","), " lands"))
    
    # Group bullet scores
    bullet_scores <- features %>%
        mutate(landA = as.numeric(landA), landB = as.numeric(landB)) %>%
        mutate(join_b1 = paste(indices_b1, collapse = ","),
               join_b2 = paste(indices_b2, collapse = ",")) %>%
        filter(landA %in% indices_b1, landB %in% indices_b2) %>%
        filter(bulletA == unique(bulletA)[1], bulletB == unique(bulletB)[1]) %>%
        mutate(diagonal_id = 1 + (6 + (landA - landB)) %% 6) %>%
        group_by(diagonal_id) %>%
        summarise(rfscore = mean(rfscore))
        
    return(bullet_scores)
}

# Helper function to get unique ordered combinations
get_unique_ordered_combinations <- function(n) {
    unlist(lapply(1:n, function(size) combn(n, size, simplify = FALSE)), recursive = FALSE)
}

# Generate unique ordered combinations for b1 and b2
b1_indices_combinations <- get_unique_ordered_combinations(1)
b2_indices_combinations <- get_unique_ordered_combinations(1)

get_bullet_lands <- function(barrel1, barrel2, bullet1, bullet2) {
    # Find bullet lands for a given barrel number
    # Pattern is "br1_1_land1.x3p" where first 1 is barrel, second is bullet
    
    # Get all files in the directory
    files <- list.files("~/Downloads/CSAFE Data")
    
    # Filter files for the given barrel numbers and bullet numbers
    # b1_files <- grep(paste0("br", barrel1, "_", bullet1, "_land"), files, value = TRUE)
    # b2_files <- grep(paste0("br", barrel2, "_", bullet2, "_land"), files, value = TRUE)
    # b1_files <- grep(paste0("Br", barrel1, " Bullet ", bullet1, "-"), files, value = TRUE)
    # b2_files <- grep(paste0("Br", barrel2, " Bullet ", bullet2, "-"), files, value = TRUE)
    # b1_files <- grep(paste0("EvoFinder PGPD Barrel ", barrel1, "-", bullet1), files, value = TRUE)
    # b2_files <- grep(paste0("EvoFinder PGPD Barrel ", barrel2, "-", bullet2), files, value = TRUE)
    # b1_files <- grep(paste0("Sensofar_CC PGPD Barrel ", barrel1, "-", bullet1), files, value = TRUE)
    # b2_files <- grep(paste0("Sensofar_CC PGPD Barrel ", barrel2, "-", bullet2), files, value = TRUE)
    # b1_files <- grep(paste0("phoenix-", barrel1, "-", "B", bullet1), files, value = TRUE)
    # b2_files <- grep(paste0("phoenix-", barrel2, "-", "B", bullet2), files, value = TRUE)
    b1_files <- grep(paste0(barrel1), files, value = TRUE)
    b2_files <- grep(paste0(barrel2), files, value = TRUE)
    
    # Create a temp directory for each bullet separately, copy all files
    # for that bullet to the temp directory, and return the temp directory
    # Make subdirectorys in the tmpdir()
    dir1_path <- file.path(tempdir(), b1_files[1])
    if (file.exists(dir1_path)) {
        unlink(dir1_path, recursive = TRUE)
    }
    dir.create(dir1_path)
    
    dir2_path <- file.path(tempdir(), b2_files[1])
    if (file.exists(dir2_path)) {
        unlink(dir2_path, recursive = TRUE)
    }
    dir.create(dir2_path)
    
    sapply(b1_files, function(x) {
        file.copy(file.path("~/Downloads/CSAFE Data", x), dir1_path)
    })
    
    sapply(b2_files, function(x) {
        file.copy(file.path("~/Downloads/CSAFE Data", x), dir2_path)
    })
    
    return(c(dir1_path, dir2_path))
}

# Get every combination of barrel1, barrel2, bullet1, bullet2

# Define barrels and bullets
# barrels <- 1:10
barrels <- c("Beretta 92F-2", "Beretta 92F 9mm", "Lorcin L9MM-2",
             "Lorcin L9MM-CSAFE", "Ruger P95DC-2", "Ruger P95DC-CSAFE")
bullets <- 1

# Stats on this
num_barrels <- length(barrels)
num_bullets <- num_barrels * length(bullets)

# Generate all combinations
combinations <- expand.grid(
    barrel1 = barrels,
    barrel2 = barrels,
    bullet1 = bullets,
    bullet2 = bullets
)

combinations <- tibble(
    barrel1 = c("Beretta 92F 9mm", "Beretta 92F-2", "Lorcin L9MM-2", "Lorcin L9MM-CSAFE", "Ruger P95DC-2", "Ruger P95DC-CSAFE"),
    barrel2 = c("Beretta 92F 9mm", "Beretta 92F-2", "Lorcin L9MM-2", "Lorcin L9MM-CSAFE", "Ruger P95DC-2", "Ruger P95DC-CSAFE"),
    bullet1 = 1,
    bullet2 = 1,
)

# Compute the full bullet-level features once
results_tibble <- tibble(
    input = combinations,
    output = pmap(combinations, ~ get_bullet_lands(..1, ..2, ..3, ..4))
) %>% 
    unnest(input) %>%
    mutate(b1id = paste0(barrel1, bullet1), b2id = paste0(barrel2, bullet2)) %>%
    mutate(b1id = factor(b1id, labels = 1:length(unique(b1id))), b2id = factor(b2id, labels = 1:length(unique(b2id)))) %>%
    right_join(
        as_tibble(t(combn(num_bullets, 2)), .name_repair = "universal") %>%
            rename(b1id = `...1`, b2id = `...2`) %>%
            mutate(b1id = factor(b1id), b2id = factor(b2id))
    )

# Compute features on the pair
features_df <- results_tibble %>%
    mutate(
        features = map(output, ~ compute_bullet_comparisons(.x[1], .x[2]))
    )

save(features_df, file = "fullbullet_features_df.RData")
load("fullbullet_features_df.RData")

bind_features <- features_df %>% mutate(features_row = row_number()) %>% select(-output) %>%
    select(barrel1, barrel2, bullet1, bullet2, b1id, b2id, features) %>%
    right_join(
        as_tibble(t(combn(num_bullets, 2)), .name_repair = "universal") %>%
            rename(b1id = `...1`, b2id = `...2`) %>%
            mutate(b1id = factor(b1id), b2id = factor(b2id))
    ) %>%
    arrange(b1id, b2id)

# Run the simulation study across all combinations of index subsets for b1 and b2
simulation_results_temp <- tidyr::expand_grid(
    features_row = 1:nrow(bind_features),  # Index for each row in results_tibble
    b1_indices = b1_indices_combinations,
    b2_indices = b2_indices_combinations
) %>%
    # Join the features from results_tibble based on features_row
    left_join(bind_features %>% mutate(features_row = row_number()), by = "features_row") %>%
    # Map over features, b1_indices, and b2_indices
    mutate(results = purrr::pmap(
        list(features, barrel1, barrel2, bullet1, bullet2, b1_indices, b2_indices),
        ~ analyze_bullet_comparisons(
            features = ..1,
            barrel1 = ..2,
            barrel2 = ..3,
            bullet1 = ..4,
            bullet2 = ..5,
            indices_b1 = ..6,
            indices_b2 = ..7
        )
    )) %>%
    rowwise() %>%
    mutate(
        join_b1 = paste(b1_indices, collapse = ","),
        join_b2 = paste(b2_indices, collapse = ",")
    )

# Compute the optimal alignment
results_temp <- simulation_results_temp %>%
    select(features_row, join_b1, join_b2, results) %>%
    unnest(results) %>%
    select(features_row, join_b1, join_b2, diagonal_id, rfscore) %>%
    arrange(features_row, join_b1, join_b2, diagonal_id)

# Generate a feature set for a new model, possibly
features_temp <- simulation_results_temp %>%
    distinct(barrel1, barrel2, bullet1, bullet2, .keep_all = TRUE) %>%
    unnest(features) %>%
    select(barrel1, barrel2, bullet1, bullet2, land1, land2, ccf:sum_peaks) %>%
    mutate(match = ifelse(barrel1 == barrel2, 1, 0))

save(features_temp, file = "fullbullet_features_temp.RData")

# Produce the final simulation results
simulation_results <- simulation_results_temp %>%
    left_join(results_temp, by = c("features_row", "join_b1", "join_b2")) %>%
    select(barrel1, barrel2, bullet1, bullet2, b1_indices, b2_indices,
           diagonal_id, rfscore) %>%
    rowwise() %>%
    mutate(
        b1_lands = paste(b1_indices, collapse = ","),
        b2_lands = paste(b2_indices, collapse = ",")
    ) %>%
    select(barrel1, barrel2, bullet1, bullet2, b1_lands, b2_lands, diagonal_id,
           rfscore) %>%
    ungroup()

# Display the simulation results
simulation_results
write_csv(simulation_results, "fullbullet_bullet_degradation_raw_simulation_results.csv")
simulation_results <- read_csv("fullbullet_bullet_degradation_raw_simulation_results.csv") %>%
    mutate(
        b1_lands = strsplit(as.character(b1_lands), ""),
        b2_lands = strsplit(as.character(b2_lands), "")
    ) %>%
    rowwise() %>%
    mutate(
        b1_lands = paste(b1_lands, collapse = ","),
        b2_lands = paste(b2_lands, collapse = ",")
    ) %>%
    ungroup()

# Process the simulation results
final_results_full <- simulation_results %>%
    mutate(
        num_lands_b1 = sapply(b1_lands, function(x) length(strsplit(x, ",")[[1]])),
        num_lands_b2 = sapply(b2_lands, function(x) length(strsplit(x, ",")[[1]])),
        match = barrel1 == barrel2,
        consecutive = sapply(b1_lands, function(x) all(diff(as.numeric(strsplit(x, ",")[[1]])) == 1)) &
            sapply(b2_lands, function(x) all(diff(as.numeric(strsplit(x, ",")[[1]])) == 1))
    ) %>%
    mutate(
        score = rfscore * ((num_lands_b1 * num_lands_b2) / (max(num_lands_b1) * max(num_lands_b2))),
        sqrt_score = rfscore * (sqrt(num_lands_b1 * num_lands_b2) / sqrt((max(num_lands_b1) * max(num_lands_b2)))),
        log_score = rfscore * (log(num_lands_b1 * num_lands_b2) / log((max(num_lands_b1) * max(num_lands_b2))))
    )
write_csv(final_results_full, "fullbullet_bullet_degradation_full_simulation_results.csv")
    
# Aggregate across simulation counts of b1 and b2
final_results <- final_results_full %>%
    filter(consecutive) %>%
    group_by(barrel1, barrel2, bullet1, bullet2, b1_lands, b2_lands, num_lands_b1, num_lands_b2, match) %>%
    summarise(rfscore = max(rfscore),
              score = max(score),
              sqrt_score = max(sqrt_score),
              log_score = max(log_score)) %>%
    group_by(N = num_lands_b1, M = num_lands_b2, match) %>%
    summarise(
        mean_rfscore = mean(rfscore),
        sd_rfscore = sd(rfscore),
        mean_score = mean(score),
        sd_score = sd(score),
        mean_sqrt_score = mean(sqrt_score),
        sd_sqrt_score = sd(sqrt_score),
        mean_log_score = mean(log_score),
        sd_log_score = sd(log_score)
    )
write_csv(final_results, "fullbullet_bullet_degradation_simulation_results.csv")

# Visualize the simulation results with ggplot2
ggplot(final_results %>% filter(match), aes(x = N, y = M, fill = mean_rfscore)) +
    geom_tile() +
    geom_text(aes(label = round(mean_rfscore, 3), 
                  color = ifelse(mean_rfscore >= .6, "white", "black")),
              fontface = "bold") +
    scale_x_continuous(breaks = 1:6) +
    scale_y_continuous(breaks = 1:6) +
    scale_fill_gradient2(low = "chocolate4", high = "darkgreen", mid = "lightgrey", midpoint = 0.5) +
    scale_color_identity() +
    labs(
        title = "Bullet Degradation Simulation Results",
        subtitle = "For Matching Sensofar Phoenix Bullets",
        x = "Number of Lands in Bullet 1",
        y = "Number of Lands in Bullet 2",
        fill = "Mean Bullet Score"
    ) +
    theme_minimal(base_size = 16) +
    theme(
        legend.position = "off"
    )
ggsave("phoenix_bullet_degradation_simulation_match_results.png", width = 7.7, height = 6, dpi = 300, bg = "white")

# Visualize the simulation results with ggplot2
ggplot(final_results %>% filter(!match), aes(x = N, y = M, fill = mean_rfscore)) +
    geom_tile() +
    geom_text(aes(label = round(mean_rfscore, 3), 
                  color = ifelse(mean_rfscore >= .6, "white", "black")),
              fontface = "bold") +
    scale_x_continuous(breaks = 1:6) +
    scale_y_continuous(breaks = 1:6) +
    scale_fill_gradient2(low = "chocolate4", high = "darkgreen", mid = "lightgrey", midpoint = 0.5) +
    scale_color_identity() +
    labs(
        title = "Bullet Degradation Simulation Results",
        subtitle = "For Non-Matching Sensofar Phoenix Bullets",
        x = "Number of Lands in Bullet 1",
        y = "Number of Lands in Bullet 2",
        fill = "Mean Bullet Score"
    ) +
    theme_minimal(base_size = 16) +
    theme(
        legend.position = "off"
    )
ggsave("phoenix_bullet_degradation_simulation_nonmatch_results.png", width = 7.7, height = 6, dpi = 300, bg = "white")

# Visualize the simulation results with ggplot2
ggplot(final_results %>% filter(match), aes(x = N, y = M, fill = mean_log_score)) +
    geom_tile() +
    geom_text(aes(label = round(mean_log_score, 3), 
                  color = ifelse(mean_log_score >= .6, "white", "black")),
              fontface = "bold") +
    scale_x_continuous(breaks = 1:6) +
    scale_y_continuous(breaks = 1:6) +
    scale_fill_gradient2(low = "chocolate4", high = "darkgreen", mid = "lightgrey", midpoint = 0.5) +
    scale_color_identity() +
    labs(
        title = "Weighted Bullet Degradation Simulation Results",
        subtitle = "For Matching Sensofar Phoenix Bullets",
        x = "Number of Lands in Bullet 1",
        y = "Number of Lands in Bullet 2",
        fill = "Mean Bullet Score"
    ) +
    theme_minimal(base_size = 16) +
    theme(
        legend.position = "off"
    )
ggsave("phoenix_bullet_degradation_simulation_match_results_weighted.png", width = 7.7, height = 6, dpi = 300, bg = "white")

# Visualize the simulation results with ggplot2
ggplot(final_results %>% filter(!match), aes(x = N, y = M, fill = mean_score)) +
    geom_tile() +
    geom_text(aes(label = round(mean_score, 3), 
                  color = ifelse(mean_score >= .6, "white", "black")),
              fontface = "bold") +
    scale_x_continuous(breaks = 1:6) +
    scale_y_continuous(breaks = 1:6) +
    scale_fill_gradient2(low = "chocolate4", high = "darkgreen", mid = "lightgrey", midpoint = 0.5) +
    scale_color_identity() +
    labs(
        title = "Weighted Bullet Degradation Simulation Results",
        subtitle = "For Non-Matching Sensofar Phoenix Bullets",
        x = "Number of Lands in Bullet 1",
        y = "Number of Lands in Bullet 2",
        fill = "Mean Bullet Score"
    ) +
    theme_minimal(base_size = 16) +
    theme(
        legend.position = "off"
    )
ggsave("phoenix_bullet_degradation_simulation_nonmatch_results_weighted.png", width = 7.7, height = 6, dpi = 300, bg = "white")

# Create a gradient background
gradient_data <- expand.grid(
    rfscore = seq(min(final_results_full$rfscore, na.rm = TRUE), 
                  max(final_results_full$rfscore, na.rm = TRUE), 
                  length.out = 100),
    y = seq(0, 1, length.out = 100)
)

# Prepare the data for the histogram
hist_data <- final_results_full %>%
    filter(consecutive) %>%
    group_by(barrel1, barrel2, bullet1, bullet2, b1_lands, b2_lands, num_lands_b1, num_lands_b2, match) %>%
    summarise(score = max(sqrt_score)) %>%
    mutate(num_lands_b1 = factor(num_lands_b1, levels = 6:1))

# Create gradient data spanning the full x and y range
gradient_data <- expand.grid(
    score = seq(0, 
                  1, 
                  length.out = 100),
    y = seq(0, 1, length.out = 100)
)

# Plot with gradient spanning the full panel
ggplot() +
    # Gradient background
    geom_tile(
        data = gradient_data,
        aes(x = score, y = y, fill = score),
        alpha = 0.5
    ) +
    scale_fill_gradient2(
        low = "chocolate4",
        high = "darkgreen",
        mid = "lightgrey",
        midpoint = 0.5,
        name = "Score"
    ) +
    # Overlay histogram
    geom_density(
        data = hist_data,
        aes(x = score, y = after_stat(density), colour = match),
        # binwidth = 0.01,
        alpha = 0.02,
        fill = "lightgrey",
        position = "dodge"
    ) +
    facet_grid(num_lands_b1 ~ num_lands_b2) +
    labs(
        title = "Distribution of Sqrt Weighted Bullet Scores",
        subtitle = "For All Sensofar Phoenix Bullets",
        x = "Weighted Score",
        y = "Distribution"
    ) +
    scale_colour_manual(values = c("chocolate4", "darkgreen")) +
    xlim(c(0, 1)) +
    theme_minimal(base_size = 16) +
    theme(
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8, angle = 45)
    )
ggsave("phoenix_bullet_degradation_simulation_histogram.png", width = 12, height = 8, dpi = 300, bg = "white")

simulation_results %>%
    mutate(beginning1 = sapply(strsplit(barrel1, " "), `[[`, 1),
           beginning2 = sapply(strsplit(barrel2, " "), `[[`, 1),
           match = beginning1 == beginning2) %>%
    ggplot(aes(x = rfscore, fill = match)) +
    geom_bar(position = "dodge") +
    scale_fill_manual(values = c("chocolate4", "darkgreen")) +
    xlim(c(0, 1)) +
    theme_minimal(base_size = 16) +
    theme(
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8, angle = 45)
    )
