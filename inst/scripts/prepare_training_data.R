library(tidyverse)

# Get all RData files with "features_df" in the name, store in separate variables
files <- list.files(pattern = "features_df.*\\.RData$")
# Load each file and store in a list, keeping in mind its RData
# Include the file name
file_list <- map(files, ~ { 
  load(.x)
  tibble(
    file = .x,
    features = get("features_df")
  ) %>%
      unnest(features) %>%
      mutate(barrel1 = as.character(barrel1), barrel2 = as.character(barrel2))
})

# Bind the file list into a single data frame, converting barrel cols to char
file_df <- bind_rows(file_list)

# Extract the `features` column and bind in a big dataframe with the file name
features <- map_dfr(file_list, ~ {
  tibble(
    file = .x$file,
    features = .x$features
  )
}) %>%
    unnest(features) %>%
    select(-land1, -land2, -ccf0)

## Output #1: List of Datasets
files

## Output #2: Data Frame with Features
write_csv(features, "land_training_data.csv")

## Output #3: Corrections required
# Rotation required for Hamby 252, automatically performed

# Bind them all together with tidyverse and bind_rows
all_data <- files %>%
  map_dfr(~ read_csv(.x, col_types = cols(.default = "c")))

# Get all land to land comparisons