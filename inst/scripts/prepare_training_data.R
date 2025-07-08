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
    barrel1 = .x$barrel1,
    barrel2 = .x$barrel2,
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

###
# Model Training
###
features_raw <- read_csv("land_training_data.csv")

# Get the top score for every pair
top_scores <- features_raw %>%
  group_by(file, barrel1, barrel2, bulletA, bulletB, landA) %>%
  summarise(topscore = max(rfscore))

features_scores <- features_raw %>%
  left_join(top_scores) %>%
  group_by(file, bulletA, bulletB, landA) %>%
  mutate(match = rfscore == topscore & barrel1 == barrel2)  # TODO: Ground truth

# We need to remove certain columns
features <- features_scores %>%
  ungroup() %>%
  select(-file, -barrel1, -barrel2, -bulletA, -bulletB, -landA, -landB, -rfscore, -topscore)

# Split into an 80% training 20% test set
set.seed(123)  # For reproducibility
train_indices <- sample(1:nrow(features), size = 0.8 * nrow(features))
train_set <- features[train_indices, ]
test_set <- features[-train_indices, ]

# Now fit a random forest
library(randomForest)

rf_model <- randomForest(
  factor(match) ~ .,
  data = train_set,
  ntree = 100,  # Number of trees
  importance = TRUE
)

importance(rf_model)
