# Load packages
library(tidyverse)
library(tidymodels)

tidymodels_prefer()

# Read modeling dataset
hab_model_df <- read_csv(
  "data/processed/hab_nlp_modeling_data_grouped.csv"
)

# Create bulletin-level grouping variable
# A bulletin is classified as having an action if at least one of its
# text segments received the public_health_action label.
hab_model_df <- hab_model_df |>
  group_by(bulletin_id) |>
  mutate(
    bulletin_has_action = any(public_health_action == "public_health_action")
  ) |>
  ungroup() |>
  mutate(
    bulletin_has_action = factor(if_else(bulletin_has_action, "has_action", "no_action"))
  )

# Split data into training and testing sets
# Grouping by bulletin_id prevents segments from the same bulletin
# from appearing in both datasets.
set.seed(321)

hab_split <- group_initial_split(
  hab_model_df,
  group = bulletin_id,
  prop = 0.80,
  strata = bulletin_has_action
)

# Extract training and testing datasets
hab_train <- training(hab_split)
hab_test <- testing(hab_split)

# Summarize the training and testing datasets
split_summary <- bind_rows(
  hab_train |>
    summarise(dataset = "Training", segments = n(), bulletins = n_distinct(bulletin_id)),
  hab_test |>
    summarise(dataset = "Testing", segments = n(), bulletins = n_distinct(bulletin_id))
)

split_summary

# Verify that no bulletin appears in both datasets
# This should return character(0), confirming no bulletin exists in both sets.
intersect(unique(hab_train$bulletin_id), unique(hab_test$bulletin_id))

# Check class distribution in each dataset
hab_train |> count(public_health_action)
hab_test |> count(public_health_action)

# Save the split object
saveRDS(
  hab_split,
  "data/processed/hab_grouped_train_test_split.rds"
)

