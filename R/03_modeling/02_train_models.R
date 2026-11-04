library(tidyverse)
library(tidymodels)
library(textrecipes)
library(naivebayes)

tidymodels_prefer()

# Read the saved grouped train/test split
hab_split <- readRDS(
  "data/processed/hab_grouped_train_test_split.rds"
)

# Extract training and testing datasets
hab_train <- training(hab_split)
hab_test <- testing(hab_split)

# Define text preprocessing recipe
# Tokenize the text, limit the vocabulary to the 1,000 most frequent tokens,
# and transform the tokens into TF-IDF features.
hab_recipe <- recipe(
  public_health_action ~ Text,
  data = hab_train
) |>
  step_tokenize(Text) |>
  step_tokenfilter(Text, max_tokens = 1000) |>
  step_tfidf(Text)

# Create grouped cross-validation folds
# Grouping by bulletin_id ensures that segments from the same bulletin
# are not split between training and validation folds.
set.seed(123)

hab_folds <- group_vfold_cv(
  hab_train,
  group = bulletin_id,
  v = 5
)

# Inspect the recipe and cross-validation folds
hab_recipe
hab_folds


# Naive Bayes baseline
# Train a simple probabilistic classifier to establish a baseline
# for comparison with the more complex models.

# Define Naive Bayes model specification
nb_spec <- naive_Bayes() |>
  set_mode("classification") |>
  set_engine("naivebayes")

# Create workflow by combining the recipe and model
nb_wf <- workflow() |>
  add_recipe(hab_recipe) |>
  add_model(nb_spec)

# Fit model using grouped cross-validation
set.seed(123)

nb_rs <- fit_resamples(
  nb_wf,
  hab_folds,
  metrics = metric_set(accuracy, roc_auc),
  control = control_resamples(save_pred = TRUE)
)

# Collect cross-validation performance metrics
collect_metrics(nb_rs)


# Lasso logistic regression
# Tune the regularization penalty to manage the large number of
# TF-IDF text features and reduce the risk of overfitting.

# Define Lasso logistic regression model
# mixture = 1 specifies Lasso (L1) regularization.
lasso_spec <- logistic_reg(
  penalty = tune(),
  mixture = 1
) |>
  set_mode("classification") |>
  set_engine("glmnet")

# Create grid of 30 candidate penalty values
lambda_grid <- grid_regular(
  penalty(),
  levels = 30
)

# Create workflow combining the recipe and model
lasso_wf <- workflow() |>
  add_recipe(hab_recipe) |>
  add_model(lasso_spec)

# Tune the regularization penalty using grouped cross-validation
set.seed(2023)

lasso_tune <- tune_grid(
  lasso_wf,
  resamples = hab_folds,
  grid = lambda_grid,
  metrics = metric_set(accuracy, roc_auc),
  control = control_resamples(save_pred = TRUE)
)

# Collect cross-validation performance metrics
lasso_metrics <- collect_metrics(lasso_tune)
lasso_metrics

# Select the simplest model within one standard error of
# the best ROC AUC result, favoring stronger regularization.
chosen_lasso <- lasso_tune |>
  select_by_one_std_err(
    metric = "roc_auc",
    -penalty
  )

chosen_lasso

# Finalize the workflow using the selected penalty
final_lasso <- finalize_workflow(
  lasso_wf,
  chosen_lasso
)

# Visualize model performance across penalty values
lasso_plot <- autoplot(lasso_tune) +
  labs(
    title = "Lasso Performance Across Regularization Penalties"
  ) +
  theme_minimal(base_size = 13)

lasso_plot

# Fit the finalized model on the complete training dataset
# This allows inspection of the fitted model's coefficients.
fitted_lasso <- fit(
  final_lasso,
  hab_train
)

# Extract and rank model coefficients by absolute magnitude
lasso_words <- fitted_lasso |>
  extract_fit_parsnip() |>
  tidy() |>
  filter(term != "(Intercept)") |>
  arrange(desc(abs(estimate)))

# Display the 20 terms with the largest coefficient magnitudes
lasso_words |>
  slice_head(n = 20)

write_csv(
  lasso_metrics,
  "outputs/tables/lasso_cross_validation_metrics.csv"
)

write_csv(
  lasso_words,
  "outputs/tables/lasso_coefficients.csv"
)

ggsave(
  "outputs/figures/lasso_performance.png",
  plot = lasso_plot,
  width = 8,
  height = 6,
  dpi = 300
)

## Random Forest

# Random Forest is tuned over mtry and min_n. These control how many text
# features are considered per split and how small a node can be before splitting.
rf_spec <- rand_forest(
  mtry = tune(),
  trees = 300,
  min_n = tune()
) |>
  set_mode("classification") |>
  set_engine("ranger", importance = "impurity")

rf_wf <- workflow() |>
  add_recipe(hab_recipe) |>
  add_model(rf_spec)

rf_grid <- grid_regular(
  mtry(range = c(10L, 100L)),
  min_n(range = c(2L, 20L)),
  levels = 3
)

set.seed(2026)
rf_tune <- tune_grid(
  rf_wf,
  resamples = hab_folds,
  grid = rf_grid,
  metrics = metric_set(accuracy, roc_auc),
  control = control_resamples(save_pred = TRUE)
)

rf_tune |> show_best(metric = "roc_auc")

best_rf <- select_best(rf_tune, metric = "roc_auc")
final_rf_wf <- finalize_workflow(rf_wf, best_rf)

best_rf

# Save Naive Bayes workflow
saveRDS(
  nb_wf,
  "data/processed/naive_bayes_workflow.rds"
)

# Save Lasso workflow
saveRDS(
  final_lasso,
  "data/processed/lasso_workflow.rds"
)

# Save Random Forest workflow
saveRDS(
  final_rf_wf,
  "data/processed/random_forest_workflow.rds"
)