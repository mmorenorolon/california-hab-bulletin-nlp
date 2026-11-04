# Load packages
library(tidyverse)
library(tidymodels)

tidymodels_prefer()

# Read the saved grouped train/test split
hab_split <- readRDS(
  "data/processed/hab_grouped_train_test_split.rds"
)

# Load finalized model workflows
nb_wf <- readRDS("data/processed/naive_bayes_workflow.rds")
final_lasso <- readRDS("data/processed/lasso_workflow.rds")
final_rf_wf <- readRDS("data/processed/random_forest_workflow.rds")

# Held-Out Bulletin Results

# Each final model is evaluated once on the held-out bulletin set. These are
# the results reported in the final project, not the earlier tuning results.
nb_final_fit <- last_fit(
  nb_wf,
  hab_split,
  metrics = metric_set(accuracy, roc_auc)
)

lasso_final_fit <- last_fit(
  final_lasso,
  hab_split,
  metrics = metric_set(accuracy, roc_auc)
)

rf_final_fit <- last_fit(
  final_rf_wf,
  hab_split,
  metrics = metric_set(accuracy, roc_auc)
)

all_models <- bind_rows(
  nb_final_fit |> collect_metrics() |> mutate(model = "Naive Bayes"),
  lasso_final_fit |> collect_metrics() |> mutate(model = "Lasso"),
  rf_final_fit |> collect_metrics() |> mutate(model = "Random Forest")
) |>
  select(model, .metric, .estimate) |>
  arrange(.metric, desc(.estimate))

all_models

# Model Comparison Plot
all_models_plot <- all_models |>
  ggplot(aes(x = model, y = .estimate, fill = model)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.3f", .estimate)), vjust = -0.35, size = 4) +
  facet_wrap(~ .metric) +
  coord_cartesian(ylim = c(0, 1.08)) +
  scale_fill_manual(values = c(
    "Lasso" = "#758bfd",
    "Naive Bayes" = "#450920",
    "Random Forest" = "#2d6a4f"
  )) +
  labs(
    title = "Model Performance on Held-Out HAB Bulletins",
    x = NULL,
    y = "Estimate"
  ) +
  theme_minimal(base_size = 13)

ggsave(
  "outputs/figures/model_comparison_held_out.png",
  plot = all_models_plot,
  width = 9,
  height = 6,
  dpi = 300
)
# Random Forest Confusion Matrix

# The confusion matrix shows the type and number of final-test errors made by
# the best-performing model.
rf_predictions <- rf_final_fit |> collect_predictions()

rf_conf_mat <- rf_predictions |>
  conf_mat(truth = public_health_action, estimate = .pred_class)

rf_conf_mat

rf_class_metrics <- rf_predictions |>
  metric_set(accuracy, sens, spec, ppv, npv)(
    truth = public_health_action,
    estimate = .pred_class,
    event_level = "first"
  )

rf_class_metrics

# Random Forest Confusion Plot
rf_conf_plot <- rf_conf_mat |>
  autoplot(type = "heatmap") +
  labs(title = "Confusion Matrix: Tuned Random Forest on Held-Out Bulletins") +
  scale_fill_gradient(low = "#d8f3dc", high = "#2d6a4f") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 12, hjust = 0.5))

ggsave(
  "outputs/figures/random_forest_confusion_matrix.png",
  plot = rf_conf_plot,
  width = 8,
  height = 6,
  dpi = 300
)