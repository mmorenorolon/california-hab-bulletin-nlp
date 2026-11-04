library(tidyverse)
library(tidytext)

hab_model_df <- read_csv(
  "data/processed/hab_nlp_modeling_data_grouped.csv"
) |>
  mutate(
    public_health_action = factor(
      public_health_action,
      levels = c("public_health_action", "no_public_health_action")
    )
  )

# This visualization describes terms that are distinctive to each proxy class.
# It is descriptive rather than causal: terms may reflect bulletin topic,
# writing style, and the public-health-action context at the same time.
top_tfidf_terms <- hab_model_df |>
  unnest_tokens(word, Text) |>
  anti_join(stop_words, by = "word") |>
  filter(str_detect(word, "[a-z]")) |>
  count(public_health_action, word, sort = TRUE) |>
  bind_tf_idf(word, public_health_action, n) |>
  group_by(public_health_action) |>
  slice_max(tf_idf, n = 15, with_ties = FALSE) |>
  ungroup()

top_tfidf_terms_plot <- top_tfidf_terms |>
  mutate(word = reorder_within(word, tf_idf, public_health_action)) |>
  ggplot(aes(x = tf_idf, y = word, fill = public_health_action)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ public_health_action, scales = "free_y") +
  scale_y_reordered() +
  scale_fill_manual(values = c("#fb6107", "#7cb518")) +
  labs(
    title = "Top TF-IDF Terms by Public Health Action Class",
    x = "TF-IDF",
    y = NULL
  ) +
  theme_minimal(base_size = 13)

ggsave(
  "outputs/figures/top_tfidf_terms.png",
  plot = top_tfidf_terms_plot,
  width = 8,
  height = 6,
  dpi = 300
)