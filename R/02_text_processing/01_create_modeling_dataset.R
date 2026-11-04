# Load packages
library(tidyverse)

#' Create NLP Modeling Dataset
#'
#' Converts scraped HAB bulletins into text-level observations
#' Creates proxy public-health-action labels
#' Removes label-defining terms from predictor text 
#' Produces a modeling-ready dataset for supervised classification.

 
# Load helper functions used throughout the analysis
source("R/01_data_access/01_helper_functions.R")

# Read bulletin corpus
# Load the scraped bulletin text and associated metadata
bulletin_corpus <- read_csv(
  "data/processed/hab_bulletin_document_corpus.csv"
)

# Define keyword patterns

# Create keyword patterns to identify proxy public-health-action language and HAB-related text.
# These patterns are intentionally broad so the corpus
# retains a variety of HAB topics.

# Define terms associated with public-health advisories and actions.
public_health_pattern <- paste(
  c(
    "advisory", "closure", "closed", "quarantine", "do not consume",
    "do not eat", "do not harvest", "warning", "public health",
    "health advisory", "shellfish advisory", "beach advisory",
    "beach closure", "cdfw", "cdph", "department of public health"
  ),
  collapse = "|"
)

# Define terms associated with harmful algal blooms and related topics.
hab_terms_pattern <- paste(
  c(
    "harmful algal bloom", "hab", "pseudo-nitzschia", "domoic acid",
    "alexandrium", "saxitoxin", "dinophysis", "akashiwo",
    "lingulodinium", "phytoplankton", "toxin", "bloom", "shellfish",
    "marine mammal", "stranding"
  ),
  collapse = "|"
)

# Create HAB text segments 

# Each extracted text segment becomes one modeling observation.
# The split pattern generally separates sentences, so these are described
# as text segments rather than perfectly extracted sentences.

# Retain usable bulletins, split their text into segments, and calculate
# the word count and lowercase version of each segment.
hab_segments <- bulletin_corpus |>
  
  # Keep bulletins with more than 100 words
  filter(!is.na(text), word_count >= 100) |>
  
  # Select bulletins and associated metadata
  select(title, url, source, year_guess, month_guess, text) |>
  
  # Create text segments
  mutate(segment = str_split(text, "(?<=[.!?])\\s+(?=[A-Z])")) |>
  
  # Undo segments as lists
  unnest(segment) |>
  
  mutate(
    
    # Clean text segments
    segment = clean_text(segment),
    
    # Assign an id to each segment by row number
    segment_id = row_number(),
    
    # Count words in each segment
    segment_word_count = str_count(segment, "\\S+"),
    
    # Change text to lowecase
    segment_lower = str_to_lower(segment)
  ) |>
  
  # Keep only segments with more than 20 words
  filter(segment_word_count >= 20) |>
  mutate(
    
    # Assign type of action to each segment 
    # based on action pattern
    public_health_action = if_else(
      str_detect(segment_lower, public_health_pattern),
      "public_health_action",
      "no_public_health_action"
    ),
    
    # Determine relevance of segments to HAB-related terms
    hab_relevant = str_detect(segment_lower, hab_terms_pattern)
  ) |>
  
  # Keep only segments with HAB-related language
  filter(hab_relevant)

# Create modeling dataset

# Remove label-defining terms from the predictor text after creating the labels. 
# Retain the bulletin URL as an identifier for grouped resampling.
hab_model_df <- hab_segments |>
  mutate(
    # Remove public-health keywords from the text used as model input.
    Text = segment_lower |>
      str_replace_all(public_health_pattern, " ") |>
      clean_text(),
    
    # Set the positive class as the first factor level for model evaluation.
    public_health_action = factor(
      public_health_action,
      levels = c("public_health_action", "no_public_health_action")
    ),
    
    # Use the original bulletin URL to identify the source bulletin.
    bulletin_id = url
  ) |>
  
  # Retain the variables needed for modeling and later interpretation.
  select(
    segment_id, bulletin_id, title, url, year_guess, month_guess,
    Text, public_health_action, segment_word_count
  ) |>
  
  # Remove observations with missing or empty predictor text.
  filter(!is.na(Text), Text != "")

# Summarize modeling dataset

model_data_summary <- hab_model_df |>
  summarise(
    
    # Calculate number of observations
    text_segments = n(),
    
    # Calculate number of bulletins
    bulletins = n_distinct(bulletin_id),
    
    # Calculate typical segment length
    median_words_per_segment = median(segment_word_count),
    
    # Calculate year range
    earliest_year = min(year_guess, na.rm = TRUE),
    latest_year = max(year_guess, na.rm = TRUE)
  )

# Display the modeling dataset summary.
model_data_summary

# Display the number of segments in each public-health-action class.
hab_model_df |>
  count(public_health_action, name = "segments")

# Visualize class distribution
hab_model_df |>
  count(public_health_action) |>
  ggplot(aes(x = public_health_action, y = n, fill = public_health_action)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = n), vjust = -0.35, size = 4) +
  scale_fill_manual(values = c("#fb6107", "#7cb518")) +
  labs(
    title = "Class Distribution of HAB Public Health Action Labels",
    x = NULL,
    y = "Number of text segments"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 12, hjust = 0.5))

ggsave(
  "outputs/figures/hab_class_distribution.png",
  width = 8,
  height = 5,
  dpi = 300
)
# Save processed datasets

# Save the HAB-relevant text segments
write_csv(
  hab_segments, 
  "data/processed/hab_bulletin_text_segment_corpus.csv"
  )

# Save the modeling dataset containing cleaned predictor 
# text, proxy labels, and bulletin identifiers.
write_csv(
  hab_model_df,
  "data/processed/hab_nlp_modeling_data_grouped.csv"
  )