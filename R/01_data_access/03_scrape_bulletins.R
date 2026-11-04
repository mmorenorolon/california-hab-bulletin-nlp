# Load packages
library(rvest)
library(tidyverse)
library(httr)
library(stringr)

source("R/01_data_access/01_helper_functions.R")

# This function collects visible article text and removes common navigation labels
# It pauses briefly between requests to be courteous to the server.

#' Scrape SCCOOS HAB bulletin text
#'
#' Downloads bulletin content from URLs identified in
#' 02_build_bulletin_index.R, extracts visible article text,
#' removes boilerplate navigation content, and builds a
#' document-level bulletin corpus with text, word counts,
#' and scrape status metadata.
#' @param x A character vector containing the URL.
#'
#' @return A dataframe with the bulletin corpus
#' 
scrape_html_bulletin <- function(url) {
  
  # Pace requests by half a second
  Sys.sleep(0.5)
  
  # Download and parse HTML
  page <- safe_read_html(url)
  
  # Handle failed pages by returning a row indicating failure
  if (is.null(page)) {
    return(tibble(text = NA_character_, scrape_status = "failed_read"))
  }
  
  # Extract article text
  text_nodes <- page |>
    
    # Find headings, paragraphs, or lists inside page content
    html_elements(
      "article h1, article h2, article h3, article p, article li,
       main h1, main h2, main h3, main p, main li,
       .entry-content h1, .entry-content h2, .entry-content h3,
       .entry-content p, .entry-content li"
    ) |>
    
    # Convert HTML to text
    html_text2() |>
    
    # Clean extracted text
    clean_text()
  
  # Drop empty strings
  text_nodes <- text_nodes[text_nodes != ""]
  
  # Some archive pages have a different HTML structure, so I use a wider fallback selector when the article-specific selector returns no text.
  
  # If article selectors fail...
  if (length(text_nodes) == 0) {
    text_nodes <- page |>
      
      # Use a broader search of text contents
      html_elements("h1, h2, h3, p, li") |>
      
      # Convert HTML to text
      html_text2() |>
      
      # Clean extracted text
      clean_text()
    
    # Remove blanks
    text_nodes <- text_nodes[text_nodes != ""]
  }
  
  # Create boilerplate pattern for text that is not bulletin content
  boilerplate_pattern <- paste(
    "^toggle menu$|^navigation$|^about$|^observations$|^products$|",
    "^projects$|^follow us$|^feedback$|^contact us$",
    sep = ""
  )
  
  # Remove boilerplate (navigation content)
  text_nodes <- text_nodes[
    
    # Keep entries that DO NOT match boilerplate
    !str_detect(str_to_lower(text_nodes), boilerplate_pattern)
  ]
  
  # Output table
  tibble(
    
    # Combine all text
    text = paste(text_nodes, collapse = " "),
    
    # Assign content status
    scrape_status = if_else(length(text_nodes) > 0, "ok", "empty")
  )
}

# Uploaded PDFs are excluded here because this workflow extracts HTML text.

# Read bulletin index
bulletin_index <- read_csv("data/processed/hab_bulletin_index.csv")

# Build bulletin corpus
bulletin_corpus <- bulletin_index |>
  
  # Exclude PDFs
  filter(source != "pdf_or_upload") |>
  
  # Scrape every URL
  mutate(scraped = map(url, scrape_html_bulletin)) |>
  
  # Expand list column
  unnest(scraped) |>
  
  mutate(
    
    # Clean extracted text
    text = clean_text(text),
    
    # Count non-whitespace sequences
    word_count = str_count(text, "\\S+"),
    
    # Count number of characters
    character_count = str_length(text)
  )

# View which bulletins where successfully read
bulletin_corpus |>
  count(scrape_status)

# Summary statistics for bulletin corpus
corpus_summary <- bulletin_corpus |>
  summarise(
    
    # Total number of bulletins
    bulletins_total = n(),
    
    # Count bulletins with meaningful content
    usable_bulletins = sum(!is.na(text) & word_count >= 100),
    
    # Median bulletin size
    median_words = median(word_count, na.rm = TRUE),
    
    # Quantify the smallest bulletin
    min_words = min(word_count, na.rm = TRUE),
    
    # Quantify the largest bulletin
    max_words = max(word_count, na.rm = TRUE)
  )

# View results
corpus_summary

# Save bulletin corpus data
write_csv(
  bulletin_corpus,
  "data/processed/hab_bulletin_document_corpus.csv"
  )