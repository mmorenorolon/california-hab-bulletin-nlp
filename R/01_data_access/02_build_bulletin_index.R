# Load packages
library(tidyverse)
library(rvest)
library(httr)
library(stringr)

source("R/01_data_access/01_helper_functions.R")

# I accessed the SCCOOS archive and extracted links that appear to point to individual HAB bulletin pages, archived email bulletins, or uploaded files.

# Store and download the archive URL to access the webpage, parse the HTML and prevent the script from crashing if the webpage is down.
archive_url <- "https://sccoos.org/archive-ca-hab-bulletins/"
archive_page <- safe_read_html(archive_url)

# Check that download was successful
if (is.null(archive_page)) {
  stop("Archive page could not be read. Check the SCCOOS URL and internet connection.")
}

# Extract hyperlinks with anchor elements
links <- archive_page |> html_elements("a")

# Create bulletin index table
bulletin_index <- tibble(
  
  # Extract visible link text
  title = html_text2(links),
  
  # Extract URLs
  url = html_attr(links, "href")
) |>
  mutate(
    
    # Clean title text
    title = clean_text(title),
    
    # Convert relative URL paths to full URLs
    url = url_absolute(url, archive_url)
  ) |>
  
  # Remove missing URLs and empty titles
  filter(!is.na(url), title != "") |>
  
  filter(str_detect(
    
    # Convert to lowercase
    # Look for bulletin-related words
    str_to_lower(title),
    "bulletin|report|january|february|march|april|may|june|july|august|september|october|november|december|spring|summer|fall|winter"
  )) |>
  
  # Keep only bulletin URLs (bulletin pages, email archives, uploaded files or PDFs)
  filter(str_detect(url, "sccoos.org/california-hab-bulletin|t.e2ma.net|wp-content/uploads")) |>
  
  # Remove duplicate URLs
  distinct(url, .keep_all = TRUE) |>
  mutate(
    
    # Create metadata column to record source type
    source = case_when(
      str_detect(url, "t.e2ma.net") ~ "email_archive",
      str_detect(url, "wp-content/uploads") ~ "pdf_or_upload",
      TRUE ~ "sccoos_html"
    ),
    
    # Extract year at each row
    year_guess = map_int(paste(title, url), extract_year_from_text),
    
    # Extract month at each row
    month_guess = map_int(title, extract_month_from_text)
  )

# Summary table to verify successful extraction
bulletin_index |>
  summarise(
    
    # Number of bulletin links
    bulletin_links_found = n(),
    
    # Extract earliest year
    earliest_year = min(year_guess, na.rm = TRUE),
    
    # Extract latest year
    latest_year = max(year_guess, na.rm = TRUE)
  )

# Save the bulletin index for use in subsequent scripts.
write_csv(
  bulletin_index, 
  "data/processed/hab_bulletin_index.csv"
  )