# These helper functions clean text, safely read webpages, and infer dates from the bulletin titles and URLs.

#' Clean Text
#'
#' Standardizes whitespace in character strings by replacing nonbreaking spaces, removing repeated whitespace, and trimming leading and trailing spaces.
#'
#' @param x A character vector containing text to clean.
#'
#' @return A character vector with standardized whitespace.
#'
clean_text <- function(x) {
  x |>
    str_replace_all("\\u00a0", " ") |>
    str_replace_all("\\s+", " ") |>
    str_squish()
}

#' Safely Read an HTML webpage 
#'
#' Attempts to read an HTML webpage. If the webpage cannot be accessed
#' or parsed, the function returns NULL instead of stopping execution.
#'
#' @param url A character string containing the webpage URL.
#'
#' @return An HTML document if successful, or NULL if an error occurs.
#'
safe_read_html <- function(url) {
  tryCatch(read_html(url), error = function(e) NULL)
}

#' Extract Year from Text
#'
#' Identifies the first four-digit year beginning with "20" in a
#' character string.
#'
#' @param x A character string containing a year.
#'
#' @return An integer representing the extracted year, or NA if no
#' matching year is found.
#'
extract_year_from_text <- function(x) {
  year <- str_extract(x, "20\\d{2}") |> as.integer()
  ifelse(length(year) == 0 || is.na(year), NA_integer_, year)
}

#' Extract Month from Text
#'
#' Identifies a month name or abbreviated month name within a
#' character string.
#'
#' @param x A character string containing a month name or abbreviation.
#'
#' @return An integer from 1 to 12 representing the month, or NA if
#' no matching month is found.
#'
extract_month_from_text <- function(x) {
  month_lookup <- tibble(
    month_name = c(month.name, month.abb),
    month_num = rep(1:12, 2)
  )
  
  found <- month_lookup |>
    filter(str_detect(str_to_lower(x), str_to_lower(month_name))) |>
    slice(1) |>
    pull(month_num)
  
  ifelse(length(found) == 0, NA_integer_, found)
}