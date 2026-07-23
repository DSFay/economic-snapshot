################################################################################
# Scrape TSA Data from DHS, Log, & Save
################################################################################

# This script scrapes the latest data from the TSA's Encounters Data
# the data are updated daily
# The script scrapes the daily encounters volume, saves them, and logs all activity

library(rvest)
library(dplyr)
library(httr)
library(readr)
library(lubridate)

# Work from the repo root (found via the .here file) so the relative
# data/ paths below resolve on any clone.
setwd(here::here())

# Shared paths and the log_message() helper come from config.R
source(here::here("R", "config.R"))
LOG_FILE <- "tsa_download_log.txt"

# Scrape one TSA year page. The main page (no year suffix) shows the current year;
# year-specific pages (e.g. /2025) have the full calendar year.
scrape_tsa_year <- function(year) {
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  url <- if (year == current_year) {
    "https://www.tsa.gov/travel/passenger-volumes"
  } else {
    paste0("https://www.tsa.gov/travel/passenger-volumes/", year)
  }

  resp <- GET(
    url,
    user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64)
                AppleWebKit/537.36 (KHTML, like Gecko)
                Chrome/118.0.5993.70 Safari/537.36")
  )
  stop_for_status(resp)

  page <- read_html(content(resp, "text", encoding = "UTF-8"))
  tbl  <- page %>% html_element("table") %>% html_table(fill = TRUE)
  if (is.null(tbl)) stop(paste("No table found on TSA page for year", year))

  tbl %>%
    mutate(
      date      = as.Date(Date, format = "%m/%d/%Y"),
      encounters = parse_number(Numbers)
    ) %>%
    select(date, encounters) %>%
    filter(!is.na(date), !is.na(encounters)) %>%
    distinct()
}


tryCatch({
  current_year <- as.integer(format(Sys.Date(), "%Y"))
  prev_year    <- current_year - 1

  log_message(paste("Scraping TSA pages for", prev_year, "and", current_year), LOG_FILE)

  # Scrape current and previous year pages
  new_data <- bind_rows(
    scrape_tsa_year(prev_year),
    scrape_tsa_year(current_year)
  )
  log_message(paste("Scraped", nrow(new_data), "rows from TSA year pages"), LOG_FILE)

  # Load existing stable CSV (all historical data accumulated over time)
  stable_file <- "./data/tsa/tsa_daily_passenger_volumes.csv"
  if (file.exists(stable_file)) {
    existing <- read_csv(stable_file, show_col_types = FALSE) %>%
      select(date, encounters) %>%
      filter(!is.na(date), !is.na(encounters))
  } else {
    existing <- tibble(date = as.Date(character()), encounters = numeric())
  }

  # Merge all sources: new_data is last so it takes priority on overlapping dates
  # (historic raw file is already incorporated into tsa_daily_passenger_volumes.csv)
  tsa_all <- bind_rows(existing, new_data) %>%
    arrange(date) %>%
    group_by(date) %>%
    slice_tail(n = 1) %>%   # keep freshest value per date
    ungroup() %>%
    filter(!is.na(encounters))

  # Add derived columns used by server.R
  tsa_fin <- tsa_all %>%
    mutate(
      year           = year(date),
      month          = month(date),
      week           = week(date),
      day            = day(date),
      encounters_10k = round(encounters / 10000, digits = 2)
    ) %>%
    distinct()

  # Save back to the stable filename (load_all_latest always picks most-recent mtime)
  outdir <- "./data/tsa"
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

  write.csv(tsa_fin, file.path(outdir, "tsa_daily_passenger_volumes.csv"), row.names = FALSE)

  log_message(paste0("✅ Done: TSA Encounters — ", nrow(tsa_fin),
                     " rows saved (", min(tsa_fin$date), " to ", max(tsa_fin$date), ")"),
              LOG_FILE)

}, error = function(e) {
  log_message(paste("❌ Error scraping TSA data:", e$message), LOG_FILE)
})
