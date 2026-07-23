################################################################################
# Scrape Measles Data from CDC, Log, & Save
################################################################################

# This script scrapes the latest data from the CDC's Measles Cases & Outbreaks Tracker
  # the data are updated weekly on Thursdays
  # The script scrapes the weekly new Measles cases, saves them, and logs all activity


# Install packages as needed
# install.packages("httr")
# install.packages("jsonlite")


# Load required packages
library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)   # month()/year()/week() below — must be loaded explicitly
                     # so this script also works standalone (cron runs it solo)

# Work from the repo root (found via the .here file) so the relative
# data/ paths below resolve on any clone.
setwd(here::here())

# Shared paths and the log_message() helper come from config.R
source(here::here("R", "config.R"))
LOG_FILE <- "cdc_measles_download_log.txt"

# Step 1: Download JSON from the CDC
  # NOTE - this URL is manually sources from the Network elements cited 
  # when the download csv button is clicked for monthly outbreak data
  # at https://www.cdc.gov/measles/data-research/index.html 
  # if this breaks, scrape data from table displayed on webpage
url <- "https://www.cdc.gov/wcms/vizdata/measles/MeaslesCasesWeekly.json"
response <- GET(url)


# Check if request was successful (status code 200)
if (status_code(response) == 200) {
  tryCatch({
    # Parse response JSON
    json_data <- content(response, as = "text", encoding = "UTF-8")
    json_list <- fromJSON(json_data)
    measles_df <- bind_rows(json_list) %>%
      mutate(
        week_start = as.Date(week_start),
        week_end = as.Date(week_end),
        cases = as.integer(cases), 
        month = month(week_start, label = TRUE, abbr = TRUE),
        year = year(week_start),
        week = week(week_start),
        week = if_else(week > 52, 52L, week),  # ← collapse week 53 into 52
        new_cases_1k = cases/1000) %>%
      arrange(year, week) %>%
      distinct()

  
    # Save to CSV
    outdir <- "./data/cdc_measles_weekly_onset"
    if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
    
    outfile <- file.path(outdir, 
                         paste0("measles_data_",
                                min(measles_df$week_start), "-", max(measles_df$week_end), ".csv"))
    
    write.csv(measles_df, outfile, row.names = FALSE)
    
    # Log success
    log_message("✅ Successfully downloaded and saved measles data.", LOG_FILE)
  }, error = function(e) {
    # Log any parsing or saving errors
    log_message(paste("❌ Error during JSON parsing or saving CSV:", e$message), LOG_FILE)
  })
} else {
  # Log HTTP failure
  log_message(paste("❌ HTTP request failed. Status code:", status_code(response)), LOG_FILE)
}
    

