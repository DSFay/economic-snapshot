################################################################################
# Scrape Tourist Entry Data from ITA, Log, & Save
################################################################################

# This script scrapes the latest data from the International Travel Administration's 
  # monthly tracker for country of origin of tourists entering the US 

# The script scrapes monthly country of origin counts, saves them, and logs all activity


# Install packages as needed
# install.packages("httr")
# install.packages("jsonlite")


# Load required packages
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(rvest)
library(readxl)
library(stringr)


# Work from the repo root (found via the .here file) so the relative
# data/ paths below resolve on any clone.
setwd(here::here())

# monthly scrape I-94 data for toursits visiting US w/Countries of Origin
# https://www.trade.gov/i-94-arrivals-program


# Shared paths and the log_message() helper come from config.R
source(here::here("R", "config.R"))
LOG_FILE <- "ita_download_log.txt"

# Page with link to monthly Excel
page <- read_html("https://www.trade.gov/i-94-arrivals-program")

# Look for the Excel link by its link text (you might need to refine selector)
link_node <- page %>%
  html_nodes(xpath = "//a[contains(., 'I-94 Monthly International Visitor Arrivals') and contains(@href, '.xlsx')]") %>%
  html_attr("href")

# Convert to full URL if relative
excel_url <- ifelse(grepl("^https?://", link_node),
                    link_node,
                    url_absolute(link_node, "https://www.trade.gov"))

log_message(paste("Discovered dynamic URL:", excel_url), LOG_FILE)

# Step 1: Download Excel from the ITA
# url <- "https://www.trade.gov/sites/default/files/2024-06/Monthly%20Arrivals%202000%20to%20Present%20%E2%80%93%20Country%20of%20Residence%20%28COR%29_1.xlsx"
temp_file <- tempfile(fileext = ".xlsx")
response <- GET(excel_url, write_disk(temp_file, overwrite = TRUE))


if (status_code(response) == 200) {
  tryCatch({
    # Parse response JSON
    
    df <- read_excel(temp_file)
    
    col2 <- colnames(df[2])
    col3 <- colnames(df[3])
    
    ita_df <-  df %>%
      rename(country = all_of(col2), world_region = all_of(col3)) %>%
      select(-1, -!starts_with("202"), country, world_region) %>%
      # reshape wide month columns to long rows (tidyr replaces the retired
      # reshape2::melt; values coerced to character to survive mixed types)
      pivot_longer(cols = -c(country, world_region),
                   names_to = "date", values_to = "visitors",
                   values_transform = as.character) %>%
      filter(str_detect(visitors, "^\\d+$")) %>%
      mutate(
        clean_date = str_extract(date, "^\\d{4}-\\d{1,2}"),
        date = as.Date(paste0(clean_date, "-01"), format = "%Y-%m-%d"),   # ch DATE used in VIZ
        year = as.integer(str_sub(date, 1, 4)),
        month = as.integer(str_sub(date, 6, 7)),
        visitors = as.integer(visitors)
      ) %>%
      filter(!is.na(world_region), !is.na(visitors), year>2021, year<2030) %>%
      select(-clean_date) %>%
      distinct()
    
    # Save to CSV
    outdir <- "./data/international_trade_administration"
    if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
    
    outfile <- file.path(outdir, 
                         paste0("monthly_arrivals_country_i94_",
                                min(ita_df$date), "-", max(ita_df$date), ".csv"))
    write.csv(ita_df, outfile, row.names = FALSE)
    
    
    # Log success
    log_message("✅ Successfully downloaded and saved tourism data", LOG_FILE)}
    , error = function(e) {
    # Log any parsing or saving errors
    log_message(paste("❌ Error during JSON parsing or saving CSV:", e$message), LOG_FILE)
  })
} else {
  # Log HTTP failure
  log_message(paste("❌ HTTP request failed. Status code:", status_code(response)), LOG_FILE)
}

