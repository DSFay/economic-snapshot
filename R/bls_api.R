

# install.packages('blsAPI')
library(blsAPI)
library(devtools)
library(dplyr)
library(rjson)
library(httr)
# library(tidyverse)
library(glue)

# install_github("mikeasilva/blsAPI")

# BLS API how to https://github.com/mikeasilva/blsAPI

# Work from the repo root (found via the .here file) so the relative
# data/ and creds/ paths below resolve on any clone.
setwd(here::here())

# Shared paths and the log_message() helper come from config.R
source(here::here("R", "config.R"))
LOG_FILE <- "bls_download_log.txt"

dotenv::load_dot_env(file = "creds/bls_creds.env")

api_key <- Sys.getenv("BLS_API_KEY")      # should save your key


# test small example 
# labor_force <- laus_get_data(c("California", "Florida", "Texas", "Nevada"), "labor force", 2022, 2025)

# another possibly helpful function
# help_laus_areacodes()

tryCatch({

# split state list in half so they don't return "out of bounds error"
  # first half of state list 
states_a_m <-c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
               "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia",
               "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky",
               "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",
               "Mississippi", "Missouri", "Montana")
  
  # second half of state list 
states_n_w <- c("Nebraska", "Nevada", "New Hampshire",
                "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota",
                "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island",
                "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont",
                "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")

start_year <- 2018
end_year <- as.numeric(format(Sys.Date(), "%Y")) # returns current year - keeps code dynamic

# save date to use in file name
obsv_end <- Sys.Date()


labor_force_a_m <- laus_get_data(states_a_m, "labor force", start_year, end_year, api.version = 2, bls.key = api_key)
labor_force_n_w <- laus_get_data(states_n_w, "labor force", start_year, end_year, api.version = 2, bls.key = api_key)

# combine two halves of data
state_labor_force_df <- bind_rows(labor_force_a_m, labor_force_n_w) 


state_labor_force <- state_labor_force_df %>%
  rename(state = Location, 
         month = periodName,
         labor_force = Labor_Force) %>%
  mutate(
    year = as.character(year),
    labor_force_m = as.numeric(labor_force)/1000000,
    state_abbr = case_when(
      state == "District of Columbia" ~ "DC",
      # State == "Puerto Rico" ~ "PR",
      TRUE ~ state.abb[match(state, state.name)])) %>%
  select(year, month, state, state_abbr, labor_force, labor_force_m) %>%
  distinct()


# Build dynamic file path
file_path <- file.path("./data/bls_civilian_labor_force_by_state",
                       paste0("state_labor_force_population_", start_year, "-", obsv_end, ".csv"))

if (!dir.exists(dirname(file_path))) dir.create(dirname(file_path), recursive = TRUE)

# Save data
write.csv(state_labor_force, file_path, row.names = FALSE)

# Log success
log_message(paste0("✅ Successfully scraped ", nrow(state_labor_force),
                   " rows -> ", file_path), LOG_FILE)

}, error = function(e) {
  # Log any error
  log_message(paste0("❌ Error scraping BLS data: ", e$message), LOG_FILE)
})

