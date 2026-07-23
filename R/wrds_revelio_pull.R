# initial setup instructions for accessing WRDS data via R can be found here:
# https://wrds-www.wharton.upenn.edu/pages/support/programming-wrds/programming-r/r-from-the-web/

# steps include: 1) setting up a .pgpass /pgpass.conf file
#                2) add .Rprofile information to the beginning of each WRDS 
#                   session to connect to the PostgrsSQL database 

# install.packages("RPostgres")
# install.packages("dotenv")
# remotes::install_github("blopker/dotenv")

library(RPostgres)
library(lubridate)
library(dplyr)
library(tidyr)
library(DBI)
library(dotenv)

# Work from the repo root (found via the .here file) so the relative
# data/ paths below resolve on any clone.
setwd(here::here())

# Load shared paths and the log_message() helper, then read WRDS credentials
# from the gitignored creds/ folder (see config.R) so secrets are never
# committed.
source(here::here("R", "config.R"))
LOG_FILE <- "wrds_download_log.txt"
dotenv::load_dot_env(file = file.path(CREDS_DIR, "wrds_creds.env"))
# Sys.getenv("WRDS_USER")      # should print your username
# Sys.getenv("WRDS_PASSWORD")  # should print your password


tryCatch({
  wrds <- dbConnect(
    RPostgres::Postgres(),
    host = 'wrds-pgdata.wharton.upenn.edu',
    port = 9737,
    sslmode = 'require',
    dbname = 'wrds',
    user = Sys.getenv("WRDS_USER"),
    password = Sys.getenv("WRDS_PASSWORD")
  )
  log_message("✅ Successfully connected to WRDS database.", LOG_FILE)
}, error = function(e) {
  log_message(paste0("❌ Failed to connect to WRDS: ", e$message), LOG_FILE)
  stop(e)  # stop script if connection fails
})

# -------------------
# Pull COSMOS job listings
# -------------------
tryCatch({
  jobs_cosmos <- dbSendQuery(wrds,
                             "SELECT
       post_date - ((EXTRACT(DOW FROM post_date)::int + 6) % 7) * INTERVAL '1 day' AS week_start,
       COUNT(*) AS count_new_posts
     FROM revelio.postings_cosmos
     WHERE country = 'United States'
       AND post_date > '2021-01-01'
     GROUP BY week_start
     ORDER BY week_start"
  )
  
  data_cosmos_group <- dbFetch(jobs_cosmos)
  dbClearResult(jobs_cosmos)
  
  # Build dynamic file path
  file_path <- file.path("data/revelio",
                         paste0("revelio_cosmos_weekly_jobs_",
                                min(data_cosmos_group$week_start), "-",
                                max(data_cosmos_group$week_start), ".csv"))
  write.csv(data_cosmos_group, file_path, row.names = FALSE)
  
  log_message(paste0("✅ Successfully pulled COSMOS data (", nrow(data_cosmos_group),
                     " rows) -> ", file_path), LOG_FILE)
}, error = function(e) {
  log_message(paste0("❌ Error pulling COSMOS data: ", e$message), LOG_FILE)
})
