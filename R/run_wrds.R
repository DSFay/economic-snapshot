################################################################################
# run_wrds.R — Manual WRDS / Revelio Data Pull
#
# WRDS requires 2FA (phone call from Duo) on first connection each session.
# This script CANNOT be run via CRON unattended. Run it manually:
#
#   1. Open RStudio or a terminal
#   2. From the repo root, run: Rscript R/run_wrds.R
#   3. Answer the Duo phone call and press any key when prompted
#   4. If the first run fails with an auth error, run the script again —
#      the session token is usually cached after the first 2FA challenge
#
# Output: data/revelio/revelio_cosmos_weekly_jobs.csv
################################################################################

library(RPostgres)
library(lubridate)
library(dplyr)
library(DBI)
library(dotenv)
library(readr)

source(here::here("R", "config.R"))

LOG_FILE    <- "wrds_download_log.txt"
outdir      <- file.path(DATA_DIR, "revelio")
stable_file <- file.path(outdir, "revelio_cosmos_weekly_jobs.csv")

dotenv::load_dot_env(file = file.path(CREDS_DIR, "wrds_creds.env"))

if (Sys.getenv("WRDS_USER") == "" || Sys.getenv("WRDS_PASSWORD") == "") {
  msg <- paste0("❌ WRDS credentials not found. Check ", CREDS_DIR, "/wrds_creds.env.")
  message(msg)
  log_message(msg, LOG_FILE)
  stop("WRDS credentials missing.")
}

message("Connecting to WRDS — you will receive a Duo 2FA phone call. Answer and press any key.")

tryCatch({
  wrds <- dbConnect(
    RPostgres::Postgres(),
    host     = "wrds-pgdata.wharton.upenn.edu",
    port     = 9737,
    sslmode  = "require",
    dbname   = "wrds",
    user     = Sys.getenv("WRDS_USER"),
    password = Sys.getenv("WRDS_PASSWORD")
  )
  log_message("✅ Connected to WRDS.", LOG_FILE)
}, error = function(e) {
  msg <- paste0("❌ Failed to connect to WRDS: ", e$message,
                "\n   If this is a 2FA error, wait 30 seconds and run the script again.")
  message(msg)
  log_message(msg, LOG_FILE)
  stop(e)
})

tryCatch({
  # Pull all weekly COSMOS job postings from 2021 onward
  # On incremental runs, we still pull the full history because WRDS aggregates
  # by week — cheaper than caching partial results
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

  data_cosmos <- dbFetch(jobs_cosmos)
  dbClearResult(jobs_cosmos)
  dbDisconnect(wrds)

  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  write_csv(data_cosmos, stable_file)

  log_message(paste0("✅ WRDS Revelio COSMOS: ", nrow(data_cosmos),
                     " weekly rows saved to ", stable_file), LOG_FILE)
  message(paste0("✅ Done — ", nrow(data_cosmos), " rows saved."))

}, error = function(e) {
  log_message(paste0("❌ Error pulling COSMOS data: ", e$message), LOG_FILE)
  message(paste0("❌ Error: ", e$message))
})
