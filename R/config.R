################################################################################
# Shared Configuration — Economic Snapshot
#
# All data pull scripts source this file at startup.
################################################################################

# Project root, found with the here package (anchored by the .here file at the
# repo root). Everything lives inside the repo, so the same relative layout
# works on any clone — laptop, RStudio, or a GitHub Actions runner — as long
# as the working directory is anywhere inside the project.
BASE_DIR   <- here::here()
DATA_DIR   <- file.path(BASE_DIR, "data")
CREDS_DIR  <- file.path(BASE_DIR, "creds")
CODE_DIR   <- file.path(BASE_DIR, "R")
LOG_DIR    <- file.path(DATA_DIR, "logs")
START_DATE <- as.Date("2022-01-01")

# Ensure log directory exists on first run
if (!dir.exists(LOG_DIR)) dir.create(LOG_DIR, recursive = TRUE)

# Shared logging function used by all data pull scripts
# log_file: filename only (e.g. "fred_download_log.txt") — written to LOG_DIR
log_message <- function(message, log_file) {
  log_path <- file.path(LOG_DIR, log_file)
  if (!file.exists(log_path)) file.create(log_path)
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(sprintf("[%s] %s\n", timestamp, message), file = log_path, append = TRUE)
}
