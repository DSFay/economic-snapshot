################################################################################
# run_all.R — Master Orchestration Script
#
# Runs all automated data pull scripts in sequence. Safe to call from CRON or
# interactively. Each script is wrapped in tryCatch so one failure does not
# block the rest.
#
# WRDS (Revelio job postings) is excluded — it requires manual 2FA.
# Run R/run_wrds.R separately when needed.
#
# Usage (from anywhere inside the repo):
#   Rscript R/run_all.R
################################################################################

source(here::here("R", "config.R"))

RUN_LOG <- "run_all_log.txt"

scripts <- list(
  list(name = "FRED API",       file = "fred_api_pull.R"),
  list(name = "BLS API",        file = "bls_api.R"),
  list(name = "TSA Encounters", file = "scrape_tsa_encounters.R"),
  list(name = "CDC Measles",    file = "scrape_measles_cdc.R"),
  list(name = "ITA I-94",       file = "scrape_intnl_trade_admin_i_94.R")
)

# Validate credential files exist before running anything
required_creds <- c("fred_creds.env", "bls_creds.env")
missing_creds  <- required_creds[!file.exists(file.path(CREDS_DIR, required_creds))]

if (length(missing_creds) > 0) {
  msg <- paste0("❌ Missing credential files in ", CREDS_DIR, ": ",
                paste(missing_creds, collapse = ", "),
                "\n   Copy creds_template.env to ", CREDS_DIR,
                " and fill in your API keys.")
  message(msg)
  log_message(msg, RUN_LOG)
  stop("Credential files missing — aborting run.")
}

log_message(paste0("=== run_all.R started (", length(scripts), " scripts) ==="), RUN_LOG)

successes <- 0
failures  <- 0

for (s in scripts) {
  script_path <- file.path(CODE_DIR, s$file)
  log_message(paste0("--- Running: ", s$name, " (", s$file, ") ---"), RUN_LOG)
  message(paste0("[", format(Sys.time(), "%H:%M:%S"), "] Starting: ", s$name))

  tryCatch({
    source(script_path, local = new.env())
    successes <- successes + 1
    log_message(paste0("✅ Completed: ", s$name), RUN_LOG)
    message(paste0("[", format(Sys.time(), "%H:%M:%S"), "] ✅ Done: ", s$name))
  }, error = function(e) {
    failures <<- failures + 1
    msg <- paste0("❌ Failed: ", s$name, " — ", e$message)
    log_message(msg, RUN_LOG)
    message(msg)
  })
}

summary_msg <- paste0("=== run_all.R finished: ", successes, " succeeded, ",
                       failures, " failed ===")
log_message(summary_msg, RUN_LOG)
message(summary_msg)

# ── Email alert on failure ────────────────────────────────────────────────────
gmail_creds_path <- file.path(CREDS_DIR, "gmail_creds.env")

if (failures > 0 && file.exists(gmail_creds_path)) {
  tryCatch({
    dotenv::load_dot_env(file = gmail_creds_path)

    body_text <- paste0(
      "## Economic Snapshot data pull failed\n\n",
      failures, " of ", length(scripts), " scripts failed on ",
      format(Sys.time(), "%Y-%m-%d at %H:%M"), ".\n\n",
      "Check the log at:\n`", file.path(LOG_DIR, RUN_LOG), "`"
    )

    email <- blastula::compose_email(body = blastula::md(body_text))

    blastula::smtp_send(
      email,
      to          = Sys.getenv("GMAIL_USER"),
      from        = Sys.getenv("GMAIL_USER"),
      subject     = paste0("⚠️ Economic Snapshot: ", failures,
                           " pull(s) failed – ", format(Sys.Date(), "%Y-%m-%d")),
      credentials = blastula::creds_envvar(
        user        = "GMAIL_USER",
        pass_envvar = "GMAIL_APP_PASSWORD",
        host        = "smtp.gmail.com",
        port        = 465,
        use_ssl     = TRUE
      )
    )

    log_message("📧 Failure alert email sent.", RUN_LOG)
    message("📧 Failure alert email sent.")

  }, error = function(e) {
    log_message(paste0("⚠️ Could not send failure email: ", e$message), RUN_LOG)
    message(paste0("⚠️ Could not send failure email: ", e$message))
  })
}
