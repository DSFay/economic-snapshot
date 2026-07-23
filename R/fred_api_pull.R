library(fredr)
library(dplyr)
library(readr)
library(here)

# Work from the repo root (found via the .here file) so the relative
# data/ and creds/ paths below resolve on any clone.
setwd(here::here())

# Shared paths and the log_message() helper come from config.R
source(here::here("R", "config.R"))
LOG_FILE <- "fred_download_log.txt"

dotenv::load_dot_env(file = "creds/fred_creds.env")

# get key from .sh file 
api_key <- Sys.getenv("FRED_API_KEY")

# make sure key accessed successfully
fredr_has_key()

fredr_set_key(api_key)


# use fredr package to fetch data using the FRED API (tried fredo first but didn't work)
# https://fredblog.stlouisfed.org/2024/12/leveraging-r-for-powerful-data-analysis/
# enter specific serieis ids, dates
# returns a df

obsv_start <- as.Date("2022-01-01")
obsv_end <- Sys.Date()

fred_series_list <- list(
  
  # Consumer spending
  all_vehicle_sales                    = c("TOTALSA", "m"),
  cpi_urban_all_season_adj             = c("CPIAUCSL", "m"),
  consumer_sentiment                   = c("UMCSENT", "m"),
  consumer_loans_all_comm_banks        = c("CLSACBW027SBOG", "w"),

  # Household costs
  gas_price_regular                    = c("GASREGW", "w"),
  rent_primary_residence               = c("CUSR0000SEHA", "m"),
  mortgage_30yr_fixed                  = c("MORTGAGE30US", "w"),
  
  # National unemployment
  initial_unemployment_claims_avg      = c("IC4WSA", "w"),
  continued_unemployment_claims_avg    = c("CC4WSA", "w"),
  unemployment_rate                    = c("UNRATE", "m"),
  adp_non_farm_payroll                 = c("ADPWNUSNERSA", "w"),
  jolts_job_openings                   = c("JTSJOL", "m"),

  # Swing state unemployment (weekly continued claims)
  weekly_unemployment_claims_wi   = c("WICCLAIMS", "w"),
  weekly_unemployment_claims_mi   = c("MICCLAIMS", "w"),
  weekly_unemployment_claims_pa   = c("PACCLAIMS", "w"),
  weekly_unemployment_claims_ga   = c("GACCLAIMS", "w"),
  weekly_unemployment_claims_nc   = c("NCCCLAIMS", "w"),
  weekly_unemployment_claims_nv   = c("NVCCLAIMS", "w"),
  weekly_unemployment_claims_az   = c("AZCCLAIMS", "w"),

  # Swing state monthly SA unemployment rates
  monthly_unrate_wi                = c("WIUR", "m"),
  monthly_unrate_mi                = c("MIUR", "m"),
  monthly_unrate_pa                = c("PAUR", "m"),
  monthly_unrate_ga                = c("GAUR", "m"),
  monthly_unrate_nc                = c("NCUR", "m"),
  monthly_unrate_nv                = c("NVUR", "m"),
  monthly_unrate_az                = c("AZUR", "m"),

  # Large state + DC unemployment (weekly continued claims)
  weekly_unemployment_claims_ny   = c("NYCCLAIMS", "w"),
  weekly_unemployment_claims_fl   = c("FLCCLAIMS", "w"),
  weekly_unemployment_claims_ca   = c("CACCLAIMS", "w"),
  weekly_unemployment_claims_tx   = c("TXCCLAIMS", "w"),
  weekly_unemployment_claims_dc   = c("DCCCLAIMS", "w"),

  # Large state monthly SA unemployment rates
  monthly_unrate_ny                = c("NYUR", "m"),
  monthly_unrate_fl                = c("FLUR", "m"),
  monthly_unrate_ca                = c("CAUR", "m"),
  monthly_unrate_tx                = c("TXUR", "m"),

  # Wage growth: Unweighted Median Hourly Wage Growth: Overall 
  median_hourly_wage_growth  = c("FRBATLWGT3MMAWMHWGO", "m"),
  
  # Business spending 
  durable_goods                   = c("DGORDER", "m"),
  durables_no_defence             = c("ADXDNO", "m"),
                      
  # Travel 
  us_intnl_dom_passenger_air_load = c("LOADFACTOR", "m"),
  scheduled_passeng_air_transport_ppi = c("PCU481111481111", "m"),
  
  # Markets 
  bond_10_yield                   = c("DGS10", "d"),
  spy                             = c("SP500", "d"), 
  usd                             = c("DTWEXBGS", "w"),
  
  # Real Gross Fixed Capital Formation for United States
  fixed_cap_formation = c("NFIRSAXDCUSQ", "q"),
  # New Privately-Owned Housing Units Started: Total Units, Seasonally Adjusted 
  housing_starts = c("HOUST", "m"),
  # GDPNow, Atlanta FED
  fed_now_casts = c("GDPNOW", "q")
  )


# Empty list to track failures
failures <- list()

# Loop and try each download
for (name in names(fred_series_list)) {
  series_id <- fred_series_list[[name]][1]
  fq <- fred_series_list[[name]][2]
  folder_path <- file.path("data", "fred", paste0(name, "_",series_id))
  file_path <- file.path(folder_path, 
                         paste0(name, "_",obsv_start, "-", obsv_end, ".csv"))
  
  # Try fetching data
  tryCatch({
    df <- fredr(series_id = series_id, 
                frequency = fq, 
                observation_start = obsv_start, 
                observation_end = obsv_end) %>%
      mutate(
            date = as.Date(date),
            year = lubridate::year(date))
    
    if (!dir.exists(folder_path)) dir.create(folder_path, recursive = TRUE) 
    write_csv(df, file_path)
    
    msg <- paste0("✅ Successfully downloaded ", series_id)
    message(msg)
    log_message(msg, LOG_FILE)  # log success
    
  }, error = function(e) {
    msg <- paste0("❌ Failed: ", series_id, " - ", e$message)
    message(msg)
    log_message(msg, LOG_FILE)  # log error
    
    failures[[length(failures) + 1]] <<- data.frame(
      timestamp = Sys.time(),
      series_id = series_id,
      name = name,
      error_msg = e$message,
      stringsAsFactors = FALSE
    )
  })
}

# -------------------
# Save failure summary if any
# -------------------
if (length(failures) > 0) {
  log_df <- do.call(rbind, failures)
  fail_log_path <- file.path("data", "logs", "fred_api_pull_failures.csv")
  write_csv(log_df, fail_log_path)
  log_message(paste0("⚠️ Logged ", nrow(log_df), " failures to ", fail_log_path), LOG_FILE)
} else {
  log_message("✅ All series downloaded successfully.", LOG_FILE)
}