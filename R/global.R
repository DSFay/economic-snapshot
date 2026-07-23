################################################################################
# global.R — Shared data loading and preparation
#
# Sourced by BOTH front ends (shiny-app/server.R and economic_snapshot.qmd),
# so everything that touches raw data lives in exactly one place and the two
# front ends cannot drift apart. Chart construction stays in each front end.
#
# Provides:
#   - shared chart constants (palettes, date breaks, ELECTION_DATE)
#   - load_all_latest(): loads the newest CSV from every data/ subfolder
#   - one prepared data frame per chart (unrate_filtered, group1_data, ...)
################################################################################

library(dplyr)
library(readr)
library(purrr)
library(lubridate)

source(here::here("R", "config.R"))

# --- Shared chart constants ------------------------------------------------
date_break    <- "3 months"
date_labels   <- "%b\n%Y"   # month abbrev over year: "Mar" / "2022"
month_breaks  <- c(1, 14, 27, 40, 49)   # for charts with week-of-year x-axis
month_labels  <- c("Jan", "Apr", "Jul", "Oct", "Dec")

# Okabe-Ito colorblind-safe palette (+ 2 extras for 10-category charts)
okabe_ito     <- c("#E69F00","#56B4E9","#009E73","darkred","#0072B2",
                   "#D55E00","#CC79A7","#999999")
palette_10    <- c(okabe_ito, "#117733", "#44AA99")

# Date of the 2024 general election, drawn as a reference line on most charts
ELECTION_DATE <- as.Date("2024-11-01")

# --- Shared minimal chart style --------------------------------------------
# Style tokens for the "clean & minimal" look, shared by both front ends so
# the Quarto page and the Shiny app stay visually in sync. theme_snapshot()
# is defined here but only CALLED after each front end has attached ggplot2.
LINE_COLOR     <- "#1f2937"   # thin near-black single-series line
GRID_COLOR     <- "#f0f0f0"   # faint horizontal gridlines
AXIS_COLOR     <- "#b8b8b8"   # muted axis tick text
LABEL_COLOR    <- "#8c8c8c"   # axis titles
TITLE_COLOR    <- "#1c1c1c"   # chart titles
SUBTITLE_COLOR <- "#a0a0a0"   # chart subtitles / captions
ANNOT_COLOR    <- "#c8c8c8"   # subtle reference lines + their labels
ACCENT_COLOR   <- "#4f46e5"   # restrained accent, used sparingly
ELECTION_COLOR <- ANNOT_COLOR # election reference line

theme_snapshot <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.background    = ggplot2::element_rect(fill = "white", color = NA),
      panel.background   = ggplot2::element_rect(fill = "white", color = NA),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = GRID_COLOR, linewidth = 0.4),
      axis.text          = ggplot2::element_text(color = AXIS_COLOR, size = 9),
      axis.title         = ggplot2::element_text(color = LABEL_COLOR, size = 10),
      plot.title         = ggplot2::element_text(color = TITLE_COLOR, size = 14),
      plot.subtitle      = ggplot2::element_text(color = SUBTITLE_COLOR, size = 11),
      plot.caption       = ggplot2::element_text(color = SUBTITLE_COLOR, size = 8, hjust = 1),
      legend.text        = ggplot2::element_text(color = "#6d6d6d", size = 9),
      legend.title       = ggplot2::element_blank()
    )
}

# --- Choose the data source -------------------------------------------------
# Use the real data/ folder if the pull scripts have populated it; otherwise
# fall back to the bundled synthetic sample so a fresh clone can render the
# dashboard without API keys or licensed data. Regenerate the sample with
# R/make_sample_data.R.
USING_SAMPLE_DATA <- length(list.files(DATA_DIR, pattern = "[.]csv$",
                                       recursive = TRUE)) == 0
if (USING_SAMPLE_DATA) {
  DATA_DIR <- file.path(BASE_DIR, "sample_data")
  message("data/ is empty — loading the synthetic sample_data/ instead. ",
          "Run R/run_all.R with your own API keys to fetch real data.")
}

# --- Load the latest CSV from each data folder into a named object ---------
load_all_latest <- function(base_dir = DATA_DIR) {
  stopifnot(dir.exists(base_dir))
  all_dirs  <- list.dirs(base_dir, recursive = TRUE, full.names = TRUE)
  all_dirs  <- all_dirs[basename(all_dirs) != "logs"]
  leaf_dirs <- all_dirs[!all_dirs %in% dirname(all_dirs)]
  purrr::walk(leaf_dirs, function(d) {
    files <- list.files(d, pattern = "\\.csv$", full.names = TRUE)
    if (length(files) == 0) return(NULL)
    latest_file <- files[which.max(file.info(files)$mtime)]
    assign(basename(d), readr::read_csv(latest_file, show_col_types = FALSE),
           envir = .GlobalEnv)
  })
  invisible(NULL)
}
load_all_latest()

# --- Labor: national unemployment rate -------------------------------------
unrate_filtered <- unemployment_rate_UNRATE |>
  rename(unrate_percent = value) |>
  filter(!is.na(unrate_percent)) |>
  mutate(date = as.Date(date)) |>
  distinct() |>
  filter(year > 2021)
unrate_mean <- mean(unrate_filtered$unrate_percent, na.rm = TRUE)

# --- Labor: ADP private payroll --------------------------------------------
adp <- adp_non_farm_payroll_ADPWNUSNERSA |>
  rename(persons = value) |>
  mutate(date = as.Date(date)) |>
  distinct() |>
  filter(year > 2021)

# --- Labor: JOLTS job openings (guarded: chart shows a placeholder if the
#     series has not been downloaded yet) -----------------------------------
if (exists("jolts_job_openings_JTSJOL")) {
  jolts_dat <- jolts_job_openings_JTSJOL |>
    mutate(date = as.Date(date)) |>
    filter(year > 2021, !is.na(value))
}

# --- Labor: initial jobless claims (4-week average) + wage growth ----------
initial_claims_dat <- initial_unemployment_claims_avg_IC4WSA |>
  mutate(date = as.Date(date)) |>
  filter(year > 2021, !is.na(value))

wage_growth_dat <- median_hourly_wage_growth_FRBATLWGT3MMAWMHWGO |>
  mutate(date = as.Date(date)) |>
  filter(year > 2021, !is.na(value))

# --- Labor: state continued-claims rates -----------------------------------
# The BLS file has one row per state-month; the claims series are weekly.
# Average the labor force to one value per state-year before joining, so the
# join does not fan each weekly claims row out across all twelve months.
labor_force <- bls_civilian_labor_force_by_state |>
  group_by(state_abbr, year) |>
  summarise(labor_force = mean(labor_force, na.rm = TRUE), .groups = "drop")

add_labor_force <- function(unemp_df, state_abbr, labor_force) {
  unemp_df |>
    mutate(state_abbr = toupper(state_abbr),
           year = as.integer(year),
           value = as.numeric(value)) |>
    left_join(labor_force, by = c("state_abbr", "year"))
}

unemp_list <- list(
  WI = weekly_unemployment_claims_wi_WICCLAIMS,
  MI = weekly_unemployment_claims_mi_MICCLAIMS,
  PA = weekly_unemployment_claims_pa_PACCLAIMS,
  GA = weekly_unemployment_claims_ga_GACCLAIMS,
  NC = weekly_unemployment_claims_nc_NCCCLAIMS,
  NV = weekly_unemployment_claims_nv_NVCCLAIMS,
  AZ = weekly_unemployment_claims_az_AZCCLAIMS,
  NY = weekly_unemployment_claims_ny_NYCCLAIMS,
  FL = weekly_unemployment_claims_fl_FLCCLAIMS,
  CA = weekly_unemployment_claims_ca_CACCLAIMS,
  TX = weekly_unemployment_claims_tx_TXCCLAIMS,
  DC = weekly_unemployment_claims_dc_DCCCLAIMS
)

unemp_with_lf <- imap(unemp_list, ~add_labor_force(.x, .y, labor_force))
group1_names  <- c("WI", "MI", "PA", "GA", "NC", "NV", "AZ")  # swing states
group2_names  <- setdiff(names(unemp_with_lf), group1_names)  # large states + DC

# Roll the weekly claims rate up to a monthly average per state — the raw
# weekly series is very noisy, and a monthly mean reads far more clearly.
to_monthly_rate <- function(df) {
  df |>
    mutate(unemployment_rate = (value / labor_force) * 100,
           date = floor_date(as.Date(date), "month")) |>
    group_by(state, date) |>
    summarise(unemployment_rate = mean(unemployment_rate, na.rm = TRUE),
              .groups = "drop")
}
group1_data <- to_monthly_rate(bind_rows(unemp_with_lf[group1_names], .id = "state"))
group2_data <- to_monthly_rate(bind_rows(unemp_with_lf[group2_names], .id = "state"))

# --- Consumers --------------------------------------------------------------
car_sales <- all_vehicle_sales_TOTALSA |>
  mutate(date = as.Date(date)) |>
  filter(year > 2021)

cpi_dat <- cpi_urban_all_season_adj_CPIAUCSL |>
  mutate(date = as.Date(date)) |>
  filter(year > 2021, !is.na(value))

sentiment_dat <- consumer_sentiment_UMCSENT |>
  mutate(date = as.Date(date)) |>
  filter(year > 2021)

consume_loans <- consumer_loans_all_comm_banks_CLSACBW027SBOG |>
  mutate(date = as.Date(date)) |>
  filter(year > 2021)

durable_goods_dat <- durable_goods_DGORDER |>
  mutate(date = as.Date(date)) |>
  filter(year > 2021)

durables_no_def_dat <- durables_no_defence_ADXDNO |>
  mutate(date = as.Date(date)) |>
  filter(year > 2021)

# --- Consumers: household costs (gas, rent, mortgage) ----------------------
gas_dat <- gas_price_regular_GASREGW |>
  mutate(date = as.Date(date)) |>
  filter(year > 2021, !is.na(value))

rent_dat <- rent_primary_residence_CUSR0000SEHA |>
  mutate(date = as.Date(date)) |>
  filter(year > 2021, !is.na(value))

mortgage_dat <- mortgage_30yr_fixed_MORTGAGE30US |>
  mutate(date = as.Date(date)) |>
  filter(year > 2021, !is.na(value))

# --- Travel: top-10 tourist origin countries (ITA I-94) ---------------------
intnl_visitor_yr <- international_trade_administration |>
  filter(!is.na(world_region), !is.na(visitors), year > 2021) |>
  distinct() |>
  group_by(country, date) |>
  summarize(total_visitors_100k = sum(as.integer(visitors)) / 100000,
            .groups = "drop")

top_10_tourist_origins <- intnl_visitor_yr |>
  group_by(date) |>
  slice_max(order_by = total_visitors_100k, n = 10, with_ties = FALSE) |>
  ungroup()

# --- Travel: TSA checkpoint encounters, aggregated to weekly ----------------
tsa_weekly <- tsa |>
  mutate(date = as.Date(date)) |>
  filter(date > as.Date("2021-12-31")) |>
  mutate(week_start = floor_date(date, "week", week_start = 1)) |>
  group_by(week_start) |>
  summarise(encounters_10k = sum(encounters_10k, na.rm = TRUE),
            n_days = n(), .groups = "drop") |>
  # keep only complete weeks; partial first/last weeks plot as false cliffs
  filter(n_days == 7)

# --- Travel: airline passenger load factor ---------------------------------
loadfactor_dat <- us_intnl_dom_passenger_air_load_LOADFACTOR |>
  mutate(date = as.Date(date)) |>
  filter(year > 2021, !is.na(value))

# --- Public health: CDC measles cases ---------------------------------------
measles_dat <- cdc_measles_weekly_onset |>
  mutate(week_start = as.Date(week_start))

# --- Markets ----------------------------------------------------------------
# The Treasury yield and S&P 500 arrive daily; average them to a weekly value
# to smooth the daily noise while keeping more detail than a monthly series.
# Housing starts is already monthly.
to_weekly_mean <- function(df) {
  df |>
    mutate(date = floor_date(as.Date(date), "week", week_start = 1)) |>
    group_by(date) |>
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
}

bond_dat <- bond_10_yield_DGS10 |>
  mutate(date = as.Date(date)) |>
  filter(year > 2021) |>
  to_weekly_mean()

spy_dat <- spy_SP500 |>
  mutate(date = as.Date(date)) |>
  filter(year > 2021) |>
  to_weekly_mean()

house_dat <- housing_starts_HOUST |>
  mutate(date = as.Date(date)) |>
  filter(year > 2021)

# Trade-weighted US dollar index (weekly) and the Atlanta Fed GDP nowcast
# (quarterly estimate of annualized real GDP growth).
usd_dat <- usd_DTWEXBGS |>
  mutate(date = as.Date(date)) |>
  filter(year > 2021, !is.na(value))

gdpnow_dat <- fed_now_casts_GDPNOW |>
  mutate(date = as.Date(date)) |>
  filter(year > 2021, !is.na(value))

# --- Freshness stamp --------------------------------------------------------
# Latest observation date across the series, shown as a "data through" stamp
# in the site footer.
DATA_THROUGH <- suppressWarnings(max(c(
  max(unrate_filtered$date,   na.rm = TRUE),
  max(spy_dat$date,           na.rm = TRUE),
  max(bond_dat$date,          na.rm = TRUE),
  max(tsa_weekly$week_start,  na.rm = TRUE),
  max(cpi_dat$date,           na.rm = TRUE),
  max(house_dat$date,         na.rm = TRUE)
), na.rm = TRUE))
