################################################################################
# make_sample_data.R — Generate the synthetic sample dataset
#
# Writes small, entirely synthetic CSVs into sample_data/, mirroring the
# folder structure and column layout of the real data/ folder. The front ends
# fall back to sample_data/ when data/ is empty (see global.R), so a fresh
# clone can render the dashboard without API keys or licensed data.
#
# The numbers are random walks with plausible levels — they are NOT real
# economic data. Regenerate with:  Rscript R/make_sample_data.R
################################################################################

library(dplyr)
library(readr)
library(lubridate)

setwd(here::here())

set.seed(42)
SAMPLE_DIR <- "sample_data"
START      <- as.Date("2022-01-01")
END        <- as.Date("2026-06-30")

# Random walk around a level: n points starting at `level`, step sd `noise`,
# optional per-step `drift`, clamped to stay positive
walk <- function(n, level, noise, drift = 0) {
  pmax(level + cumsum(rnorm(n, mean = drift, sd = noise)), level * 0.05)
}

write_sample <- function(subdir, filename, df) {
  dir <- file.path(SAMPLE_DIR, subdir)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  write_csv(df, file.path(dir, filename))
}

# --- FRED series (date, series_id, value, realtime_start, realtime_end, year)
fred_sample <- function(name, series_id, freq, level, noise, drift = 0) {
  dates <- switch(freq,
    d = seq(START, END, by = "day"),
    w = seq(START, END, by = "week"),
    m = seq(START, END, by = "month"),
    q = seq(START, END, by = "quarter"))
  df <- tibble(
    date           = dates,
    series_id      = series_id,
    value          = round(walk(length(dates), level, noise, drift), 2),
    realtime_start = END,
    realtime_end   = END,
    year           = year(dates)
  )
  write_sample(file.path("fred", paste0(name, "_", series_id)),
               paste0(name, "_sample.csv"), df)
}

fred_sample("unemployment_rate",             "UNRATE",         "m", 3.8,   0.1)
fred_sample("adp_non_farm_payroll",          "ADPWNUSNERSA",   "w", 1.25e8, 8e4, 4e4)
fred_sample("jolts_job_openings",            "JTSJOL",         "m", 10000, 250, -40)
fred_sample("all_vehicle_sales",             "TOTALSA",        "m", 15.5,  0.5)
fred_sample("cpi_urban_all_season_adj",      "CPIAUCSL",       "m", 285,   0.4, 0.9)
fred_sample("consumer_sentiment",            "UMCSENT",        "m", 70,    3)
fred_sample("consumer_loans_all_comm_banks", "CLSACBW027SBOG", "w", 900,   3, 0.6)
fred_sample("durable_goods",                 "DGORDER",        "m", 265000, 5000)
fred_sample("durables_no_defence",           "ADXDNO",         "m", 245000, 5000)
fred_sample("bond_10_yield",                 "DGS10",          "d", 3.0,   0.04)
fred_sample("spy",                           "SP500",          "d", 4500,  30, 1.2)
fred_sample("housing_starts",                "HOUST",          "m", 1450,  60)
fred_sample("initial_unemployment_claims_avg", "IC4WSA",       "w", 220000, 8000)
fred_sample("median_hourly_wage_growth",     "FRBATLWGT3MMAWMHWGO", "m", 4.5, 0.15)
fred_sample("usd",                           "DTWEXBGS",       "w", 118,   1)
fred_sample("fed_now_casts",                 "GDPNOW",         "q", 2.2,   0.6)
fred_sample("us_intnl_dom_passenger_air_load", "LOADFACTOR",   "m", 83,    1.5)
fred_sample("gas_price_regular",             "GASREGW",        "w", 3.6,   0.06)
fred_sample("rent_primary_residence",        "CUSR0000SEHA",   "m", 400,   1.2, 2.5)
fred_sample("mortgage_30yr_fixed",           "MORTGAGE30US",   "w", 6.5,   0.08)

# Weekly continued claims per state (levels roughly proportional to size)
claims_levels <- c(WI = 30000, MI = 60000, PA = 90000, GA = 35000,
                   NC = 25000, NV = 20000, AZ = 25000, NY = 150000,
                   FL = 30000, CA = 350000, TX = 100000, DC = 15000)
for (st in names(claims_levels)) {
  fred_sample(paste0("weekly_unemployment_claims_", tolower(st)),
              paste0(st, "CCLAIMS"), "w",
              claims_levels[[st]], claims_levels[[st]] * 0.04)
}

# --- BLS civilian labor force by state (monthly rows per state-year) --------
states <- tibble(
  state_abbr = names(claims_levels),
  state      = c("Wisconsin", "Michigan", "Pennsylvania", "Georgia",
                 "North Carolina", "Nevada", "Arizona", "New York",
                 "Florida", "California", "Texas", "District of Columbia"),
  labor_base = c(3.1e6, 4.9e6, 6.5e6, 5.3e6, 5.1e6, 1.6e6, 3.7e6,
                 9.6e6, 11.1e6, 19.3e6, 15.1e6, 4.1e5)
)
bls_df <- expand.grid(year = 2022:2026, month = month.name,
                      state_abbr = states$state_abbr,
                      stringsAsFactors = FALSE) |>
  left_join(states, by = "state_abbr") |>
  mutate(labor_force   = round(labor_base * (1 + rnorm(n(), 0, 0.005))),
         labor_force_m = labor_force / 1e6) |>
  select(year, month, state, state_abbr, labor_force, labor_force_m)
write_sample("bls_civilian_labor_force_by_state",
             "state_labor_force_sample.csv", bls_df)

# --- CDC measles weekly cases (near zero with two labeled outbreak spikes) --
week_starts <- seq(as.Date("2022-01-02"), END, by = "week")
cases <- rpois(length(week_starts), lambda = 2)
spike <- function(center, height, width = 8) {
  d <- as.numeric(week_starts - center) / 7
  round(height * exp(-(d^2) / (2 * (width / 2.5)^2)))
}
cases <- cases + spike(as.Date("2025-03-30"), 110) + spike(as.Date("2026-01-11"), 290)
measles_df <- tibble(
  week_start   = week_starts,
  week_end     = week_starts + 6,
  cases        = as.integer(cases),
  month        = month(week_starts, label = TRUE, abbr = TRUE),
  year         = year(week_starts),
  week         = pmin(week(week_starts), 52L),
  new_cases_1k = cases / 1000
)
write_sample("cdc_measles_weekly_onset", "measles_sample.csv", measles_df)

# --- ITA I-94 monthly visitor arrivals by country ---------------------------
countries <- tibble(
  country = c("Canada", "Mexico", "United Kingdom", "Japan", "Germany",
              "Brazil", "France", "South Korea", "India", "Colombia",
              "Australia", "Italy", "Spain", "Argentina", "Netherlands"),
  world_region = c("North America", "North America", "Europe", "Asia",
                   "Europe", "South America", "Europe", "Asia", "Asia",
                   "South America", "Oceania", "Europe", "Europe",
                   "South America", "Europe"),
  base = c(1.5e6, 1.2e6, 3.5e5, 2.5e5, 1.8e5, 3.0e5, 1.6e5, 1.5e5,
           1.4e5, 3.2e5, 1.0e5, 1.2e5, 9.0e4, 8.0e4, 7.0e4)
)
months <- seq(START, END, by = "month")
ita_df <- tidyr::crossing(countries, date = months) |>
  mutate(
    # gentle summer seasonality on top of noise
    visitors = as.integer(base * (1 + 0.25 * sin(2 * pi * (month(date) - 1) / 12)
                                  + rnorm(n(), 0, 0.08))),
    year  = year(date),
    month = month(date)
  ) |>
  select(country, world_region, date, visitors, year, month)
write_sample("international_trade_administration", "monthly_arrivals_sample.csv",
             ita_df)

# --- TSA daily checkpoint encounters ----------------------------------------
days <- seq(START, END, by = "day")
tsa_df <- tibble(date = days) |>
  mutate(
    encounters = as.integer(
      2.3e6 * (1 + 0.15 * sin(2 * pi * (yday(date) - 60) / 365)   # seasonal
                 + 0.08 * (wday(date) %in% c(1, 6, 7))            # weekend bump
                 + rnorm(n(), 0, 0.04))),
    year = year(date), month = month(date), week = week(date), day = day(date),
    encounters_10k = round(encounters / 10000, 2)
  )
write_sample("tsa", "tsa_daily_sample.csv", tsa_df)

# --- Revelio weekly job postings (synthetic stand-in for licensed data) -----
rev_weeks <- seq(as.Date("2021-01-04"), END, by = "week")
rev_df <- tibble(
  week_start      = rev_weeks,
  count_new_posts = as.integer(walk(length(rev_weeks), 1.6e6, 4e4, -800))
)
write_sample("revelio", "revelio_cosmos_weekly_jobs_sample.csv", rev_df)

message("✅ Sample data written to ", SAMPLE_DIR, "/")
