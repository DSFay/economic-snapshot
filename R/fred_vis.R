library(ggplot2)
library(dplyr)
library(readr)
library(purrr)


load_and_assign <- function(series_names) {
  # Helper function to load the most recent file for a series
  load_latest_file <- function(series) {
    folder_path <- file.path("data", "fred")
    matches <- list.dirs(folder_path, full.names = TRUE, recursive = FALSE)
    target_folder <- matches[grepl(paste0("^", series, "_"), basename(matches))]
    
    if (length(target_folder) == 0) {
      warning(paste("⚠️ No folder found for series:", series))
      return(NULL)
    }
    
    files <- list.files(target_folder, full.names = TRUE, pattern = "\\.csv$")
    if (length(files) == 0) {
      warning(paste("⚠️ No CSV file found in", target_folder))
      return(NULL)
    }
    
    # Choose the latest CSV by filename (which includes date)
    latest_file <- files[order(files, decreasing = TRUE)][1]
    
    df <- readr::read_csv(latest_file, show_col_types = FALSE) %>%
      dplyr::mutate(
        date = as.Date(date),
        value = as.numeric(value)
      )
    
    # Assign to global env using the nickname
    assign(series, df, envir = .GlobalEnv)
    message(paste("✅ Loaded:", series))
    return(invisible(NULL))  # we don't need to collect the result
  }
  
  # Run the loader for each series
  purrr::walk(series_names, load_latest_file)
}

load_and_assign(c(
  # national labor
  "initial_unemployment_claims_avg",
  "continued_unemployment_claims_avg",
  "adp_non_farm_payroll",
  "unemployment_rate",
  # weekly state claims
  # swing states
  "weekly_unemployment_claims_wi",
  "weekly_unemployment_claims_mi",
  "weekly_unemployment_claims_pa",
  "weekly_unemployment_claims_ga",
  "weekly_unemployment_claims_nc",
  "weekly_unemployment_claims_nv",
  "weekly_unemployment_claims_az",
  # large states   
  "weekly_unemployment_claims_ny",
  "weekly_unemployment_claims_fl",
  "weekly_unemployment_claims_ca",
  "weekly_unemployment_claims_tx",
  "weekly_unemployment_claims_dc",
  # Consumer spending 
  "all_vehicle_sales", 
  "cpi_urban_all_season_adj",
  "median_hourly_wage_growth",
  "consumer_sentiment",
  # Business spending group
  "consumer_loans_all_comm_banks",
  "durable_goods",
  # Travel group
  "us_intnl_dom_passenger_air_load",
  "scheduled_passeng_air_transport_ppi",
  # Market group
  "spy",
  "bond_10_yield",
  "fed_now_casts",
  "housing_starts"))



# Unemployment group
national_unemployment_series <- c(
  "initial_unemployment_claims_avg",
  "continued_unemployment_claims_avg",
  "unemployment_rate",
  "adp_non_farm_payroll")

swing_state_unemployment_series <- c(
  "weekly_unemployment_claims_wi",
  "weekly_unemployment_claims_mi",
  "weekly_unemployment_claims_pa",
  "weekly_unemployment_claims_ga",
  "weekly_unemployment_claims_nc",
  "weekly_unemployment_claims_nv",
  "weekly_unemployment_claims_az"
)
large_state_unemployment_series <- c(
  "weekly_unemployment_claims_ny",
  "weekly_unemployment_claims_fl",
  "weekly_unemployment_claims_ca",
  "weekly_unemployment_claims_tx",
  "weekly_unemployment_claims_dc"
)

# Consumer spending group
consumer_series <- c(
  "all_vehicle_sales",       
  "cpi_urban_all_season_adj",     
  "consumer_sentiment",           
  "consumer_loans_all_comm_banks"
)

# Business spending group
business_series <- c(  
  "durable_goods",
  "durables_no_defence", 
  "weighted_ma_median_wage_growth"
)


# Travel group
travel_series<- c(
  "us_intnl_dom_passenger_air_load", 
  "scheduled_passeng_air_transport_ppi"
)

# Market group 
market_series <- c(
  "bond_10_yield",
  "spy", 
  "usd"
)



# -------------------------------
# Helper function: Load a series only if not already loaded into memory
# -------------------------------
load_if_needed <- function(series) {
  
  # If the object already exists in memory, just return it
  if (exists(series, envir = .GlobalEnv)) {
    message(paste("✔️ In memory:", series))  # Inform the user
    return(get(series, envir = .GlobalEnv))  # Return the in-memory object
  }
  
  # Define the main folder where FRED series are stored
  folder_path <- file.path("data", "fred")
  
  # List all subfolders in the FRED folder
  matches <- list.dirs(folder_path, full.names = TRUE, recursive = FALSE)
  
  # Find the folder that matches the series name (e.g. "unemployment_rate_UNRATE")
  target_folder <- matches[grepl(paste0("^", series, "_"), basename(matches))]
  
  # If no folder was found for this series, return a warning and NULL
  if (length(target_folder) == 0) {
    warning(paste("⚠️ No folder found for series:", series))
    return(NULL)
  }
  
  # Get all CSV files in that folder
  files <- list.files(target_folder, full.names = TRUE, pattern = "\\.csv$")
  
  # If no files found, return a warning and NULL
  if (length(files) == 0) {
    warning(paste("⚠️ No CSV files in", target_folder))
    return(NULL)
  }
  
  # Sort file names in descending order to pick the most recent one (based on filename date)
  latest_file <- files[order(files, decreasing = TRUE)][1]
  
  # Read the CSV file into a data frame
  df <- readr::read_csv(latest_file, show_col_types = FALSE) %>%
    dplyr::mutate(
      date = as.Date(date),       # Convert `date` column to actual Date objects
      value = as.numeric(value)   # Make sure `value` is numeric (some files may import it as text)
    )
  
  # Save the data frame into memory using the series name
  assign(series, df, envir = .GlobalEnv)
  
  message(paste("✅ Loaded to memory:", series))  # Let the user know it's loaded
  return(df)  # Return the loaded data
}


# -------------------------------
# Plot multiple FRED series in one grouped chart
# -------------------------------
plot_group_series <- function(series_names, group_title) {
  
  # Loop through each series name and try to load its data
  dfs <- purrr::map(series_names, function(name) {
    df <- load_if_needed(name)   # Try to load the data (from memory or disk)
    
    if (is.null(df)) return(NULL)  # If loading failed, skip this series
    
    df$series <- name  # Add a column to track the series name (used for faceting)
    return(df)  # Return the cleaned-up data frame
  }) %>% dplyr::bind_rows()  # Combine all series into one big data frame
  
  # If no data frames were successfully loaded, show a warning and skip plotting
  if (nrow(dfs) == 0) {
    warning("⚠️ No data found for any series in group:", group_title)
    return(NULL)
  }
  
  # Use ggplot2 to make a line plot for each series
  ggplot(dfs, aes(x = date, y = value)) +
    geom_line(color = "steelblue") +  # Draw lines in blue
    facet_wrap(~ series, scales = "free_y") +  # One panel per series, y-axes are independent
    scale_x_date(date_breaks = "2 months", date_labels = "%b\n%Y") +  # Custom date formatting
    labs(
      title = group_title,  # Chart title
      x = "Date",           # X-axis label
      y = "Value"           # Y-axis label
    ) +
    theme_minimal()  # Use a clean, simple theme
}

# Display national unemployment plots
plot_group_series(national_unemployment_series, "National Unemployment Indicators")

# Display swing state unemployment plots
plot_group_series(swing_state_unemployment_series, "Swing State Unemployment, Weekly")
plot_group_series(large_state_unemployment_series, "Large State Unemployment, Weekly")

# Display consumer spending plots
plot_group_series(consumer_series, "Consumer Spending Indicators")

# Display business spending plots
plot_group_series(business_series, "Business Spending Indicators")

# Display travel indicator plots
plot_group_series(travel_series, "Travel Spending Indicators")

# Display market indicator plots
plot_group_series(market_series, "Market Indicators")

