library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(lubridate)
library(ggrepel)

# All data loading and preparation is shared with the Quarto page and lives in
# R/global.R (which also sources R/config.R). This file only builds the charts
# and defines the Shiny server.
source(here::here("R", "global.R"))

# ---------------------------------------------------------------------------
# line_indicator(): builds a standard single-line time-series chart.
#
# Most charts on the dashboard share the same shape (one line over time, an
# election reference line, a minimal theme). This helper builds that shared
# chart once so each indicator becomes a single readable call instead of a
# copy-pasted block. Charts with a different shape (grouped/coloured lines,
# bars, labelled points) are still built individually below.
#
# Arguments:
#   df            data frame to plot
#   xvar, yvar    column names for the x and y axes (unquoted)
#   title, subtitle, y_lab, caption   chart text
#   line_color    colour of the line (default black)
#   show_election TRUE to draw the dashed election reference line
#   y_pretty      TRUE to use evenly spaced ("pretty") y-axis breaks
# ---------------------------------------------------------------------------
line_indicator <- function(df, yvar, title, y_lab, caption,
                           subtitle = NULL, xvar = date,
                           line_color = LINE_COLOR,
                           show_election = TRUE, y_pretty = FALSE) {
  p <- ggplot(df, aes(x = {{ xvar }}, y = {{ yvar }})) +
    geom_line(color = line_color, linewidth = 0.5) +
    scale_x_date(date_breaks = date_break, date_labels = date_labels,
                 expand = c(0.01, 0)) +
    labs(title = title, subtitle = subtitle, x = NULL, y = y_lab,
         caption = caption) +
    theme_snapshot()

  if (y_pretty)      p <- p + scale_y_continuous(breaks = pretty_breaks(n = 8))
  if (show_election) p <- p + geom_vline(xintercept = ELECTION_DATE,
                                         linetype = "dashed", color = ELECTION_COLOR)
  p
}


######### Labor indicators

plot_unrate <- line_indicator(
  unrate_filtered,
  yvar = unrate_percent,
  title = "National Unemployment, Seasonally Adjusted",
  subtitle = "Monthly, seasonally adjusted",
  y_lab = "Percentage",
  caption = "Source: U.S. Bureau of Labor Statistics via FRED® | Frequency: Monthly",
  y_pretty = TRUE) +
  geom_hline(yintercept = unrate_mean, linetype = "dotted",
             color = ANNOT_COLOR, linewidth = 0.6) +
  annotate("text",
           x = min(unrate_filtered$date, na.rm = TRUE),
           y = unrate_mean + 0.08,
           label = paste0("Period avg: ", round(unrate_mean, 1), "%"),
           hjust = 0, size = 3, color = "#9a9a9a")

plot_adp_payroll <- line_indicator(
  adp,
  yvar = persons / 1000000,
  title = "Total Nonfarm Private Payroll Employment, Seasonally Adjusted",
  subtitle = "Monthly, seasonally adjusted",
  y_lab = "Persons (millions)",
  caption = "Source: Automatic Data Processing, Inc. via FRED® | Frequency: Monthly",
  y_pretty = TRUE)

plot_swing_state_unrate <- group1_data %>%
  ggplot(aes(x = date, y = unemployment_rate, color = state)) +
  geom_line() +
  geom_vline(xintercept = ELECTION_DATE, linetype = "dashed", color = ELECTION_COLOR) +
  labs(
    title = "Swing State Continued Unemployment Claims",
    subtitle = "Monthly average of continued claims as % of labor force",
    y = "Continued Unemployment Claims Rate (%)",
    x = NULL,
    caption = "Source: U.S. Bureau of Labor Statistics via FRED® | Frequency: Monthly average",
    color = NULL) +
  scale_colour_manual(values = okabe_ito, name = NULL) +
  scale_x_date(date_breaks = date_break, date_labels = date_labels) +
  theme_minimal()

plot_large_state_unrate <- group2_data %>%
  ggplot(aes(x = date, y = unemployment_rate, color = state)) +
  geom_line() +
  geom_vline(xintercept = ELECTION_DATE, linetype = "dashed", color = ELECTION_COLOR) +
  labs(
    title = "Large State Continued Unemployment Claims",
    subtitle = "Monthly average of continued claims as % of labor force",
    y = "Continued Unemployment Claims Rate (%)",
    x = NULL,
    caption = "Source: U.S. Bureau of Labor Statistics via FRED® | Frequency: Monthly average",
    color = NULL) +
  scale_colour_manual(values = okabe_ito, name = NULL) +
  scale_x_date(date_breaks = date_break, date_labels = date_labels) +
  theme_minimal()

# Revelio COSMOS job listings — licensed data; used by this private app only,
# never published. Prep stays here (not in global.R) because the public
# Quarto page must not depend on it.
data_cosmos_graph <- revelio %>%
  mutate(year = year(week_start),
         week = week(week_start),
         week = if_else(week > 52, 52L, week),
         new_posts_1k = count_new_posts / 1000) %>%
  group_by(year, week) %>%
  summarise(new_posts_1k = sum(new_posts_1k), .groups = "drop") %>%
  arrange(year, week) %>%
  filter(year > 2021)

plot_cosmos_listings <- data_cosmos_graph %>%
  ggplot(aes(x = week, y = new_posts_1k, group = year, color = factor(year))) +
  geom_line() +
  scale_x_continuous(
    breaks = month_breaks,
    labels = month_labels,
    expand = c(0.1, 0)) +
  scale_y_continuous(breaks = pretty_breaks(n = 8)) +
  scale_colour_manual(values = okabe_ito, name = NULL) +
  labs(
    title = "New Job Listings (Revelio Labs)",
    subtitle = "Weekly new online job postings, by year",
    x = "Month",
    y = "Job Posts (1000s)",
    color = NULL,
    caption = "Source: Revelio Labs via Wharton Research Data Services | Frequency: Weekly") +
  theme_minimal()

# JOLTS job openings — jolts_dat is only defined (in global.R) once the FRED
# pull has downloaded the series; show a placeholder until then
if (exists("jolts_dat")) {
  plot_jolts <- line_indicator(
    jolts_dat,
    yvar = value / 1000,
    title = "Total Job Openings (JOLTS)",
    subtitle = "Monthly, not seasonally adjusted",
    y_lab = "Job Openings (millions)",
    caption = "Source: U.S. Bureau of Labor Statistics via FRED® | Frequency: Monthly",
    line_color = okabe_ito[2],
    y_pretty = TRUE)
} else {
  plot_jolts <- ggplot() +
    annotate("text", x = 0.5, y = 0.5,
             label = "JOLTS data not yet downloaded.\nRun R/fred_api_pull.R to enable this chart.",
             size = 5, color = "grey50", hjust = 0.5) +
    theme_void() +
    labs(title = "Total Job Openings (JOLTS)",
         caption = "Source: U.S. Bureau of Labor Statistics via FRED®")
}


########## Travel indicators

# Assign a distinct color to each of the 10 countries (ordered by total
# visitors overall) and label each line at its final point
country_order <- top_10_tourist_origins %>%
  group_by(country) %>%
  summarise(total = sum(total_visitors_100k), .groups = "drop") %>%
  arrange(desc(total)) %>%
  pull(country)

country_colors <- setNames(rep(palette_10, length.out = length(country_order)),
                           country_order)

labs_df <- top_10_tourist_origins %>%
  group_by(country) %>%
  slice_max(date, n = 1) %>%
  ungroup()

plot_tourist_origins <- top_10_tourist_origins %>%
  ggplot(aes(x = date, y = total_visitors_100k, group = country, colour = country)) +
  geom_line() +
  geom_vline(xintercept = ELECTION_DATE, linetype = "dashed", color = ELECTION_COLOR) +
  geom_text_repel(data = labs_df,
                  aes(label = country), hjust = 0, nudge_x = 0.5, size = 3,
                  show.legend = FALSE) +
  scale_colour_manual(values = country_colors, guide = "none") +
  scale_x_date(date_breaks = date_break, date_labels = date_labels) +
  coord_cartesian(clip = "off") +
  labs(
    x = NULL,
    y = "Visitors (100k)",
    title = "Top 10 Countries for Tourists Visiting US",
    subtitle = "Monthly arrivals via I-94 border crossings",
    caption = "Source: International Trade Administration (I-94) | Frequency: Monthly") +
  theme_minimal()

plot_tsa_line <- line_indicator(
  tsa_weekly,
  xvar = week_start,
  yvar = encounters_10k,
  title = "Weekly TSA Encounters",
  subtitle = "Daily checkpoint data aggregated to weekly totals",
  y_lab = "Encounters (10k)",
  caption = "Source: Transportation Security Administration | Frequency: Daily (aggregated weekly)")


########## Public health indicators

# Key outbreak periods to label (identified from CDC case data); joined to the
# actual case counts so labels sit above the bars
outbreak_labels <- data.frame(
  week_start = as.Date(c("2025-03-30", "2026-01-11")),
  label      = c("2025 TX/NM\nOutbreak", "2026 TX\nOutbreak"),
  vjust      = c(-0.4, -0.4)
) %>%
  left_join(measles_dat %>% select(week_start, cases), by = "week_start")

plot_measles <- measles_dat %>%
  ggplot(aes(x = week_start, y = cases)) +
  geom_col(width = 5) +
  geom_text(data = outbreak_labels,
            aes(x = week_start, y = cases, label = label, vjust = vjust),
            size = 2.8, color = "grey30", lineheight = 0.9) +
  labs(
    title = "Measles Cases by Rash Onset",
    subtitle = "Weekly confirmed cases, United States",
    y = "Cases",
    x = NULL,
    caption = "Source: U.S. Centers for Disease Control and Prevention | Frequency: Weekly") +
  scale_x_date(
    date_breaks = date_break,
    date_labels = date_labels,
    expand = c(0.01, 0)) +
  theme_minimal()


########## Market indicators

plot_bond_10_yield <- line_indicator(
  bond_dat,
  yvar = value,
  title = "10-Year US Bond Yield",
  subtitle = "Market yield on U.S. Treasury securities at 10-year constant maturity",
  y_lab = "Percent",
  caption = "Source: Board of Governors of the Federal Reserve System (US) via FRED® | Frequency: Daily")

plot_spy <- line_indicator(
  spy_dat,
  yvar = value,
  title = "S&P 500",
  subtitle = "Daily closing index value",
  y_lab = "Index",
  caption = "Source: S&P Dow Jones Indices LLC via FRED® | Frequency: Daily")

plot_housing_starts <- line_indicator(
  house_dat,
  yvar = value,
  title = "New Privately-Owned Housing: Total Units Started",
  subtitle = "Monthly, seasonally adjusted annual rate (thousands of units)",
  y_lab = "Thousands of Units",
  caption = "Source: U.S. Census Bureau; U.S. Dept. of Housing and Urban Development via FRED® | Frequency: Monthly")


########## Consumer indicators

plot_car_sales <- line_indicator(
  car_sales,
  yvar = value,
  title = "Total US Car Sales",
  subtitle = "Monthly, seasonally adjusted annual rate (millions of units)",
  y_lab = "Sales (millions)",
  caption = "Source: U.S. Bureau of Economic Analysis via FRED® | Frequency: Monthly")

plot_urban_cpi <- line_indicator(
  cpi_dat,
  yvar = value,
  title = "Consumer Price Index for All Urban Consumers",
  subtitle = "Monthly, seasonally adjusted (index base period: 1982–1984 = 100)",
  y_lab = "Index",
  caption = "Source: U.S. Bureau of Labor Statistics via FRED® | Frequency: Monthly")

plot_consumer_sent <- line_indicator(
  sentiment_dat,
  yvar = value,
  title = "Consumer Sentiment",
  subtitle = "University of Michigan Index of Consumer Sentiment (1966 baseline = 100)",
  y_lab = "Index",
  caption = "Source: University of Michigan Survey of Consumers via FRED® | Frequency: Monthly")

plot_consumer_loans <- line_indicator(
  consume_loans,
  yvar = value,
  title = "Consumer Loans, All Commercial Banks",
  subtitle = "Weekly, seasonally adjusted (billions of dollars)",
  y_lab = "USD (billions)",
  caption = "Source: Board of Governors of the Federal Reserve System (US) via FRED® | Frequency: Weekly")

plot_durable_goods <- line_indicator(
  durable_goods_dat,
  yvar = value,
  title = "Manufacturers' New Orders: Durable Goods, Seasonally Adjusted",
  subtitle = "Monthly, seasonally adjusted (millions of dollars)",
  y_lab = "USD (millions)",
  caption = "Source: U.S. Census Bureau via FRED® | Frequency: Monthly",
  y_pretty = TRUE)

plot_durables_no_defence <- line_indicator(
  durables_no_def_dat,
  yvar = value,
  title = "Manufacturers' New Orders: Durable Goods Excluding Defense, Seasonally Adjusted",
  subtitle = "Monthly, seasonally adjusted (millions of dollars)",
  y_lab = "USD (millions)",
  caption = "Source: U.S. Census Bureau via FRED® | Frequency: Monthly",
  y_pretty = TRUE)


# Define server logic to draw graphs
server <- function(input, output, session) {

  # unemployment
  output$plot_unrate <- renderPlot({
    plot_unrate
  })
  output$plot_swing_state_unrate <- renderPlot({
    plot_swing_state_unrate
  })
  output$plot_large_state_unrate <- renderPlot({
    plot_large_state_unrate
  })

  # job growth (wages, hiring)
  output$plot_adp_payroll <- renderPlot({
    plot_adp_payroll
  })
  output$plot_cosmos_listings <- renderPlot({
    plot_cosmos_listings
  })
  output$plot_jolts <- renderPlot({
    plot_jolts
  })

  # consumers
  output$plot_car_sales <- renderPlot({
    plot_car_sales
  })
  output$plot_urban_cpi <- renderPlot({
    plot_urban_cpi
  })
  output$plot_consumer_sent <- renderPlot({
    plot_consumer_sent
  })
  output$plot_consumer_loans <- renderPlot({
    plot_consumer_loans
  })
  output$plot_durable_goods <- renderPlot({
    plot_durable_goods
  })
  output$plot_durables_no_defence <- renderPlot({
    plot_durables_no_defence
  })

  # travel
  output$plot_tourist_origins <- renderPlot({
    plot_tourist_origins
  })
  output$plot_tsa_line <- renderPlot({
    plot_tsa_line
  })

  # disease spread
  output$plot_measles <- renderPlot({
    plot_measles
  })

  # markets
  output$plot_bond_10_yield <- renderPlot({
    plot_bond_10_yield
  })
  output$plot_spy <- renderPlot({
    plot_spy
  })
  output$plot_housing_starts <- renderPlot({
    plot_housing_starts
  })

}
