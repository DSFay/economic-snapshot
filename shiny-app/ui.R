library(shiny)
library(dplyr)   # for the %>% pipe used in the layout below


# Shared description style used across all chart blurbs
desc_style <- "font-size:14px; max-width:600px; text-align:justify; margin: 8px 0 20px 0; color:#444;"

ui <- navbarPage(
  title = "Economic Snapshot",
  id = "mainnav",
  windowTitle = "Economic Snapshot",

  # Header: CSS + JS
  header = tagList(
    tags$style(HTML("
      /* Hide the Home tab */
      a[data-value='home'] { display: none !important; }

      /* Center the brand */
      .navbar-brand {
        position: absolute;
        left: 50%;
        transform: translateX(-50%);
      }

      /* Make navbar a flex container for the tabs */
      .navbar-nav { display: flex; width: 100%; }

      /* Push About tab to the right */
      .navbar-nav li.about-tab { margin-left: auto; }

       /* Make the brand text larger */
      .navbar-brand {
      font-size: 30px;
      }

      /* Add formatting for the intro text */
      .intro {
      max-width: 600px;
      margin: 60px auto 40px auto;
      font-size: 14px;
      text-align: justify;
      }

      /* Add styling applied to entire app */
         body {
        padding-left: 25px;
        padding-right: 25px;
      }

      /* Chart description blurbs */
      .chart-desc {
        font-size: 14px;
        max-width: 600px;
        text-align: justify;
        margin: 8px 0 20px 0;
        color: #444;
      }

    ")),
    tags$script(HTML("
      $(function() {
        // Clicking brand opens Home tab
        $('.navbar-brand').on('click', function(e){
          e.preventDefault();
          $('a[data-value=\"home\"]').tab('show');
        });

        // Add class to About tab li to push it right
        $('a[data-value=\"About\"]').closest('li').addClass('about-tab');
      });
    "))
  ),

  ## HOME TAB
  tabPanel("Home", value = "home",
           fluidPage(
             fluidRow(
               column(12, HTML("<p class='intro'><b>Economic Snapshot</b>
                               Collects, synthesizes, and visualizes core economic indicators for
                               labor, consumers, travel, public health, and financial markets,
                               for the United States in a centralized view.
                               Data are visualized with no or minimal pre-processing
                               and use the highest available frequencies to allow charts
                               to reflect the maximum amount of information.</p>"),
               )
             )
           ),

           ## ─────────────────────── Labor Indicators ───────────────────────

           fluidRow(
             column(12, tags$h3("Labor Indicators",
                                style = "margin-top:80px; margin-bottom:30px; text-align:center;"))
           ),

           # row 1: National unemployment + ADP payroll
           fluidRow(
             column(6,
                    plotOutput("plot_unrate"),
                    tags$p(class = "chart-desc",
                           "The national unemployment rate measures the share of the labor force
                           that is jobless and actively seeking work. The dotted blue line shows
                           the period average. Updated monthly by the Bureau of Labor Statistics.")),
             column(6,
                    plotOutput("plot_adp_payroll"),
                    tags$p(class = "chart-desc",
                           "Total nonfarm private payroll employment tracks the number of paid
                           workers across all industries except government, farm, private household,
                           and non-profit employees. Published monthly by ADP."))
           ) %>% div(style = "margin-bottom:50px;"),

           # row 2: State unemployment claims
           fluidRow(
             column(6,
                    plotOutput("plot_swing_state_unrate"),
                    tags$p(class = "chart-desc",
                           "Weekly continued unemployment insurance claims as a percentage of each
                           state's civilian labor force, for key swing states (WI, MI, PA, GA, NC,
                           NV, AZ). Higher values indicate more workers receiving ongoing benefits
                           relative to the workforce. Updated weekly.")),
             column(6,
                    plotOutput("plot_large_state_unrate"),
                    tags$p(class = "chart-desc",
                           "Weekly continued unemployment insurance claims as a percentage of each
                           state's civilian labor force, for the five largest states by population
                           plus DC. Updated weekly."))),

           # row 3: Job listings (Revelio + JOLTS side by side)
           fluidRow(
             column(6,
                    plotOutput("plot_cosmos_listings"),
                    tags$p(class = "chart-desc",
                           "New online job postings aggregated weekly by year, sourced from Revelio
                           Labs via Wharton Research Data Services. Data are collected and
                           deduplicated from major job boards including LinkedIn and Indeed. Each
                           line represents a calendar year, enabling year-over-year comparison.")),
             column(6,
                    plotOutput("plot_jolts"),
                    tags$p(class = "chart-desc",
                           "Total job openings from the Bureau of Labor Statistics Job Openings and
                           Labor Turnover Survey (JOLTS). Unlike the Revelio postings data at left,
                           JOLTS counts all open positions at the end of each reference month
                           regardless of how they are advertised. Updated monthly."))
           ) %>% div(style = "margin-top:60px;"),

           ## ───────────────────────── Consumer Indicators ─────────────────────────
           fluidRow(
             column(12, tags$h3("Consumer Indicators", style = "margin-top:125px; margin-bottom:60px; text-align:center;"))
           ),

           fluidRow(
             column(6,
                    plotOutput("plot_car_sales"),
                    tags$p(class = "chart-desc",
                           "Total light vehicle sales in the United States, reported at a
                           seasonally adjusted annual rate (SAAR). Includes both new domestic and
                           imported cars and light trucks. Published monthly by the Bureau of
                           Economic Analysis.")),
             column(6,
                    plotOutput("plot_consumer_loans"),
                    tags$p(class = "chart-desc",
                           "Outstanding consumer loan balances held by all commercial banks,
                           including credit card, auto, and personal loans, but excluding
                           mortgages. Reported weekly by the Federal Reserve and serves as a
                           broad measure of household borrowing activity."))
           ) %>% div(style = "margin-bottom:50px;"),

           fluidRow(
             column(6,
                    plotOutput("plot_consumer_sent"),
                    tags$p(class = "chart-desc",
                           HTML("The University of Michigan Index of Consumer Sentiment measures
                           household attitudes toward personal finances, business conditions, and
                           buying conditions. The index is benchmarked to a baseline of
                           <b>100 in 1966</b>: values above 100 reflect above-average confidence;
                           values below 100 indicate below-average confidence. A sustained decline
                           often precedes reduced consumer spending. Updated monthly."))),
             column(6,
                    plotOutput("plot_urban_cpi"),
                    tags$p(class = "chart-desc",
                           "The Consumer Price Index for All Urban Consumers (CPI-U) tracks changes
                           in the average prices paid by urban consumers for a basket of goods and
                           services. The index is benchmarked to 100 in the 1982–84 base period;
                           the chart shows the level rather than the year-over-year change. Updated
                           monthly by the Bureau of Labor Statistics."))
           ),

           fluidRow(
             column(6,
                    plotOutput("plot_durable_goods"),
                    tags$p(class = "chart-desc",
                           "New orders placed with domestic manufacturers for durable goods —
                           items expected to last three or more years, such as aircraft, machinery,
                           and electronics. A leading indicator of manufacturing activity.
                           Published monthly, seasonally adjusted, by the U.S. Census Bureau.")),
             column(6,
                    plotOutput("plot_durables_no_defence"),
                    tags$p(class = "chart-desc",
                           "New durable goods orders excluding defense expenditures, isolating
                           private-sector demand for capital equipment. The defense component can
                           be lumpy and contract-driven, so this series provides a cleaner read on
                           underlying business investment trends. Published monthly by the U.S.
                           Census Bureau."))
           ) %>% div(style = "margin-bottom:50px;"),

           ## ─────────────────────────── Travel Indicators ───────────────────────────
           fluidRow(
             column(12, tags$h3("Travel Indicators", style = "margin-top:125px; margin-bottom:60px; text-align:center;"))
           ),

           fluidRow(
             column(6,
                    plotOutput("plot_tourist_origins"),
                    tags$p(class = "chart-desc",
                           "Monthly arrivals to the United States from the top 10 source countries,
                           as recorded in I-94 border crossing data. All 10 countries are shown
                           with distinct colors. Data are collected and published by the
                           International Trade Administration.")),
             column(6,
                    plotOutput("plot_tsa_line"),
                    tags$p(class = "chart-desc",
                           "Transportation Security Administration (TSA) checkpoint encounters,
                           aggregated from daily to weekly totals. Weekly aggregation smooths
                           day-of-week variation to make seasonal and trend patterns more visible.
                           Data are published daily by the TSA."))
           ),

           ## ──────────────────────── Public Health Indicators ────────────────────────
           fluidRow(
             column(12, tags$h3("Public Health Indicators", style = "margin-top:125px; margin-bottom:60px; text-align:center;"))
           ),

           fluidRow(
             column(6,
                    plotOutput("plot_measles"),
                    tags$p(class = "chart-desc",
                           "Confirmed measles cases in the United States by week of rash onset.
                           Notable outbreak periods are labeled. Cases were near zero for several
                           years before a surge beginning in late 2024 linked primarily to
                           unvaccinated communities. Data are published weekly by the CDC.")),
             column(6,
                    tags$div(style = "margin-top:30px;",
                             tags$h5("About Measles Outbreaks", style = "font-weight:bold; color:#444;"),
                             tags$p(class = "chart-desc",
                                    HTML("Measles was declared <b>eliminated</b> in the United States in
                                    2000. Since then, cases have occurred primarily in unvaccinated
                                    individuals or through importation from countries with ongoing
                                    transmission.<br><br>
                                    The <b>2025 TX/NM Outbreak</b> was centered in a low-vaccination
                                    community in West Texas and spread to New Mexico, representing
                                    the largest U.S. outbreak in recent years.<br><br>
                                    The <b>2026 TX Outbreak</b> peaked in January 2026, with cases
                                    concentrated among school-age children in counties with
                                    below-average MMR vaccination rates.<br><br>
                                    The CDC recommends two doses of the MMR (measles-mumps-rubella)
                                    vaccine for full protection. Measles is highly contagious —
                                    one infected person can spread the virus to 9–18 others."))))
           ),

           ## ───────────────────────── Market Indicators ─────────────────────────
           fluidRow(
             column(12, tags$h3("Market Indicators", style = "margin-top:125px; margin-bottom:60px; text-align:center;"))
           ),

           fluidRow(
             column(6,
                    plotOutput("plot_bond_10_yield"),
                    tags$p(class = "chart-desc",
                           "The yield on 10-year U.S. Treasury securities, quoted on an investment
                           basis at constant maturity. This rate serves as a benchmark for long-term
                           borrowing costs across the economy, including mortgage rates and corporate
                           bonds. Published daily by the Federal Reserve.")),
             column(6,
                    plotOutput("plot_spy"),
                    tags$p(class = "chart-desc",
                           "The S&P 500 index tracks the market capitalization-weighted performance
                           of 500 large U.S. publicly traded companies. It is widely used as a
                           proxy for the overall U.S. equity market. Published daily."))
           ) %>% div(style = "margin-bottom:50px;"),

           fluidRow(
             column(6, offset = 3,
                    plotOutput("plot_housing_starts"),
                    tags$p(class = "chart-desc",
                           "New privately-owned housing units started, reported at a seasonally
                           adjusted annual rate. Housing starts are a leading indicator of
                           construction activity and broadly reflect demand conditions in the
                           housing market. Published monthly by the U.S. Census Bureau and the
                           Department of Housing and Urban Development."))
           ) %>% div(style = "margin-bottom:200px;")
  ),


  # 2nd tab: About
  tabPanel("About",
           fluidPage(
             fluidRow(
               column(12,
                      tags$p(
                        HTML("Economic Snapshot was developed by Faelynn Carroll, Ethan Kapstein, and Jacob N. Shapiro."),
                        style = "text-align:center; font-size:14px; max-width:700px; margin:50px auto;"
                      )
               )
             )
           )
  )
)
