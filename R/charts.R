################################################################################
# charts.R — Shared chart-building helpers for the Quarto site
#
# Sourced by every category page (labor.qmd, consumers.qmd, ...). Turns the
# shared, prepared data frames from global.R into interactive plotly charts.
# Depends on the style tokens + theme_snapshot() defined in global.R.
################################################################################

library(ggplot2)
library(plotly)

# plotly_chart(): turn a ggplot into a plotly chart. rangeselector = TRUE adds
# the 1Y / 2Y / All time-window buttons under the chart.
# (Named plotly_chart, not interactive, because interactive() is a built-in R
#  function that knitr calls during rendering.)
plotly_chart <- function(p, rangeselector = TRUE) {
  # ggplotly drops the ggplot caption, so capture it and re-add it below as a
  # bottom annotation (the source credit), mirroring the Shiny app.
  caption <- p$labels$caption
  gp <- ggplotly(p) |>
    config(displayModeBar = FALSE, responsive = TRUE) |>
    layout(font = list(family = "Inter, system-ui, sans-serif"),
           # Unified hover: one tooltip per date listing every series, with a
           # clean date header.
           hovermode = "x unified")
  if (rangeselector) {
    # ggplotly writes date axes as plain numbers (days since 1970-01-01) on a
    # "linear" axis, and plotly silently drops rangeselector buttons unless
    # the axis is a true "date" axis. Convert each trace's x values to date
    # strings (and bar widths from day units to milliseconds) so the
    # range buttons actually render.
    ms_per_day <- 86400000
    gp$x$data <- lapply(gp$x$data, function(tr) {
      if (!is.null(tr$x)) {
        tr$x <- format(as.Date(unlist(tr$x), origin = "1970-01-01"))
        if (identical(tr$type, "bar") && !is.null(tr$width)) {
          tr$width <- unlist(tr$width) * ms_per_day
        }
      }
      tr
    })
    gp$x$layout$xaxis <- modifyList(gp$x$layout$xaxis, list(
      type = "date", autorange = TRUE, tickvals = NULL, ticktext = NULL,
      # Negative y with yanchor "top" drops the buttons just below the x-axis
      # labels; a light pill background makes them easy to notice.
      rangeselector = list(
        x = 0, xanchor = "left", y = -0.14, yanchor = "top",
        font = list(size = 11, color = "#6b7280"),
        bgcolor = "#f1f1f3", activecolor = "#e4e4fb",
        bordercolor = "#e2e2e6", borderwidth = 1,
        buttons = list(
          list(count = 1, label = "1Y", step = "year", stepmode = "backward"),
          list(count = 2, label = "2Y", step = "year", stepmode = "backward"),
          list(step = "all", label = "All")
        )
      ),
      rangeslider = list(visible = FALSE)
    ))
  }
  # Hover + number formatting. By default ggplotly labels the value with the
  # raw ggplot expression (e.g. "persons/1e6") and shows unformatted numbers.
  # Replace the label with the y-axis title (which carries the unit, e.g.
  # "Persons (millions)"), or the series name on multi-series charts, and
  # format numbers with thousands separators + trimmed decimals.
  value_fmt <- ",.4~f"          # 1,234.5 style; trailing zeros trimmed
  ylab <- gp$x$layout$yaxis$title
  if (is.list(ylab)) ylab <- ylab$text
  if (is.null(ylab) || !nzchar(ylab)) ylab <- "Value"

  is_text_only <- function(tr) !is.null(tr$mode) && grepl("text", tr$mode) &&
                               !grepl("lines|markers", tr$mode)
  n_series <- sum(vapply(gp$x$data, function(tr) !is_text_only(tr), logical(1)))

  gp$x$data <- lapply(gp$x$data, function(tr) {
    if (!is_text_only(tr)) {
      # Single-series: label with the unit (y-axis title). Multi-series: label
      # each line with its series name (state / country).
      label <- if (n_series > 1 && !is.null(tr$name) && nzchar(tr$name))
        tr$name else ylab
      tr$hovertemplate <- paste0(label, ": %{y:", value_fmt, "}<extra></extra>")
      # Drop ggplotly's default tooltip text (holds the raw variable name).
      tr$text <- NULL
    } else {
      # On-chart text labels shouldn't appear in the unified hover box.
      tr$hoverinfo <- "skip"
    }
    tr
  })
  gp$x$layout$yaxis$tickformat <- value_fmt
  gp$x$layout$xaxis$hoverformat <- "%b %d, %Y"

  # Source credit on its own line below the buttons (right-aligned). Anchored
  # to the bottom of the plot (y = 0) and pushed down a fixed number of pixels
  # (yshift), so it lands in the bottom margin regardless of the chart's
  # height — a fraction-based y gets clipped on tall charts. Its own line means
  # it never collides with the buttons on narrow / mobile widths.
  if (!is.null(caption)) {
    gp$x$layout$annotations <- c(gp$x$layout$annotations, list(list(
      text = caption, showarrow = FALSE,
      xref = "paper", yref = "paper",
      x = 1, xanchor = "right", y = 0, yanchor = "top", yshift = -76,
      font = list(size = 9, color = "#bcbcbc")
    )))
  }
  # Room under the x-axis for the button row + the source line beneath it.
  gp$x$layout$margin$b <- 98
  # Titles are rendered as HTML above each chart (so they wrap), so the plot
  # itself needs only a small top margin.
  gp$x$layout$margin$t <- 20
  gp
}

# line_chart(): standard single-line time series, then made interactive.
# The chart title is rendered as HTML above the chart (see the .chart-title
# element on each page), not inside the plot, so it can wrap.
line_chart <- function(df, yvar, y_lab, caption, xvar = date,
                       line_color = LINE_COLOR, show_election = TRUE) {
  p <- ggplot(df, aes(x = {{ xvar }}, y = {{ yvar }})) +
    geom_line(color = line_color, linewidth = 0.5) +
    labs(x = NULL, y = y_lab, caption = caption) +
    theme_snapshot()
  if (show_election) {
    p <- p + geom_vline(xintercept = ELECTION_DATE,
                        linetype = "dashed", color = ELECTION_COLOR)
  }
  plotly_chart(p)
}
