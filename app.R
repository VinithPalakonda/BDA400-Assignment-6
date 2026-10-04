# ============================================================
# Student: Vinith Palakonda
# Course: BDA400 - Data Science Tools and Techniques
# Assignment 6 - Technical Analysis using R, Visualization Phase
# Project: Portfolio Visualization Dashboard using R Shiny
# Repository: https://github.com/VinithPalakonda/BDA400-Assignment-6
# ============================================================

# ---------------------------
# 0. Install packages once
# ---------------------------
# Uncomment and run once if needed:
# install.packages(c(
#   "shiny", "ggplot2", "quantmod", "TTR", "dplyr",
#   "tidyr", "lubridate", "scales"
# ))

# ---------------------------
# 1. Load libraries
# ---------------------------
library(shiny)
library(ggplot2)
library(quantmod)
library(TTR)
library(dplyr)
library(tidyr)
library(lubridate)
library(scales)

# ---------------------------
# 2. Helper functions
# ---------------------------

# Safely fetch stock data.
# Yahoo Finance is the primary source required by the assignment.
# If Yahoo fails, the app gives a clear error message instead of crashing.
fetch_stock_data <- function(symbol, from, to) {
  tryCatch(
    {
      x <- suppressWarnings(
        getSymbols(
          Symbols = symbol,
          src = "yahoo",
          from = from,
          to = to,
          auto.assign = FALSE
        )
      )

      if (NROW(x) == 0) {
        stop("No data was returned for this symbol and date range.")
      }

      x
    },
    error = function(e) {
      stop(
        paste0(
          "Unable to fetch data from Yahoo Finance. ",
          "Check the ticker symbol, internet connection, and date range. ",
          "Details: ", e$message
        )
      )
    }
  )
}

# Convert an xts OHLC object to a standard data frame.
xts_to_ohlc_df <- function(x) {
  out <- data.frame(
    Date = as.Date(index(x)),
    Open = as.numeric(Op(x)),
    High = as.numeric(Hi(x)),
    Low = as.numeric(Lo(x)),
    Close = as.numeric(Cl(x)),
    Volume = as.numeric(Vo(x)),
    Adjusted = as.numeric(Ad(x))
  )

  out %>% filter(complete.cases(Open, High, Low, Close))
}

# Aggregate daily data into weekly or monthly OHLC bars.
aggregate_timeframe <- function(df, timeframe) {
  if (timeframe == "Daily") {
    return(df)
  }

  period_col <- if (timeframe == "Weekly") {
    floor_date(df$Date, unit = "week", week_start = 1)
  } else {
    floor_date(df$Date, unit = "month")
  }

  df %>%
    mutate(Period = period_col) %>%
    group_by(Period) %>%
    summarise(
      Date = min(Date),
      Open = first(Open),
      High = max(High, na.rm = TRUE),
      Low = min(Low, na.rm = TRUE),
      Close = last(Close),
      Volume = sum(Volume, na.rm = TRUE),
      Adjusted = last(Adjusted),
      .groups = "drop"
    ) %>%
    arrange(Date)
}

# Add technical indicators and trading signals.
add_indicators <- function(df, short_n, long_n, rsi_n, macd_fast, macd_slow, macd_signal) {

  validate_n <- function(n, minimum = 1) {
    max(minimum, as.integer(n))
  }

  short_n <- validate_n(short_n, 2)
  long_n <- validate_n(long_n, short_n + 1)
  rsi_n <- validate_n(rsi_n, 2)
  macd_fast <- validate_n(macd_fast, 2)
  macd_slow <- validate_n(macd_slow, macd_fast + 1)
  macd_signal <- validate_n(macd_signal, 2)

  df$SMA_Short <- SMA(df$Close, n = short_n)
  df$SMA_Long <- SMA(df$Close, n = long_n)
  df$RSI <- RSI(df$Close, n = rsi_n)

  macd_obj <- MACD(
    df$Close,
    nFast = macd_fast,
    nSlow = macd_slow,
    nSig = macd_signal,
    maType = "EMA",
    percent = FALSE
  )

  df$MACD <- as.numeric(macd_obj[, 1])
  df$MACD_Signal <- as.numeric(macd_obj[, 2])

  # Current state (Buy / Sell / Hold) from moving-average relationship
  df$Position <- case_when(
    is.na(df$SMA_Short) | is.na(df$SMA_Long) ~ "Hold",
    df$SMA_Short > df$SMA_Long ~ "Buy",
    df$SMA_Short < df$SMA_Long ~ "Sell",
    TRUE ~ "Hold"
  )

  # True crossover events used for annotations.
  prev_short <- lag(df$SMA_Short)
  prev_long <- lag(df$SMA_Long)

  df$Signal <- case_when(
    !is.na(prev_short) & !is.na(prev_long) &
      prev_short <= prev_long & df$SMA_Short > df$SMA_Long ~ "Buy",
    !is.na(prev_short) & !is.na(prev_long) &
      prev_short >= prev_long & df$SMA_Short < df$SMA_Long ~ "Sell",
    TRUE ~ "Hold"
  )

  df
}

# Scale a numeric indicator to the stock-price range so it can be
# visually overlaid on the same chart while preserving its shape.
scale_to_price <- function(x, price_low, price_high) {
  if (all(is.na(x))) return(rep(NA_real_, length(x)))

  x_rng <- range(x, na.rm = TRUE)
  p_rng <- c(price_low, price_high)

  if (diff(x_rng) == 0 || diff(p_rng) == 0) {
    return(rep(mean(p_rng), length(x)))
  }

  (x - x_rng[1]) / diff(x_rng) * diff(p_rng) + p_rng[1]
}

# ---------------------------
# 3. User Interface
# ---------------------------

ui <- fluidPage(
  titlePanel("BDA400 Portfolio Technical Analysis Dashboard"),

  sidebarLayout(
    sidebarPanel(
      textInput(
        "symbol",
        "Stock Symbol:",
        value = "AAPL",
        placeholder = "e.g., AAPL, MSFT, TSLA"
      ),

      dateRangeInput(
        "date_range",
        "Select Date Range:",
        start = Sys.Date() - 365,
        end = Sys.Date(),
        max = Sys.Date()
      ),

      selectInput(
        "time_frame",
        "Select Time Frame:",
        choices = c("Daily", "Weekly", "Monthly"),
        selected = "Daily"
      ),

      selectInput(
        "chart_type",
        "Select Chart Type:",
        choices = c("Line", "Candlestick", "Area"),
        selected = "Candlestick"
      ),

      checkboxGroupInput(
        "technical_indicators",
        "Technical Indicators:",
        choices = c(
          "Moving Averages" = "MA",
          "RSI" = "RSI",
          "MACD" = "MACD"
        ),
        selected = c("MA")
      ),

      tags$hr(),
      h4("Indicator Parameters"),

      fluidRow(
        column(
          6,
          numericInput("short_ma", "Short MA:", value = 20, min = 2, step = 1)
        ),
        column(
          6,
          numericInput("long_ma", "Long MA:", value = 50, min = 3, step = 1)
        )
      ),

      numericInput("rsi_period", "RSI Period:", value = 14, min = 2, step = 1),

      fluidRow(
        column(
          4,
          numericInput("macd_fast", "MACD Fast:", value = 12, min = 2, step = 1)
        ),
        column(
          4,
          numericInput("macd_slow", "MACD Slow:", value = 26, min = 3, step = 1)
        ),
        column(
          4,
          numericInput("macd_signal", "Signal:", value = 9, min = 2, step = 1)
        )
      ),

      checkboxInput(
        "show_signals",
        "Show Buy/Sell annotations",
        value = TRUE
      ),

      actionButton("refresh", "Refresh Data", class = "btn-primary"),

      tags$hr(),
      helpText(
        "Trading rule: Buy when the short moving average crosses above ",
        "the long moving average; Sell when it crosses below; otherwise Hold."
      )
    ),

    mainPanel(
      tabsetPanel(
        tabPanel(
          "Price & Signals",
          br(),
          plotOutput("stock_chart", height = "650px")
        ),
        tabPanel(
          "Latest Signal",
          br(),
          h3(textOutput("latest_signal")),
          tableOutput("latest_values")
        ),
        tabPanel(
          "Data Preview",
          br(),
          tableOutput("data_table")
        ),
        tabPanel(
          "Method",
          br(),
          h4("Dashboard Methodology"),
          tags$ul(
            tags$li("Historical OHLC stock data is fetched from Yahoo Finance using quantmod."),
            tags$li("The selected date range is filtered reactively."),
            tags$li("Daily data can be aggregated into weekly or monthly OHLC bars."),
            tags$li("Chart type can be switched among line, candlestick, and area."),
            tags$li("Moving Averages, RSI, and MACD can each be turned on or off."),
            tags$li("Buy/Sell signals are generated from moving-average crossover events."),
            tags$li("Signal annotations are displayed directly on the price chart.")
          )
        )
      )
    )
  )
)

# ---------------------------
# 4. Server Logic
# ---------------------------

server <- function(input, output, session) {

  raw_data <- eventReactive(
    input$refresh,
    {
      req(input$symbol, input$date_range)

      validate(
        need(nchar(trimws(input$symbol)) > 0, "Please enter a stock symbol."),
        need(
          input$date_range[1] < input$date_range[2],
          "The start date must be before the end date."
        )
      )

      withProgress(message = "Fetching stock data...", value = 0.4, {
        x <- fetch_stock_data(
          symbol = toupper(trimws(input$symbol)),
          from = input$date_range[1],
          to = input$date_range[2] + 1
        )
        incProgress(0.6)
        x
      })
    },
    ignoreNULL = FALSE
  )

  prepared_data <- reactive({
    x <- raw_data()
    req(x)

    df <- xts_to_ohlc_df(x)

    # Extra defensive filtering using the selected dates.
    df <- df %>%
      filter(
        Date >= as.Date(input$date_range[1]),
        Date <= as.Date(input$date_range[2])
      )

    df <- aggregate_timeframe(df, input$time_frame)

    validate(
      need(nrow(df) >= 5, "Not enough observations for the selected settings.")
    )

    add_indicators(
      df,
      short_n = input$short_ma,
      long_n = input$long_ma,
      rsi_n = input$rsi_period,
      macd_fast = input$macd_fast,
      macd_slow = input$macd_slow,
      macd_signal = input$macd_signal
    )
  })

  output$stock_chart <- renderPlot({
    df <- prepared_data()
    req(df)

    validate(
      need(
        input$short_ma < input$long_ma,
        "Short MA must be smaller than Long MA."
      ),
      need(
        input$macd_fast < input$macd_slow,
        "MACD Fast period must be smaller than MACD Slow period."
      )
    )

    p <- ggplot(df, aes(x = Date))

    # ---------------------------
    # Base stock chart
    # ---------------------------
    if (input$chart_type == "Line") {
      p <- p +
        geom_line(aes(y = Close, color = "Close"), linewidth = 0.8)
    }

    if (input$chart_type == "Area") {
      p <- p +
        geom_area(
          aes(y = Close, fill = "Close"),
          alpha = 0.25
        ) +
        geom_line(aes(y = Close, color = "Close"), linewidth = 0.7)
    }

    if (input$chart_type == "Candlestick") {
      up_df <- df %>% filter(Close >= Open)
      down_df <- df %>% filter(Close < Open)

      # Width adapts to selected time frame.
      candle_width <- switch(
        input$time_frame,
        "Daily" = 0.65,
        "Weekly" = 4.0,
        "Monthly" = 18.0
      )

      p <- p +
        geom_segment(
          data = df,
          aes(x = Date, xend = Date, y = Low, yend = High),
          linewidth = 0.4,
          inherit.aes = FALSE
        ) +
        geom_rect(
          data = up_df,
          aes(
            xmin = as.numeric(Date) - candle_width / 2,
            xmax = as.numeric(Date) + candle_width / 2,
            ymin = Open,
            ymax = Close,
            fill = "Up"
          ),
          alpha = 0.75,
          inherit.aes = FALSE
        ) +
        geom_rect(
          data = down_df,
          aes(
            xmin = as.numeric(Date) - candle_width / 2,
            xmax = as.numeric(Date) + candle_width / 2,
            ymin = Close,
            ymax = Open,
            fill = "Down"
          ),
          alpha = 0.75,
          inherit.aes = FALSE
        )
    }

    # ---------------------------
    # Overlay Moving Averages
    # ---------------------------
    if ("MA" %in% input$technical_indicators) {
      p <- p +
        geom_line(
          aes(y = SMA_Short, color = paste0("SMA ", input$short_ma)),
          linewidth = 0.8,
          na.rm = TRUE
        ) +
        geom_line(
          aes(y = SMA_Long, color = paste0("SMA ", input$long_ma)),
          linewidth = 0.8,
          na.rm = TRUE
        )
    }

    # Determine price range for scaled overlays.
    p_low <- min(df$Low, na.rm = TRUE)
    p_high <- max(df$High, na.rm = TRUE)

    # ---------------------------
    # Overlay RSI
    # ---------------------------
    if ("RSI" %in% input$technical_indicators) {
      df$RSI_Scaled <- scale_to_price(df$RSI, p_low, p_high)

      p <- p +
        geom_line(
          data = df,
          aes(y = RSI_Scaled, color = "RSI (scaled)"),
          linewidth = 0.65,
          linetype = "dashed",
          na.rm = TRUE
        )
    }

    # ---------------------------
    # Overlay MACD
    # ---------------------------
    if ("MACD" %in% input$technical_indicators) {
      df$MACD_Scaled <- scale_to_price(df$MACD, p_low, p_high)
      df$MACD_Signal_Scaled <- scale_to_price(df$MACD_Signal, p_low, p_high)

      p <- p +
        geom_line(
          data = df,
          aes(y = MACD_Scaled, color = "MACD (scaled)"),
          linewidth = 0.65,
          na.rm = TRUE
        ) +
        geom_line(
          data = df,
          aes(y = MACD_Signal_Scaled, color = "MACD Signal (scaled)"),
          linewidth = 0.65,
          linetype = "dotdash",
          na.rm = TRUE
        )
    }

    # ---------------------------
    # Buy / Sell annotations
    # ---------------------------
    if (isTRUE(input$show_signals)) {
      signal_df <- df %>% filter(Signal %in% c("Buy", "Sell"))

      if (nrow(signal_df) > 0) {
        p <- p +
          geom_point(
            data = signal_df,
            aes(y = Close, shape = Signal, color = Signal),
            size = 3,
            inherit.aes = FALSE
          ) +
          geom_text(
            data = signal_df,
            aes(
              x = Date,
              y = Close,
              label = Signal
            ),
            vjust = -0.9,
            size = 3.5,
            fontface = "bold",
            inherit.aes = FALSE
          )
      }
    }

    p +
      labs(
        title = paste(
          toupper(trimws(input$symbol)),
          "-",
          input$time_frame,
          input$chart_type,
          "Chart"
        ),
        subtitle = paste(
          "Range:",
          format(input$date_range[1], "%Y-%m-%d"),
          "to",
          format(input$date_range[2], "%Y-%m-%d")
        ),
        x = "Date",
        y = "Price",
        color = "Series",
        fill = "Series",
        shape = "Signal",
        caption = paste(
          "Source: Yahoo Finance |",
          "RSI/MACD are scaled only for visual overlay on the price axis."
        )
      ) +
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
      scale_y_continuous(labels = dollar_format()) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        panel.grid.minor = element_blank()
      )
  })

  output$latest_signal <- renderText({
    df <- prepared_data()
    req(df)

    latest <- tail(df, 1)

    paste0(
      "Current Trading State: ",
      latest$Position,
      " | Last Close: ",
      dollar(round(latest$Close, 2))
    )
  })

  output$latest_values <- renderTable({
    df <- prepared_data()
    req(df)

    latest <- tail(df, 1)

    data.frame(
      Metric = c(
        "Date",
        "Close",
        paste0("SMA ", input$short_ma),
        paste0("SMA ", input$long_ma),
        paste0("RSI ", input$rsi_period),
        "MACD",
        "MACD Signal",
        "Trading State"
      ),
      Value = c(
        as.character(latest$Date),
        round(latest$Close, 2),
        round(latest$SMA_Short, 2),
        round(latest$SMA_Long, 2),
        round(latest$RSI, 2),
        round(latest$MACD, 4),
        round(latest$MACD_Signal, 4),
        latest$Position
      ),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$data_table <- renderTable({
    df <- prepared_data()
    req(df)

    df %>%
      select(
        Date, Open, High, Low, Close, Volume,
        SMA_Short, SMA_Long, RSI, MACD, MACD_Signal,
        Position, Signal
      ) %>%
      tail(15) %>%
      mutate(
        across(
          where(is.numeric),
          ~ round(.x, 2)
        )
      )
  }, striped = TRUE, bordered = TRUE, spacing = "xs")
}

# ---------------------------
# 5. Run Application
# ---------------------------

shinyApp(ui = ui, server = server)
