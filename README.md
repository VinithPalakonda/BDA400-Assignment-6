# BDA400 Assignment 6 — Technical Analysis using R, Visualization Phase

**Student:** Vinith Palakonda  
**Course:** BDA400 — Data Science Tools and Techniques  
**Assignment:** Assignment 6 — Technical Analysis using R, Visualization Phase  
**Repository:** https://github.com/VinithPalakonda/BDA400-Assignment-6

## Project Description

This project is an interactive R Shiny portfolio dashboard that retrieves historical stock-market data from Yahoo Finance using the `quantmod` package.

The application allows users to enter a stock symbol, select a date range, choose Daily, Weekly, or Monthly time frames, and visualize the selected stock using Line, Candlestick, or Area charts.

The dashboard also calculates and displays the following technical indicators:

- Short-term and long-term Simple Moving Averages (SMA)
- Relative Strength Index (RSI)
- Moving Average Convergence Divergence (MACD)

Each technical indicator can be turned on or off from the dashboard, and the indicator parameters can be customized by the user.

## Trading Strategy

The dashboard uses a Moving Average Crossover strategy:

- **Buy:** generated when the short-term moving average crosses above the long-term moving average.
- **Sell:** generated when the short-term moving average crosses below the long-term moving average.
- **Hold:** used when no crossover signal is generated.

Buy and Sell events are annotated directly on the stock-price chart.

## Application Features

- Historical stock data retrieved from Yahoo Finance
- Interactive stock-symbol input
- User-selected date range
- Daily, Weekly, and Monthly time frames
- Line, Candlestick, and Area chart options
- Moving Average overlays
- RSI overlay
- MACD overlay
- Individual on/off controls for technical indicators
- Customizable indicator parameters
- Buy, Sell, and Hold trading logic
- Buy/Sell annotations on the chart
- Latest trading-state display
- Recent-data preview
- Error handling for invalid ticker symbols, unavailable data, invalid date ranges, and incompatible settings

## Required R Packages

```r
install.packages(c(
  "shiny",
  "ggplot2",
  "quantmod",
  "TTR",
  "dplyr",
  "tidyr",
  "lubridate",
  "scales"
))
```

## How to Run the Application

1. Open RStudio.
2. Open `app.R`.
3. Install the required packages if they are not already installed.
4. Click **Run App**.
5. Enter a stock ticker such as `AAPL`, `MSFT`, or `TSLA`.
6. Select the desired date range, chart type, time frame, and technical indicators.
7. Click **Refresh Data** to update the dashboard.

## Example Settings

- **Stock Symbol:** AAPL
- **Time Frame:** Daily
- **Chart Type:** Candlestick
- **Short Moving Average:** 20
- **Long Moving Average:** 50
- **RSI Period:** 14
- **MACD:** 12, 26, 9

## GitHub Repository

https://github.com/VinithPalakonda/BDA400-Assignment-6

## Author

**Vinith Palakonda**
