# ------------------------------------------------------------------
# BDA400 - Assignment 2: Technical Analysis using R (Preliminary Stage)
# File: main.R
# Runs the whole process: read portfolio -> load data -> statistics
# -> display tables and charts.
#
# Before running: Session > Set Working Directory > To Source File
# Location (so R can find portfolio.txt and the other scripts)
# ------------------------------------------------------------------

source("stock_functions.R")
source("display_utils.R")

# ---- Settings ----
PORTFOLIO_FILE <- "portfolio.txt"
START_DATE     <- Sys.Date() - 365   # last 1 year of data
END_DATE       <- Sys.Date()
MA_WINDOW      <- 20                 # 20 day simple moving average

# ---- Step 1: read the portfolio ----
symbols <- read_portfolio(PORTFOLIO_FILE)
cat("Stocks in portfolio:", paste(symbols, collapse = ", "), "\n")

# ---- Step 2: import data (one data frame per stock) ----
stocks <- load_stock_data(PORTFOLIO_FILE, from = START_DATE, to = END_DATE,
                          to_global = TRUE)
str(stocks, max.level = 1)

# ---- Step 3: calculate statistics ----
stats <- calculate_all_statistics(stocks, column = "Close", ma_window = MA_WINDOW)

# ---- Step 4: display data ----
display_stock_data(stocks, n = 5)

for (sym in names(stocks)) {
  display_quote_summary(stocks[[sym]], sym)
}

display_statistics(stats$table)

# ---- Step 5: charts ----
for (sym in names(stocks)) {
  plot_price_with_ma(stats$by_stock[[sym]], sym, MA_WINDOW)
  plot_candlestick(stocks[[sym]], sym, MA_WINDOW)
}
plot_normalized_comparison(stocks)
plot_statistics_bar(stats$table)

# ---- Save results so they can be used in Assignment 3 ----
write.csv(stats$table, "statistics_summary.csv", row.names = FALSE)
saveRDS(stocks, "stocks_data.rds")
cat("\nDone. Statistics saved to statistics_summary.csv\n")
