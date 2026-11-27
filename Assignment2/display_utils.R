# ------------------------------------------------------------------
# BDA400 - Assignment 2
# File: display_utils.R
# Purpose: my own functions to display the loaded stock data and the
#          statistics in a few different ways (tables, a Yahoo Finance
#          style quote summary, and charts).
# ------------------------------------------------------------------

library(quantmod)


# helper - format big numbers with commas
fmt_num <- function(x, digits = 2) {
  formatC(x, format = "f", digits = digits, big.mark = ",")
}


# ---------------------------------------------------------------
# 1) display_stock_data()
# Prints the first and last n rows of each stock's data frame.
# ---------------------------------------------------------------
display_stock_data <- function(stock_list, n = 5) {
  for (sym in names(stock_list)) {
    df <- stock_list[[sym]]
    cat("\n==================================================\n")
    cat(" ", sym, " - ", nrow(df), " trading days (",
        format(min(df$Date)), " to ", format(max(df$Date)), ")\n", sep = "")
    cat("==================================================\n")
    cat("First", n, "rows:\n")
    print(head(df, n), row.names = FALSE)
    cat("\nLast", n, "rows:\n")
    print(tail(df, n), row.names = FALSE)
  }
  invisible(stock_list)
}


# ---------------------------------------------------------------
# 2) display_quote_summary()
# Shows one stock the way Yahoo Finance shows the "Summary" box:
# previous close, open, day's range, 52 week range, volume, avg volume.
# ---------------------------------------------------------------
display_quote_summary <- function(stock_df, symbol = "") {
  n <- nrow(stock_df)
  last <- stock_df[n, ]
  prev <- stock_df[n - 1, ]

  change <- last$Close - prev$Close
  change_pct <- change / prev$Close * 100

  last_year <- stock_df[stock_df$Date > max(stock_df$Date) - 365, ]

  cat("\n--------------------------------------------\n")
  cat(symbol, "   ", fmt_num(last$Close), "  ",
      ifelse(change >= 0, "+", ""), fmt_num(change), " (",
      ifelse(change >= 0, "+", ""), fmt_num(change_pct), "%)\n", sep = "")
  cat("As of close on", format(last$Date, "%B %d, %Y"), "\n")
  cat("--------------------------------------------\n")
  cat(sprintf("%-18s %s\n", "Previous Close", fmt_num(prev$Close)))
  cat(sprintf("%-18s %s\n", "Open", fmt_num(last$Open)))
  cat(sprintf("%-18s %s - %s\n", "Day's Range", fmt_num(last$Low), fmt_num(last$High)))
  cat(sprintf("%-18s %s - %s\n", "52 Week Range",
              fmt_num(min(last_year$Low)), fmt_num(max(last_year$High))))
  cat(sprintf("%-18s %s\n", "Volume", fmt_num(last$Volume, 0)))
  cat(sprintf("%-18s %s\n", "Avg. Volume", fmt_num(mean(last_year$Volume), 0)))
  invisible(NULL)
}


# ---------------------------------------------------------------
# 3) display_statistics()
# Prints the combined statistics table, rounded. Uses knitr::kable
# for a nicer table if knitr is installed.
# ---------------------------------------------------------------
display_statistics <- function(stats_table, digits = 2) {
  tbl <- stats_table
  num_cols <- sapply(tbl, is.numeric)
  tbl[num_cols] <- lapply(tbl[num_cols], round, digits)

  if (requireNamespace("knitr", quietly = TRUE)) {
    print(knitr::kable(tbl, caption = "Basic statistics (Close price)"))
  } else {
    print(tbl, row.names = FALSE)
  }
  invisible(tbl)
}


# ---------------------------------------------------------------
# 4) plot_price_with_ma()
# Line chart of the closing price with the moving average on top.
# ---------------------------------------------------------------
plot_price_with_ma <- function(stats_result, symbol = "", ma_window = 20) {
  ma <- stats_result$moving_average
  s  <- stats_result$summary

  plot(ma$Date, ma$Close, type = "l", col = "steelblue", lwd = 1.5,
       main = paste(symbol, "Closing Price with", ma_window, "Day SMA"),
       xlab = "Date", ylab = "Price")
  lines(ma$Date, ma$SMA, col = "darkorange", lwd = 2)
  abline(h = s$Mean, col = "grey40", lty = 2)
  legend("topleft",
         legend = c("Close", paste0("SMA ", ma_window), "Mean"),
         col = c("steelblue", "darkorange", "grey40"),
         lty = c(1, 1, 2), lwd = c(1.5, 2, 1), bty = "n")
}


# ---------------------------------------------------------------
# 5) plot_candlestick()
# Candlestick chart with volume and SMA using quantmod::chartSeries.
# chartSeries needs an xts object so the data frame is converted back.
# ---------------------------------------------------------------
plot_candlestick <- function(stock_df, symbol = "", ma_window = 20, last_n_days = 90) {
  df <- tail(stock_df, last_n_days)
  stock_xts <- xts(df[, c("Open", "High", "Low", "Close", "Volume")],
                   order.by = df$Date)
  chartSeries(stock_xts, name = symbol, type = "candlesticks",
              theme = chartTheme("white"),
              TA = paste0("addVo(); addSMA(n = ", ma_window, ")"))
}


# ---------------------------------------------------------------
# 6) plot_normalized_comparison()
# Compares all stocks on one chart by rebasing each one to 100 on the
# first day, so stocks with very different prices can be compared.
# ---------------------------------------------------------------
plot_normalized_comparison <- function(stock_list) {
  normalized <- lapply(stock_list, function(df) {
    data.frame(Date = df$Date, Index = df$Close / df$Close[1] * 100)
  })

  y_range <- range(unlist(lapply(normalized, function(d) d$Index)))
  # NOTE: do.call(c, ...) on a named list drops the Date class and gives
  # plain numbers (days since 1970), so the axis showed 20500, 20600...
  # unname() + as.Date() keeps them as real dates.
  x_range <- as.Date(range(do.call(c, unname(lapply(normalized, function(d) d$Date)))))
  cols <- rainbow(length(normalized))

  # plot the first stock with type = "n" so R draws a proper date axis
  plot(normalized[[1]]$Date, normalized[[1]]$Index, type = "n",
       xlim = x_range, ylim = y_range,
       main = "Portfolio Performance (rebased to 100)",
       xlab = "Date", ylab = "Index (start = 100)")
  abline(h = 100, lty = 2, col = "grey")
  for (i in seq_along(normalized)) {
    lines(normalized[[i]]$Date, normalized[[i]]$Index, col = cols[i], lwd = 1.5)
  }
  # legend in 4 columns at the bottom so it doesn't cover the lines
  legend("bottomleft", legend = names(normalized), col = cols, lwd = 2,
         ncol = 4, bg = "white", cex = 0.75)
}


# ---------------------------------------------------------------
# 7) plot_statistics_bar()
# Bar chart comparing mean price and standard deviation of each stock.
# ---------------------------------------------------------------
plot_statistics_bar <- function(stats_table) {
  m <- rbind(stats_table$Mean, stats_table$Std_Dev)
  colnames(m) <- stats_table$Symbol
  barplot(m, beside = TRUE, col = c("steelblue", "tomato"),
          main = "Mean Closing Price vs Standard Deviation",
          ylab = "Price", las = 2)
  legend("topright", legend = c("Mean", "Std Dev"),
         fill = c("steelblue", "tomato"), bty = "n")
}
