# ------------------------------------------------------------------
# BDA400 - Assignment 2: Technical Analysis using R (Preliminary Stage)
# File: stock_functions.R
# Purpose: utility functions to read the portfolio, import stock data
#          with quantmod and compute basic statistics on it.
# ------------------------------------------------------------------

# Load required packages (install them first with install_packages.R)
library(quantmod)
library(TTR)


# ---------------------------------------------------------------
# read_portfolio()
# Reads portfolio.txt and returns a clean vector of stock symbols.
# Blank lines and lines starting with # are ignored.
# ---------------------------------------------------------------
read_portfolio <- function(file = "portfolio.txt") {
  if (!file.exists(file)) {
    stop("Portfolio file not found: ", file)
  }

  symbols <- readLines(file, warn = FALSE)
  symbols <- trimws(symbols)
  symbols <- symbols[symbols != "" & !startsWith(symbols, "#")]
  symbols <- unique(toupper(symbols))

  if (length(symbols) == 0) {
    stop("No stock symbols found in ", file)
  }
  symbols
}


# ---------------------------------------------------------------
# load_stock_data()
# Reads the symbols from portfolio.txt and downloads the daily price
# data for each one from Yahoo Finance using quantmod::getSymbols().
# Each stock is converted from xts into its own data frame.
#
# Returns: a named list of data frames, e.g. stocks$AAPL
# If to_global = TRUE each data frame is also created as a separate
# variable in the global environment (AAPL, MSFT, ...).
# ---------------------------------------------------------------
load_stock_data <- function(file = "portfolio.txt",
                            from = Sys.Date() - 365,
                            to = Sys.Date(),
                            src = "yahoo",
                            to_global = FALSE) {
  symbols <- read_portfolio(file)
  stock_list <- list()

  for (sym in symbols) {
    message("Downloading data for ", sym, " ...")

    # tryCatch so one bad symbol does not stop the whole portfolio
    stock_xts <- tryCatch(
      getSymbols(sym, src = src, from = from, to = to, auto.assign = FALSE),
      error = function(e) {
        warning("Could not download ", sym, ": ", conditionMessage(e))
        NULL
      }
    )

    if (is.null(stock_xts)) next

    # convert xts -> data frame and give the columns simple names
    # (quantmod names them like "AAPL.Open", "RY.TO.Close", ...)
    stock_df <- data.frame(Date = index(stock_xts),
                           coredata(stock_xts),
                           row.names = NULL)
    colnames(stock_df) <- c("Date", "Open", "High", "Low",
                            "Close", "Volume", "Adjusted")

    # remove days with missing prices (holidays etc.)
    stock_df <- stock_df[!is.na(stock_df$Close), ]

    stock_list[[sym]] <- stock_df
  }

  if (to_global) {
    for (sym in names(stock_list)) {
      assign(sym, stock_list[[sym]], envir = .GlobalEnv)
    }
  }

  message("Loaded ", length(stock_list), " of ", length(symbols), " symbols.")
  stock_list
}


# ---------------------------------------------------------------
# get_mode()
# Base R has no function for the statistical mode (mode() returns the
# storage type). Stock prices are continuous so almost every value is
# unique - I round the prices first (to whole dollars by default) and
# return the most frequent rounded value.
# ---------------------------------------------------------------
get_mode <- function(x, digits = 0) {
  x <- round(x[!is.na(x)], digits)
  if (length(x) == 0) return(NA)
  unique_vals <- unique(x)
  unique_vals[which.max(tabulate(match(x, unique_vals)))]
}


# ---------------------------------------------------------------
# calculate_statistics()
# Takes ONE stock data frame and calculates:
#   moving average (SMA), mean, mode, median and standard deviation
# Returns a list with:
#   $summary        - one row data frame with the statistics
#   $moving_average - data frame with Date, Close and the SMA column
# ---------------------------------------------------------------
calculate_statistics <- function(stock_df, column = "Close", ma_window = 20) {
  if (!column %in% names(stock_df)) {
    stop("Column '", column, "' not found in the data frame")
  }

  prices <- stock_df[[column]]

  if (length(prices) < ma_window) {
    stop("Not enough data (", length(prices), " rows) for a ",
         ma_window, "-day moving average")
  }

  # Simple moving average from the TTR package
  sma <- as.numeric(SMA(prices, n = ma_window))

  summary_stats <- data.frame(
    Start_Date  = min(stock_df$Date),
    End_Date    = max(stock_df$Date),
    Days        = length(prices),
    Mean        = mean(prices, na.rm = TRUE),
    Median      = median(prices, na.rm = TRUE),
    Mode        = get_mode(prices),
    Std_Dev     = sd(prices, na.rm = TRUE),
    Min         = min(prices, na.rm = TRUE),
    Max         = max(prices, na.rm = TRUE),
    Last_Close  = tail(prices, 1),
    Latest_SMA  = tail(na.omit(sma), 1)
  )
  names(summary_stats)[names(summary_stats) == "Latest_SMA"] <-
    paste0("SMA_", ma_window)

  moving_avg <- data.frame(Date = stock_df$Date,
                           Close = prices,
                           SMA = sma)

  list(summary = summary_stats, moving_average = moving_avg)
}


# ---------------------------------------------------------------
# calculate_all_statistics()
# Runs calculate_statistics() for every stock in the list.
# Returns a list (one element per stock) plus a combined table.
# ---------------------------------------------------------------
calculate_all_statistics <- function(stock_list, column = "Close", ma_window = 20) {
  stats_list <- lapply(stock_list, calculate_statistics,
                       column = column, ma_window = ma_window)

  stats_table <- do.call(rbind, lapply(names(stats_list), function(sym) {
    cbind(Symbol = sym, stats_list[[sym]]$summary)
  }))
  rownames(stats_table) <- NULL

  list(by_stock = stats_list, table = stats_table)
}
