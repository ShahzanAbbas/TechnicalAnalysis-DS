# ------------------------------------------------------------------
# BDA400 - Assignment 2
# File: install_packages.R
# Installs the packages needed for the technical analysis project.
# Only installs the ones that are not already installed.
# ------------------------------------------------------------------

packages <- c("quantmod",   # download stock data + charting
              "TTR",        # Technical Trading Rules (SMA, EMA, RSI ...)
              "xts",        # time series objects (used by quantmod)
              "zoo",        # time series helper functions
              "knitr",      # nice tables
              "rmarkdown")  # to knit the .Rmd report

new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]

if (length(new_packages) > 0) {
  install.packages(new_packages, dependencies = TRUE)
} else {
  message("All packages are already installed.")
}

# check that every package loads
for (p in packages) {
  library(p, character.only = TRUE)
  cat(p, "version", as.character(packageVersion(p)), "loaded OK\n")
}
