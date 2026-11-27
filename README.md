# TechnicalAnalysis

Repository for my BDA400 (Data Science Tools and Techniques) assignments.
The project is a 3-stage stock technical analysis project written in R.

| Folder | Contents |
|---|---|
| `Assignment2/` | Technical Analysis using R – Preliminary Stage |
| `Assignment3/` | (next stage, coming soon) |

## Assignment 2 files

- `portfolio.txt` – stock symbols in my portfolio, one per line
- `install_packages.R` – installs quantmod, TTR and other packages
- `stock_functions.R` – `load_stock_data()`, `calculate_statistics()` and helpers
- `display_utils.R` – functions to display the data, statistics and charts
- `main.R` – runs everything
- `Assignment2_Analysis.Rmd` – R Markdown version with output and charts
- `DevinderSandhu_BDA400_A02.docx` – report with screenshots

## How to run

1. Open RStudio and run `install_packages.R` once.
2. Set the working directory to the `Assignment2` folder
   (Session > Set Working Directory > To Source File Location).
3. Run `main.R`, or open `Assignment2_Analysis.Rmd` and click **Knit**.
