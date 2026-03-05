## econ 5345 hw4 q1

rm(list = ls())
options(stringsAsFactors = FALSE)

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_flag <- "--file="
  m <- grep(file_flag, args)
  if (length(m) > 0) {
    return(dirname(normalizePath(sub(file_flag, "", args[m[1]]), winslash = "/")))
  }

  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    p <- rstudioapi::getActiveDocumentContext()$path
    if (!is.null(p) && nzchar(p)) return(dirname(normalizePath(p, winslash = "/")))
  }

  getwd()
}

script_dir <- get_script_dir()
setwd(script_dir)

needed_pkgs <- c("readr", "dplyr", "zoo", "ggplot2", "urca")
missing_pkgs <- needed_pkgs[!vapply(needed_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(
    "Missing packages: ", paste(missing_pkgs, collapse = ", "),
    "\nInstall them, e.g.: install.packages(c(", paste(sprintf('"%s"', missing_pkgs), collapse = ", "), "))"
  )
}

library(readr)
library(dplyr)
library(zoo)
library(ggplot2)
library(urca)

parse_fred_date <- function(x) {
  as.Date(x)
}

read_fred_csv <- function(path, value_col_name = NULL) {
  df <- readr::read_csv(path, show_col_types = FALSE)
  if (!("observation_date" %in% names(df))) stop("Expected column `observation_date` in ", path)

  if (is.null(value_col_name)) {
    value_cols <- setdiff(names(df), "observation_date")
    if (length(value_cols) != 1) {
      stop("Expected exactly 1 value column besides `observation_date` in ", path)
    }
    value_col_name <- value_cols[1]
  } else {
    if (!(value_col_name %in% names(df))) stop("Missing column `", value_col_name, "` in ", path)
  }

  df %>%
    transmute(
      date = parse_fred_date(.data$observation_date),
      value = as.numeric(.data[[value_col_name]])
    ) %>%
    arrange(.data$date) %>%
    filter(!is.na(.data$date), !is.na(.data$value))
}

make_monthly_zoo <- function(date, value) {
  zoo::zoo(value, zoo::as.yearmon(date))
}

make_quarterly_zoo <- function(date, value) {
  zoo::zoo(value, zoo::as.yearqtr(date))
}

plot_log_series <- function(z, title, ylab = "log(value)") {
  df <- data.frame(time = as.Date(index(z)), y = as.numeric(z))
  ggplot(df, aes(x = .data$time, y = .data$y)) +
    geom_line(linewidth = 0.4) +
    labs(title = title, x = NULL, y = ylab) +
    theme_minimal(base_size = 12)
}

run_unit_root_suite <- function(x, series_name) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) < 20) stop("Too few observations for tests: ", series_name)

  cat("\n============================================================\n")
  cat(series_name, "\n")
  cat("N =", length(x), "\n")
  cat("============================================================\n\n")

  # Phillips-Perron
  cat("\nPhillips-Perron (H0: unit root)\n")

  pp_const_tau <- urca::ur.pp(x, type = "Z-tau", model = "constant", lags = "short")
  pp_const_alpha <- urca::ur.pp(x, type = "Z-alpha", model = "constant", lags = "short")
  pp_trend_tau <- urca::ur.pp(x, type = "Z-tau", model = "trend", lags = "short")
  pp_trend_alpha <- urca::ur.pp(x, type = "Z-alpha", model = "trend", lags = "short")

  cat("\nPP (Z-tau) with constant:\n")
  print(summary(pp_const_tau))
  cat("\nPP (Z-alpha) with constant:\n")
  print(summary(pp_const_alpha))
  cat("\nPP (Z-tau) with trend:\n")
  print(summary(pp_trend_tau))
  cat("\nPP (Z-alpha) with trend:\n")
  print(summary(pp_trend_alpha))

  # Augmented Dickey-Fuller
  cat("ADF (H0: unit root)\n")
  adf_drift <- urca::ur.df(x, type = "drift", selectlags = "AIC")
  adf_trend <- urca::ur.df(x, type = "trend", selectlags = "AIC")
  cat("\nADF with drift (intercept):\n")
  print(summary(adf_drift))
  cat("\nADF with trend (intercept + trend):\n")
  print(summary(adf_trend))

  

  # KPSS 
  cat("\nKPSS (H0: stationarity)\n")
  kpss_mu <- urca::ur.kpss(x, type = "mu")
  kpss_tau <- urca::ur.kpss(x, type = "tau")
  cat("\nKPSS level-stationary (mu):\n")
  print(summary(kpss_mu))
  cat("\nKPSS trend-stationary (tau):\n")
  print(summary(kpss_tau))
}

# Load data


dollar_path <- "hw4_q1_dollar_index.csv"
gdp_path <- "hw4_q1_real_gdp.csv"

dollar_df <- read_fred_csv(dollar_path, value_col_name = "RTWEXBGS")
gdp_df <- read_fred_csv(gdp_path, value_col_name = "GDPC1")

# Construct time series (zoo) and take logs
dollar_m <- make_monthly_zoo(dollar_df$date, dollar_df$value)
gdp_q <- make_quarterly_zoo(gdp_df$date, gdp_df$value)

log_dollar_m <- log(dollar_m)
log_gdp_q <- log(gdp_q)

# Output (console + optional txt)

out_file <- "hw4_q1_test_output.txt"
sink(out_file)
on.exit(sink(), add = TRUE)

cat("HW4 Question 1 tests\n")
cat("Working directory:", normalizePath(getwd(), winslash = "/"), "\n")
cat("Dollar series file:", dollar_path, "\n")
cat("GDP series file:", gdp_path, "\n")
cat("\nNote: Dollar index provided is RTWEXBGS (Real Broad Dollar Index).\n")
cat("      TWEXBPA is discontinued at FRED.\n\n")

run_unit_root_suite(log_dollar_m, "log(RTWEXBGS) monthly")
run_unit_root_suite(log_gdp_q, "log(GDPC1) quarterly")

cat("\nSaved this output to:", out_file, "\n")
