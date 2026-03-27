rm(list = ls())
options(stringsAsFactors = FALSE)

# =========================================================
# Packages
# =========================================================
needed_pkgs <- c(
  "readr", "readxl", "dplyr", "zoo", "sandwich", "ggplot2"
)

missing_pkgs <- needed_pkgs[!vapply(needed_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  install.packages(missing_pkgs)
}

library(readr)
library(readxl)
library(dplyr)
library(zoo)
library(sandwich)
library(ggplot2)

# =========================================================
# Script directory
# =========================================================
get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  idx <- grep(file_arg, args)
  if (length(idx) > 0) {
    return(dirname(normalizePath(sub(file_arg, "", args[idx[1]]), winslash = "/")))
  }
  # When run via source(), R puts the sourced path in ofile (no --file= with Rscript -e, etc.)
  for (i in seq_len(sys.nframe())) {
    f <- sys.frame(i)
    if (exists("ofile", envir = f, inherits = FALSE)) {
      of <- f[["ofile"]]
      if (is.character(of) && length(of) == 1 && nzchar(of)) {
        return(dirname(normalizePath(of, winslash = "/")))
      }
    }
  }
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- rstudioapi::getActiveDocumentContext()$path
    if (nzchar(p)) return(dirname(normalizePath(p)))
  }
  getwd()
}

script_dir <- get_script_dir()
setwd(script_dir)

# =========================================================
# File names
# =========================================================
shock_file <- "rr_mshock.txt"
tfp_file   <- "tfp.xlsx"

# =========================================================
# Output files
# =========================================================
output_txt_ab <- "hw7_q1_ab_output.txt"
output_txt_c  <- "hw7_q1_c_output.txt"
output_png_ab <- "hw7_q1_ab_gdp_irf.png"
output_png_c  <- "hw7_q1_c_tfp_irf.png"

# =========================================================
# Parameters
# =========================================================
H <- 20
M <- 4
Q <- 4
z90 <- qnorm(0.95)   # 90% CI: point +/- 1.645*SE

# =========================================================
# Helper functions
# =========================================================
parse_yq <- function(x) {
  x <- tolower(trimws(x))
  x <- gsub(":", "", x)
  x <- gsub("q", " Q", x)
  x <- gsub("\\s+", " ", x)
  as.yearqtr(x, format = "%Y Q%q")
}

lead_vec <- function(x, h) {
  if (h == 0) return(x)
  c(x[(1 + h):length(x)], rep(NA, h))
}

lag_vec <- function(x, k) {
  if (k == 0) return(x)
  c(rep(NA, k), x[1:(length(x) - k)])
}

newey_west_se <- function(model, lag_length) {
  vcov_mat <- sandwich::NeweyWest(model, lag = lag_length, prewhite = FALSE, adjust = TRUE)
  sqrt(diag(vcov_mat))
}

run_lp <- function(df, y_var, shock_var, H = 20, M = 4, Q = 4, minimum_delay = FALSE) {
  y <- df[[y_var]]
  x <- df[[shock_var]]
  dy <- c(NA, diff(y))

  shock_sd <- sd(x, na.rm = TRUE)

  h_grid <- if (minimum_delay) 1:H else 0:H

  res_list <- vector("list", length(h_grid))

  for (i in seq_along(h_grid)) {
    h <- h_grid[i]

    lhs <- lead_vec(y, h) - lag_vec(y, 1)

    reg_df <- data.frame(lhs = lhs)

    for (m in 0:M) {
      reg_df[[paste0("x_lag", m)]] <- lag_vec(x, m)
    }

    if (minimum_delay) {
      for (q in 0:Q) {
        reg_df[[paste0("dy_lag", q)]] <- lag_vec(dy, q)
      }
      rhs_terms <- c(paste0("x_lag", 0:M), paste0("dy_lag", 0:Q))
    } else {
      for (q in 1:Q) {
        reg_df[[paste0("dy_lag", q)]] <- lag_vec(dy, q)
      }
      rhs_terms <- c(paste0("x_lag", 0:M), paste0("dy_lag", 1:Q))
    }

    reg_df <- reg_df[complete.cases(reg_df), ]

    formula_str <- paste("lhs ~", paste(rhs_terms, collapse = " + "))
    fit <- lm(as.formula(formula_str), data = reg_df)

    se_vec <- newey_west_se(fit, lag_length = h)

    beta0 <- coef(fit)["x_lag0"]
    se0   <- se_vec["x_lag0"]

    irf   <- beta0 * shock_sd
    irf_se <- se0 * shock_sd

    res_list[[i]] <- data.frame(
      h = h,
      beta_x0 = beta0,
      se_x0 = se0,
      shock_sd = shock_sd,
      irf = irf,
      lower = irf - z90 * irf_se,
      upper = irf + z90 * irf_se,
      n = nobs(fit)
    )
  }

  do.call(rbind, res_list)
}

plot_irf <- function(df, file_name, title_text, ylab_text) {
  p <- ggplot(df, aes(x = h, y = irf, color = spec, fill = spec)) +
    geom_hline(yintercept = 0, color = "gray50", linewidth = 0.4) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.18, color = NA) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.4) +
    scale_x_continuous(breaks = 0:H) +
    labs(
      title = title_text,
      x = "Horizon",
      y = ylab_text,
      color = NULL,
      fill = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")

  ggsave(file_name, plot = p, width = 9, height = 5.8, dpi = 180)
}

# =========================================================
# Read monetary shock data
# =========================================================
shock_df <- read_csv(shock_file, show_col_types = FALSE)

shock_df <- shock_df %>%
  transmute(
    tq = parse_yq(date),
    resid_romer = as.numeric(resid_romer),
    resid_full = as.numeric(resid_full)
  ) %>%
  arrange(tq)

# =========================================================
# Download quarterly real GDP from FRED
# GDPC1 = Real Gross Domestic Product
# =========================================================
gdp_url <- "https://fred.stlouisfed.org/graph/fredgraph.csv?id=GDPC1"
temp_gdp <- tempfile(fileext = ".csv")
download.file(gdp_url, destfile = temp_gdp, mode = "wb", quiet = TRUE)

gdp_raw <- read_csv(temp_gdp, show_col_types = FALSE)
nm <- names(gdp_raw)
nl <- tolower(nm)
date_nm <- nm[which(nl %in% c("date", "observation_date"))][1]
if (is.na(date_nm)) {
  date_nm <- nm[grep("date", nl)][1]
}
value_nm <- nm[nl == "gdpc1"][1]
if (is.na(value_nm)) {
  value_nm <- setdiff(nm, date_nm)[1]
}

gdp_df <- gdp_raw %>%
  transmute(
    tq = as.yearqtr(as.Date(.data[[date_nm]])),
    gdpc1 = as.numeric(.data[[value_nm]])
  ) %>%
  arrange(tq)

# =========================================================
# Merge GDP with shocks, sample through 2007Q4
# =========================================================
gdp_data <- shock_df %>%
  inner_join(gdp_df, by = "tq") %>%
  filter(tq <= as.yearqtr("2007 Q4")) %>%
  mutate(
    log_gdp = log(gdpc1)
  ) %>%
  arrange(tq)

# =========================================================
# Part (a): baseline LP for GDP
# =========================================================
res_a <- run_lp(
  df = gdp_data,
  y_var = "log_gdp",
  shock_var = "resid_romer",
  H = H, M = M, Q = Q,
  minimum_delay = FALSE
)
res_a$spec <- "Part (a): baseline"

# =========================================================
# Part (b): minimum delay assumption
# =========================================================
res_b <- run_lp(
  df = gdp_data,
  y_var = "log_gdp",
  shock_var = "resid_romer",
  H = H, M = M, Q = Q,
  minimum_delay = TRUE
)
res_b$spec <- "Part (b): minimum delay"

# For plotting together, add h=0 = 0 for part (b)
res_b_plot <- rbind(
  data.frame(
    h = 0,
    beta_x0 = NA,
    se_x0 = NA,
    shock_sd = unique(res_b$shock_sd),
    irf = 0,
    lower = 0,
    upper = 0,
    n = NA,
    spec = "Part (b): minimum delay"
  ),
  res_b
)

plot_ab_df <- rbind(res_a, res_b_plot)

plot_irf(
  plot_ab_df,
  output_png_ab,
  "HW7 Q1(a)-(b): Real GDP response to a 1-s.d. contractionary monetary shock",
  "Response of log real GDP"
)

# =========================================================
# Write part (a) and (b) results
# =========================================================
sink(output_txt_ab)

cat("HW7 Question 1(a) and 1(b)\n")
cat("==========================\n\n")

cat("Monetary shock series used: resid_romer\n")
cat("Sample ends at 2007Q4.\n")
cat("Real GDP series: GDPC1 from FRED.\n")
cat("Horizons: h = 0,...,20 for part (a); h = 1,...,20 for part (b).\n")
cat("M = 4, Q = 4.\n")
cat("90% confidence intervals use Newey-West standard errors.\n\n")

cat("Sample range for GDP regressions:\n")
cat("Start:", as.character(min(gdp_data$tq)), "\n")
cat("End:  ", as.character(max(gdp_data$tq)), "\n")
cat("N:    ", nrow(gdp_data), "\n\n")

cat("Part (a): baseline LP\n")
print(res_a, row.names = FALSE)

cat("\nPart (b): minimum delay LP\n")
print(res_b, row.names = FALSE)

sink()

# =========================================================
# Read TFP Excel file
# =========================================================
sheets <- excel_sheets(tfp_file)

# Try to locate a quarterly sheet
sheet_use <- sheets[grep("quarter", tolower(sheets))]
if (length(sheet_use) == 0) {
  sheet_use <- sheets[1]
} else {
  sheet_use <- sheet_use[1]
}

# Fernald file: "quarterly" has a one-row note above the header; "annual" does not.
if (grepl("quarterly", tolower(sheet_use))) {
  tfp_raw <- read_excel(tfp_file, sheet = sheet_use, skip = 1L)
} else {
  tfp_raw <- read_excel(tfp_file, sheet = sheet_use)
  if (!any(grepl("^date$", tolower(names(tfp_raw))))) {
    tfp_raw <- read_excel(tfp_file, sheet = sheet_use, skip = 1L)
  }
}

names_lower <- tolower(names(tfp_raw))

# Try common column names
date_col <- names(tfp_raw)[grep("^date$", names_lower)][1]
if (is.na(date_col)) {
  date_col <- names(tfp_raw)[grep("date", names_lower)][1]
}

dtfp_col <- names(tfp_raw)[grep("^dtfp$", names_lower)][1]
if (is.na(dtfp_col)) {
  dtfp_col <- names(tfp_raw)[grep("dtfp", names_lower)][1]
}

dtfp_util_col <- names(tfp_raw)[grep("dtfp.*util|util.*dtfp|dtfp_util", names_lower)][1]

if (is.na(date_col) || is.na(dtfp_col) || is.na(dtfp_util_col)) {
  stop("Could not identify the needed columns in tfp.xlsx. Check the column names.")
}

tfp_df <- tfp_raw %>%
  transmute(
    tq = parse_yq(.data[[date_col]]),
    dlog_tfp = as.numeric(.data[[dtfp_col]]) / 400,
    dlog_tfp_util = as.numeric(.data[[dtfp_util_col]]) / 400
  ) %>%
  arrange(tq) %>%
  mutate(
    log_tfp = cumsum(ifelse(is.na(dlog_tfp), 0, dlog_tfp)),
    log_tfp_util = cumsum(ifelse(is.na(dlog_tfp_util), 0, dlog_tfp_util))
  )

# =========================================================
# Merge TFP with shocks, sample through 2007Q4
# =========================================================
tfp_data <- shock_df %>%
  inner_join(tfp_df, by = "tq") %>%
  filter(tq <= as.yearqtr("2007 Q4")) %>%
  arrange(tq)

# =========================================================
# Part (c): measured TFP
# =========================================================
res_c_tfp <- run_lp(
  df = tfp_data,
  y_var = "log_tfp",
  shock_var = "resid_romer",
  H = H, M = M, Q = Q,
  minimum_delay = FALSE
)
res_c_tfp$spec <- "Measured TFP"

# =========================================================
# Part (c): utilization-adjusted TFP
# =========================================================
res_c_tfp_util <- run_lp(
  df = tfp_data,
  y_var = "log_tfp_util",
  shock_var = "resid_romer",
  H = H, M = M, Q = Q,
  minimum_delay = FALSE
)
res_c_tfp_util$spec <- "Utilization-adjusted TFP"

plot_c_df <- rbind(res_c_tfp, res_c_tfp_util)

plot_irf(
  plot_c_df,
  output_png_c,
  "HW7 Q1(c): TFP response to a 1-s.d. contractionary monetary shock",
  "Response of log TFP"
)

# =========================================================
# Write part (c) results
# =========================================================
sink(output_txt_c)

cat("HW7 Question 1(c)\n")
cat("=================\n\n")

cat("Monetary shock series used: resid_romer\n")
cat("Sample ends at 2007Q4.\n")
cat("Fernald data are interpreted as annualized quarterly growth rates:\n")
cat("dlog(TFP) = dtfp / 400\n")
cat("dlog(utilization-adjusted TFP) = dtfp_util / 400\n")
cat("Then they are cumulated into log levels.\n\n")

cat("Sample range for TFP regressions:\n")
cat("Start:", as.character(min(tfp_data$tq)), "\n")
cat("End:  ", as.character(max(tfp_data$tq)), "\n")
cat("N:    ", nrow(tfp_data), "\n\n")

cat("Measured TFP\n")
print(res_c_tfp, row.names = FALSE)

cat("\nUtilization-adjusted TFP\n")
print(res_c_tfp_util, row.names = FALSE)

sink()

# =========================================================
# Console summary
# =========================================================
cat("Done.\n")
cat("Created files:\n")
cat(output_txt_ab, "\n")
cat(output_txt_c, "\n")
cat(output_png_ab, "\n")
cat(output_png_c, "\n")