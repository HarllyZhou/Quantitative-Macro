# econ 5345 hw1 — q1(e)

# setup
script_arg <- commandArgs()
script_path <- sub("^--file=", "", script_arg[grep("^--file=", script_arg)])
script_dir <- if (length(script_path) == 1L) dirname(normalizePath(script_path)) else getwd()

# functions
read_data <- function(path) {
  d <- read.csv(path, stringsAsFactors = FALSE)
  if (!all(c("observation_date", "PCENDC96") %in% names(d))) {
    stop("Unexpected columns in ", basename(path), ". Expected: observation_date, PCENDC96")
  }
  d$observation_date <- as.Date(d$observation_date)
  d$PCENDC96 <- as.numeric(d$PCENDC96)
  d <- d[order(d$observation_date), ]
  d <- d[!is.na(d$observation_date) & !is.na(d$PCENDC96), ]
  d
}

logdiff <- function(x) diff(log(x))

plot_acf_pacf <- function(dx, out_png, title_prefix, max_lag = 40L) {
  dx <- dx[is.finite(dx)]
  if (length(dx) < 3) stop("Not enough observations after cleaning for: ", title_prefix)

  acf_obj <- acf(dx, lag.max = max_lag, plot = FALSE, na.action = na.omit)
  pacf_obj <- pacf(dx, lag.max = max_lag, plot = FALSE, na.action = na.omit)
  invisible(list(acf = acf_obj, pacf = pacf_obj))

  png(filename = out_png, width = 1200, height = 500, res = 150)
  op <- par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
  acf(dx, lag.max = max_lag, main = paste0(title_prefix, ": ACF (log-diff)"), na.action = na.omit)
  pacf(dx, lag.max = max_lag, main = paste0(title_prefix, ": PACF (log-diff)"), na.action = na.omit)
  par(op)
  dev.off()
}

# read data
m_path <- file.path(script_dir, "hw1_q1e_month.csv")
q_path <- file.path(script_dir, "hw1_q1e_quarter.csv")
m <- read_data(m_path)
q <- read_data(q_path)

end_month <- as.Date("2025-09-30")

m <- m[m$observation_date <= end_month, ]

dx_m <- logdiff(m$PCENDC96)
dx_q <- logdiff(q$PCENDC96)

# save plots
out_m <- file.path(script_dir, "hw1_q1e_month_acf_pacf.png")
out_q <- file.path(script_dir, "hw1_q1e_quarter_acf_pacf.png")

max_lag <- 40L

plot_acf_pacf(dx_m, out_m, "Monthly", max_lag = max_lag)
plot_acf_pacf(dx_q, out_q, "Quarterly", max_lag = max_lag)

cat("Saved:\n")
cat(" - ", out_m, "\n", sep = "")
cat(" - ", out_q, "\n", sep = "")

