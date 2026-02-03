# econ 5345 hw1 - q1e
#
# read clean data
# If you run from the repo root (e.g., `Rscript assignments/hw_q1e.R`),
# the CSVs won't be in the working directory. This tiny fallback fixes that.
if (!file.exists("hw1_q1e_month.csv")) {
  if (file.exists(file.path("assignments", "hw1_q1e_month.csv"))) {
    setwd("assignments")
  } else {
    stop("Cannot find hw1_q1e_month.csv. Run from assignments/ or repo root.")
  }
}

m <- read.csv("hw1_q1e_month.csv", stringsAsFactors = FALSE)
q <- read.csv("hw1_q1e_quarter.csv", stringsAsFactors = FALSE)

m$observation_date <- as.Date(m$observation_date)
q$observation_date <- as.Date(q$observation_date)
m$PCENDC96 <- as.numeric(m$PCENDC96)
q$PCENDC96 <- as.numeric(q$PCENDC96)

m <- m[order(m$observation_date), ]
q <- q[order(q$observation_date), ]
q <- q[!is.na(q$PCENDC96), ]

m <- m[m$observation_date <= as.Date("2025-09-30"), ]

dx_m <- diff(log(m$PCENDC96))
dx_q <- diff(log(q$PCENDC96))

# save plot
png("hw_q1e_month_acf_pacf.png", width = 1200, height = 500, res = 150)
par(mfrow = c(1, 2))
acf(dx_m, lag.max = 40, main = "Monthly: ACF (log-diff)", na.action = na.omit)
pacf(dx_m, lag.max = 40, main = "Monthly: PACF (log-diff)", na.action = na.omit)
dev.off()

png("hw_q1e_quarter_acf_pacf.png", width = 1200, height = 500, res = 150)
par(mfrow = c(1, 2))
acf(dx_q, lag.max = 40, main = "Quarterly: ACF (log-diff)", na.action = na.omit)
pacf(dx_q, lag.max = 40, main = "Quarterly: PACF (log-diff)", na.action = na.omit)
dev.off()

cat("Saved:\n")
cat(" - hw_q1e_month_acf_pacf.png\n")
cat(" - hw_q1e_quarter_acf_pacf.png\n")
