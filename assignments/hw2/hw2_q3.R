# econ 5345 hw2 — q3

# setup
rm(list = ls())
graphics.off()
script_arg <- commandArgs()
script_path <- sub("^--file=", "", script_arg[grep("^--file=", script_arg)])
script_dir <- if (length(script_path) == 1L) dirname(normalizePath(script_path)) else getwd()

# read data
path <- file.path(script_dir, "hw2_q3.csv")
data <- read.csv(path, stringsAsFactors = FALSE)


# output file
out_png <- file.path(script_dir, "hw2_q3a.png")
png(out_png, width = 1000, height = 800, res = 140)

par(mfrow = c(2, 2), mar = c(4, 4, 3, 1) + 0.1)

# Inflation
acf(data$inflation, main = "ACF: Inflation")
pacf(data$inflation, main = "PACF: Inflation")

# GDP growth
acf(data$gdp_growth, main = "ACF: GDP Growth")
pacf(data$gdp_growth, main = "PACF: GDP Growth")

dev.off()

cat("Saved plot to:", out_png, "\n")
