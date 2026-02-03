# econ 5345 hw1 — q1d

# settings
seed <- 123L
max_lag <- 40L
set.seed(seed)

# simulation process
n_quarters <- 10000L
n_months <- 3L * n_quarters

e <- rnorm(n_months, mean = 0, sd = 1)
C <- 0 + cumsum(e)

# compute measured consumption and change
C_mat <- matrix(C, nrow = 3L, ncol = n_quarters)
C_measured <- colMeans(C_mat)

x <- diff(C_measured)
n <- length(x)

if (max_lag >= n) stop("max_lag must be < length(diff(measured_series)).")

# find acf and pacf
acf_obj <- acf(x, lag.max = max_lag, plot = FALSE)
pacf_obj <- pacf(x, lag.max = max_lag, plot = FALSE)

# saving plots
script_arg <- commandArgs()
script_path <- sub("^--file=", "", script_arg[grep("^--file=", script_arg)])
script_dir <- if (length(script_path) == 1L) dirname(normalizePath(script_path)) else getwd()
out_png <- file.path(script_dir, "hongyi_zhou_hw1_q1d_acf_pacf.png")

png(filename = out_png, width = 1200, height = 500, res = 150)
op <- par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

acf(x, lag.max = max_lag, main = "ACF of diff(measured consumption)")
pacf(x, lag.max = max_lag, main = "PACF of diff(measured consumption)")

par(op)
dev.off()

cat("Saved ACF/PACF plot to:", out_png, "\n")

