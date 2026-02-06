# econ5345 hw2 - q2

rm(list = ls())

set.seed(123)
Tmax <- 25
sigma2 <- 3

# generate e and shocks
e_base <- rnorm(Tmax + 1, mean = 0, sd = sqrt(sigma2))

e_shock <- e_base
e_shock[1] <- e_shock[1] + 1

# simulation function
simulate_y <- function(e) {
  y <- numeric(length(e))
  y_lag <- 0
  e_lag <- 0
  
  for (t in seq_along(e)) {
    y[t] <- 0.8 * y_lag + e[t] + 0.4 * e_lag
    y_lag <- y[t]
    e_lag <- e[t]
  }
  return(y)
}

# simulate paths
y_base  <- simulate_y(e_base)
y_shock <- simulate_y(e_shock)

# impulse response
irf <- y_shock - y_base

irf_df <- data.frame(
  t = 0:Tmax,
  irf = irf
)

########################################################
# part (a)

latex_table <- knitr::kable(
  irf_df,
  format  = "latex",
  caption = "Impulse response",
  label   = "tab:q2a",
  booktabs = TRUE,
  digits = 4
)
cat(latex_table, sep = "\n")

# plot
get_script_dir <- function() {
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    path <- rstudioapi::getActiveDocumentContext()$path
    if (nzchar(path)) return(dirname(path))
  }
  getwd()
}

script_dir <- get_script_dir()
out_png <- file.path(script_dir, "hw2_q2a_irf.png")

png(out_png, width = 900, height = 700, res = 130)
plot(irf_df$t, irf_df$irf, type = "o", pch = 16,
     xlab = "t", ylab = "Impulse response",
     main = "IRF to unit shock in e_0")
abline(h = 0, lty = 3)
dev.off()

cat("Saved irf plot to:", out_png, "\n")

########################################################
# part (b)
mean <- mean(irf_df$irf)
var <- var(irf_df$irf)
cov <- cov(irf_df$irf, irf_df$irf[2:length(irf_df$irf)])
print(mean)
print(var)
print(cov)
