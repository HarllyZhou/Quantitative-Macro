# econ5345 hw2 - q2

rm(list = ls())
graphics.off()

# part (a) -------------------------------------------------------------
T25 <- 25
psi <- rep(0, T25+1)
psi[1] <- 1

for (idx in 1:T25) {
  psi[idx+1] <- 1.2 * 0.8^(idx - 1)
}

psi_25 <- data.frame(
  t = 0:T25,
  psi = psi
)

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
plot(psi_25$t, psi_25$psi, type = "o", pch = 16,
     xlab = "t", ylab = "Impulse response",
     main = "IRF to unit shock in e_0")
abline(h = 0, lty = 3)
dev.off()

cat("Saved irf plot to:", out_png, "\n")

########################################################
# part (d) -------------------------------------------------------------

set.seed(123)
T <- 200
burning <- 100

k <- 2
theta <- sqrt( 3 / k )

e <- rgamma(T + 1, shape = k, scale = theta) - k * theta


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

y <- simulate_y(e)

y_burning <- numeric(T-burning)
for (t in 1:T-burning) {
  y_burning[t] <- y[t+burning]
}


y_plot <- data.frame(
  t = 0:T,
  y = y
)

# part (a)
get_script_dir <- function() {
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    path <- rstudioapi::getActiveDocumentContext()$path
    if (nzchar(path)) return(dirname(path))
  }
  getwd()
}

script_dir <- get_script_dir()
out_png <- file.path(script_dir, "hw2_q2d_y.png")

png(out_png, width = 900, height = 700, res = 130)
plot(y_plot$t, y_plot$y, type = "o", pch = 16,
     xlab = "t", ylab = "y",
     main = "Simulated path of y")
abline(h = 0, lty = 3)
dev.off()

cat("Saved irf plot to:", out_png, "\n")
