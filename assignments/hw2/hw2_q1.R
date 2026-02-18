# econ5345 hw2 - q1
rm(list = ls())
graphics.off()

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) == 1L) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg))))
  }
  # Fallback: try rstudioapi, otherwise current working directory
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    p <- rstudioapi::getActiveDocumentContext()$path
    if (!is.null(p) && nzchar(p)) return(dirname(normalizePath(p)))
  }
  getwd()
}
script_dir <- get_script_dir()

## --- Parameters ---
sigma_z <- 0.63
sigma_g <- 0.47
rho_z   <- 0.97
rho_g   <- 0.29
alpha   <- 0.68

H <- 20
h_grid <- 0:H

## variance from z
fev_z <- sigma_z^2 * (1 - rho_z^(2 * (h_grid + 1))) / (1 - rho_z^2)

## variance from g
fev_x <- numeric(H + 1)
for (h in 0:H) {
  s <- 0:h
  w <- (1 - rho_g^(h - s + 1)) / (1 - rho_g)
  fev_x[h + 1] <- sigma_g^2 * sum(w^2)
}
fev_g <- (alpha^2) * fev_x

## total
fev_total <- fev_z + fev_g
share_transitory <- fev_z / fev_total

out_fev <- file.path(script_dir, "hw2_q1b.png")
png(out_fev, width = 950, height = 700, res = 140)

plot(h_grid, fev_total, type = "o", pch = 16,
     xlab = "h (quarters)",
     ylab = "Forecast error variance",
     main = "Forecast error variance")
grid()

dev.off()

## decomposition figure

out_decomp <- file.path(script_dir, "hw2_q1c.png")
png(out_decomp, width = 1000, height = 740, res = 140)

## left axis: contributions
ymax <- max(fev_total) * 1.05
plot(h_grid, fev_total, type = "o", pch = 16,
     ylim = c(0, ymax),
     xlab = "h (quarters)",
     ylab = "Mean-squared error contribution",
     main = "Decomposition of FEV")
grid()
lines(h_grid, fev_z, type = "o", pch = 1)
lines(h_grid, fev_g, type = "o", pch = 2)

legend("topleft",
       legend = c("Total FEV", "Transitory (z) part", "Permanent (g) part"),
       lty = 1, pch = c(16, 1, 2), bty = "n")

## right axis: share of transitory shocks
par(new = TRUE)
plot(h_grid, share_transitory, type = "o", pch = 4,
     axes = FALSE, xlab = "", ylab = "",
     ylim = c(0, 1))
axis(4, at = seq(0, 1, by = 0.2))
mtext("Transitory share (z / total)", side = 4, line = 3)

legend("topright",
       legend = c("Transitory share"),
       lty = 1, pch = 4, bty = "n")

dev.off()

cat("Saved plots to:\n",
    out_fev, "\n",
    out_decomp, "\n")
