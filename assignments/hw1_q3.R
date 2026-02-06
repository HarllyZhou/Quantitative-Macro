# econ 5345 hw1 - q3(c)(d)
rm(list = ls())
graphics.off()

# settings
rho_true <- 0.9
sigma <- 1
seed <- 123L

set.seed(seed)

simulate_panel <- function(N, T, rho, sigma) {
  y <- matrix(NA_real_, nrow = N, ncol = T + 1L)
  y[, 1L] <- rnorm(N, mean = 0, sd = sigma / sqrt(1 - rho^2))
  for (t in 1L:T) {
    e <- rnorm(N, mean = 0, sd = sigma)
    y[, t + 1L] <- rho * y[, t] + e
  }
  y
}

rho_hat_fe <- function(y) {
  N <- nrow(y)
  TT <- ncol(y) - 1L

  X <- y[, 1L:TT, drop = FALSE]
  Y <- y[, 2L:(TT + 1L), drop = FALSE]

  Xdm <- X - rowMeans(X)
  Ydm <- Y - rowMeans(Y)

  num <- sum(Xdm * Ydm)
  den <- sum(Xdm * Xdm)
  num / den
}

mc_run <- function(N, T, reps, rho, sigma) {
  hats <- numeric(reps)
  for (r in 1L:reps) {
    y <- simulate_panel(N = N, T = T, rho = rho, sigma = sigma)
    hats[[r]] <- rho_hat_fe(y)
  }
  hats
}

one_run <- function(N, T, rho, sigma) {
  y <- simulate_panel(N = N, T = T, rho = rho, sigma = sigma)
  rho_hat_fe(y)
}

latex_table <- function(df, caption, label) {
  cat("\\begin{table}[htbp]\n")
  cat("  \\centering\n")
  cat("  \\caption{", caption, "}\n", sep = "")
  cat("  \\label{", label, "}\n", sep = "")
  cat("  \\begin{tabular}{rrr}\n")
  cat("    \\hline\n")
  cat("    $N$ & $T$ & $\\hat{\\rho}$ \\\\\n")
  cat("    \\hline\n")
  for (i in seq_len(nrow(df))) {
    cat("    ", df$N[i], " & ", df$T[i], " & ", sprintf("%.6f", df$rho_hat[i]), " \\\\\n", sep = "")
  }
  cat("    \\hline\n")
  cat("  \\end{tabular}\n")
  cat("\\end{table}\n\n")
}

# part (c)

Ns_c <- c(100L, 500L, 1000L)
out_c <- data.frame(
  N = Ns_c,
  T = 2L,
  rho_hat = vapply(Ns_c, function(N) one_run(N, 2L, rho_true, sigma), numeric(1))
)

# part (d)

Ts_d <- c(5L, 20L)
out_d <- data.frame(
  N = 1000L,
  T = Ts_d,
  rho_hat = vapply(Ts_d, function(T) one_run(1000L, T, rho_true, sigma), numeric(1))
)

# print LaTeX tables (copy/paste into hw1_report.tex)
latex_table(
  out_c,
  caption = sprintf("OLS estimates"),
  label = "tab:q3c"
)

latex_table(
  out_d,
  caption = sprintf("OLS estimates"),
  label = "tab:q3d"
)

