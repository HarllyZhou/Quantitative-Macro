# econ5345 hw3 - q3
rm(list = ls())
graphics.off()

get_script_dir <- function() {
  cmd <- commandArgs(trailingOnly = FALSE)
  m <- grep("^--file=", cmd)
  if (length(m) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", cmd[m[1]]))))
  }
  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    p <- tryCatch(rstudioapi::getActiveDocumentContext()$path, error = function(e) "")
    if (nzchar(p)) return(dirname(normalizePath(p)))
  }
  return(getwd())
}
script_dir <- get_script_dir()

## package for filters
if (!requireNamespace("mFilter", quietly = TRUE)) {
  install.packages("mFilter")
}
library(mFilter)

if (!requireNamespace("xtable", quietly = TRUE)) {
  install.packages("xtable")
}
library(xtable)


set.seed(123)

# ----------------- HP filter -----------------
# DGP
T <- 120
R <- 2000
lambda <- 1600

e_mean <- 0
e_sd <- 1
u_min <- -sqrt(3)
u_max <- sqrt(3)


E <- matrix(rnorm(T * R, mean = e_mean, sd = e_sd), nrow = T, ncol = R)
U <- matrix(runif(T * R, min = u_min, max = u_max), nrow = T, ncol = R)

X <- apply(E, 2, cumsum)
Y <- apply(U, 2, cumsum)

# generate correlations
cors <- numeric(R)

for (r in 1:R) {
  x_cyc <- as.numeric(hpfilter(X[, r], freq = lambda, type = "lambda")$cycle)
  y_cyc <- as.numeric(hpfilter(Y[, r], freq = lambda, type = "lambda")$cycle)
  cors[r] <- cor(x_cyc, y_cyc)
}

# plot
out_png <- file.path(script_dir, "hw3_q3_hp.png")
png(out_png, width = 1000, height = 800, res = 140)
hist(cors, breaks = 40,
     xlab = "Correlation corr(HP-cycle(x), HP-cycle(y))",
     main = sprintf("Histogram of HP-filtered correlations (T=%d, reps=%d, lambda=%g)", T, R, lambda))
abline(v = mean(cors), lty = 2)
dev.off()

# data summary
cors_mean <- mean(cors)
cors_sd   <- sd(cors)

p_gt_1sd <- mean(cors > (cors_mean + 1 * cors_sd))
p_gt_2sd <- mean(cors > (cors_mean + 2 * cors_sd))
p_gt_3sd <- mean(cors > (cors_mean + 3 * cors_sd))

cors_sum <- summary(cors)

tab <- data.frame(
  Statistic = c("Min", "1st Qu.", "Median", "Mean ($\\mu^{HP}$)", "3rd Qu.", "Max", "SD ($\\sigma^{HP}$)", "$\\mathbb{P}(\\rho_{xy} > \\mu^{HP} + 1 \\sigma^{HP})$", "$\\mathbb{P}(\\rho_{xy} > \\mu^{HP} + 2 \\sigma^{HP})$", "$\\mathbb{P}(\\rho_{xy} > \\mu^{HP} + 3 \\sigma^{HP})$"),
  Value = c(as.numeric(cors_sum["Min."]),
            as.numeric(cors_sum["1st Qu."]),
            as.numeric(cors_sum["Median"]),
            as.numeric(cors_sum["Mean"]),
            as.numeric(cors_sum["3rd Qu."]),
            as.numeric(cors_sum["Max."]),
            cors_sd,
            p_gt_1sd, p_gt_2sd, p_gt_3sd)
)

tab$Value <- formatC(tab$Value, format = "f", digits = 4)

latex_code <- print(
  xtable(tab,
         caption = sprintf("Summary statistics and upper-tail shares of simulated correlations.", T, length(cors)),
         label = "tab:q3_hp"),
  include.rownames = FALSE,
  sanitize.text.function = identity,
  print.results = FALSE
)

cat(latex_code, "\n")

cat("Saved histogram to:\n", out_png, "\n", sep = "")
cat("Summary of correlations:\n")
print(summary(cors))
cat("Mean:", mean(cors), "  SD:", sd(cors), "\n")



# ----------------- BK filter -----------------
K <- 12
pl <- 6
pu <- 32       ## as standard quarterly frequency

cors_bk <- numeric(R)

idx <- (K + 1):(T - K)

for (r in 1:R) {
  x_cyc <- as.numeric(bkfilter(X[, r], pl = pl, pu = pu, nfix = K)$cycle)
  y_cyc <- as.numeric(bkfilter(Y[, r], pl = pl, pu = pu, nfix = K)$cycle)
  cors_bk[r] <- cor(x_cyc[idx], y_cyc[idx])
}

# histogram
out_png_bk <- file.path(script_dir, "hw3_q3_bk.png")
png(out_png_bk, width = 1000, height = 800, res = 140)
hist(cors_bk, breaks = 40,
     xlab = "Correlation corr(BK-cycle(x), BK-cycle(y))",
     main = sprintf("Histogram of BK-filtered correlations (T=%d, reps=%d, K=%d, [%d,%d])",
                    T, R, K, pl, pu))
abline(v = mean(cors_bk), lty = 2)
dev.off()

cat("Saved BK histogram to:\n", out_png_bk, "\n", sep = "")

# -------------------------------
# Summary + tail probabilities + LaTeX table
# -------------------------------
cors_mean <- mean(cors_bk)
cors_sd   <- sd(cors_bk)

p_gt_1sd <- mean(cors_bk > (cors_mean + 1 * cors_sd))
p_gt_2sd <- mean(cors_bk > (cors_mean + 2 * cors_sd))
p_gt_3sd <- mean(cors_bk > (cors_mean + 3 * cors_sd))

cors_sum <- summary(cors_bk)

tab_bk <- data.frame(
  Statistic = c("Min",
                "1st Qu.",
                "Median",
                "Mean ($\\mu^{BK}$)",
                "3rd Qu.",
                "Max",
                "SD ($\\sigma^{BK}$)",
                "$\\mathbb{P}(\\rho_{xy} > \\mu^{BK} + 1\\sigma^{BK})$",
                "$\\mathbb{P}(\\rho_{xy} > \\mu^{BK} + 2\\sigma^{BK})$",
                "$\\mathbb{P}(\\rho_{xy} > \\mu^{BK} + 3\\sigma^{BK})$"),
  Value = c(as.numeric(cors_sum["Min."]),
            as.numeric(cors_sum["1st Qu."]),
            as.numeric(cors_sum["Median"]),
            as.numeric(cors_sum["Mean"]),
            as.numeric(cors_sum["3rd Qu."]),
            as.numeric(cors_sum["Max."]),
            cors_sd,
            p_gt_1sd, p_gt_2sd, p_gt_3sd)
)

tab_bk$Value <- formatC(tab_bk$Value, format = "f", digits = 4)

latex_code_bk <- print(
  xtable(tab_bk,
         caption = sprintf("Summary statistics and upper-tail shares of BK-filtered correlations (T=%d, reps=%d, K=%d, periods [%d,%d]).",
                           T, length(cors_bk), K, pl, pu),
         label = "tab:hw3_q3_bk"),
  include.rownames = FALSE,
  sanitize.text.function = identity,
  print.results = FALSE
)
cat(latex_code_bk, "\n")
