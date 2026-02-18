# econ 5345 hw2 — q3

# part (a) ------------------------------------
## setup
rm(list = ls())
library(xtable)
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
path <- file.path(script_dir, "hw2_q3.csv")
stopifnot(file.exists(path))
data <- read.csv(path, stringsAsFactors = FALSE)



## output file
out_png <- file.path(script_dir, "hw2_q3a.png")
png(out_png, width = 1000, height = 800, res = 140)

par(mfrow = c(2, 2), mar = c(4, 4, 3, 1) + 0.1)

acf(data$inflation, main = "ACF: Inflation")
pacf(data$inflation, main = "PACF: Inflation")

acf(data$gdp_growth, main = "ACF: GDP Growth")
pacf(data$gdp_growth, main = "PACF: GDP Growth")

dev.off()

cat("Saved plot to:", out_png, "\n")

# part (c) ------------------------------------
max_p <- 10
p_grid <- 0:max_p

bic_infl <- numeric(length(p_grid))
bic_gdp  <- numeric(length(p_grid))

for (p in p_grid) {
  fit_infl <- arima(data$inflation, order = c(p, 0, 0),
                    include.mean = TRUE, method = "ML")
  fit_gdp  <- arima(data$gdp_growth, order = c(p, 0, 0),
                    include.mean = TRUE, method = "ML")

  bic_infl[p + 1] <- BIC(fit_infl)
  bic_gdp[p + 1]  <- BIC(fit_gdp)
}

bic_table <- data.frame(
  p = p_grid,
  Inflation = bic_infl,
  GDPGrowth = bic_gdp
)

xt <- xtable(
  bic_table,
  caption = "BIC values for AR(p) models, p=0,\\dots,10",
  label = "tab:bic_ar"
)

print(xt,
      include.rownames = FALSE,
      caption.placement = "top",
      sanitize.text.function = identity)

# part (d) ------------------------------------
## estimation
fit_infl <- arima(data$inflation, order = c(4,0,0),
                  include.mean = TRUE, method = "ML")

fit_gdp <- arima(data$gdp_growth, order = c(0,0,0),
                 include.mean = TRUE, method = "ML")

res_infl <- residuals(fit_infl)
res_gdp  <- residuals(fit_gdp)

## Ljung-Box Q-test
lag_test <- 12

lb_infl <- Box.test(res_infl, lag = lag_test,
                    type = "Ljung-Box",
                    fitdf = 4)     # AR(4)

lb_gdp  <- Box.test(res_gdp, lag = lag_test,
                    type = "Ljung-Box",
                    fitdf = 0)     # AR(0)

## Jarque-Bera test

jb_test <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  m <- mean(x)
  m2 <- mean((x - m)^2)
  m3 <- mean((x - m)^3)
  m4 <- mean((x - m)^4)

  S <- m3 / (m2^(3/2)) 
  K <- m4 / (m2^2)         # kurtosis (not excess)
  JB <- n/6 * (S^2 + ((K - 3)^2)/4)
  pval <- 1 - pchisq(JB, df = 2)

  list(statistic = JB, p.value = pval)
}

jb_infl <- jb_test(res_infl)
jb_gdp  <- jb_test(res_gdp)

diag_table <- data.frame(
  Series = c("Inflation (AR(4))", "GDP Growth (AR(0))"),
  LB_Statistic = c(as.numeric(lb_infl$statistic),
                   as.numeric(lb_gdp$statistic)),
  LB_pvalue = c(lb_infl$p.value,
                lb_gdp$p.value),
  JB_Statistic = c(jb_infl$statistic,
                   jb_gdp$statistic),
  JB_pvalue = c(jb_infl$p.value,
                jb_gdp$p.value)
)

xt <- xtable(diag_table,
             caption = "Diagnostic Tests: Ljung--Box and Jarque--Bera",
             label = "tab:diagnostics")

print(xt,
      include.rownames = FALSE,
      caption.placement = "top",
      digits = c(0,0,3,4,3,4),
      sanitize.text.function = identity)

## ACF/PACF
out_png <- file.path(script_dir, "hw2_q3d.png")
png(out_png, width = 1000, height = 800, res = 140)

par(mfrow = c(2,2))

acf(res_infl, main = "ACF: AR(4) Inflation Residuals")
pacf(res_infl, main = "PACF: AR(4) Inflation Residuals")

acf(res_gdp, main = "ACF: AR(0) GDP Growth Residuals")
pacf(res_gdp, main = "PACF: AR(0) GDP Growth Residuals")

dev.off()

cat("Saved residual diagnostics to:", out_png, "\n")

# part (e) ------------------------------------

## deal with the date
date_col <- names(data)[1]
d_raw <- data[[date_col]]

d_try <- suppressWarnings(as.Date(d_raw))
is_date_ok <- !all(is.na(d_try))

if (is_date_ok) {
  yy <- as.integer(format(d_try, "%Y"))
  mm <- as.integer(format(d_try, "%m"))
  qq <- (mm - 1) %/% 3 + 1
} else {
  s <- as.character(d_raw)
  s <- gsub(":", "", s)
  s <- gsub("-", "", s)
  s <- gsub(" ", "", s)
  # now expect "2014Q4"
  yy <- as.integer(substr(s, 1, 4))
  qq <- as.integer(sub(".*Q", "", s))
}

qid <- yy * 10 + qq

train_end <- 2014 * 10 + 4
test_start <- 2015 * 10 + 1
test_end <- 2019 * 10 + 4

i_train_end <- max(which(qid <= train_end))
stopifnot(is.finite(i_train_end), i_train_end >= 1)

i_test <- which(qid >= test_start & qid <= test_end)
stopifnot(length(i_test) > 0)
H <- length(i_test)

## data (training test)
infl_real <- data$inflation[i_test]
gdp_real  <- data$gdp_growth[i_test]
q_label <- paste0(yy[i_test], "Q", qq[i_test])

infl_train <- data$inflation[1:i_train_end]
gdp_train  <- data$gdp_growth[1:i_train_end]

fit_infl_train <- arima(infl_train, order = c(4,0,0), include.mean = TRUE, method = "ML")
fit_gdp_train  <- arima(gdp_train,  order = c(0,0,0), include.mean = TRUE, method = "ML")

## coefficients
co_infl <- fit_infl_train$coef
phi_infl <- unname(co_infl[paste0("ar", 1:4)])
c_infl <- if ("intercept" %in% names(co_infl)) unname(co_infl["intercept"]) else 0
sig_infl <- sqrt(fit_infl_train$sigma2)

co_gdp <- fit_gdp_train$coef
c_gdp <- if ("intercept" %in% names(co_gdp)) unname(co_gdp["intercept"]) else mean(gdp_train)
sig_gdp <- sqrt(fit_gdp_train$sigma2)

## point forecast
infl_fc <- numeric(H)
infl_hist <- infl_train
for (h in 1:H) {
  lags <- rev(tail(infl_hist, 4))
  infl_fc[h] <- c_infl + sum(phi_infl * lags)
  infl_hist <- c(infl_hist, infl_fc[h])
}

gdp_fc <- rep(c_gdp, H)

## fe bands
set.seed(1)
n_sims <- 5000

infl_sim <- matrix(NA_real_, nrow = n_sims, ncol = H)
for (s in 1:n_sims) {
  infl_hist_s <- infl_train
  for (h in 1:H) {
    eps <- rnorm(1, 0, sig_infl)
    lags <- rev(tail(infl_hist_s, 4))
    y_next <- c_infl + sum(phi_infl * lags) + eps
    infl_sim[s, h] <- y_next
    infl_hist_s <- c(infl_hist_s, y_next)
  }
}
infl_sd <- apply(infl_sim, 2, sd)
infl_l1 <- infl_fc - infl_sd; infl_u1 <- infl_fc + infl_sd
infl_l2 <- infl_fc - 2*infl_sd; infl_u2 <- infl_fc + 2*infl_sd

gdp_sim <- matrix(NA_real_, nrow = n_sims, ncol = H)
for (s in 1:n_sims) {
  eps <- rnorm(H, 0, sig_gdp)
  gdp_sim[s, ] <- c_gdp + eps
}
gdp_sd <- apply(gdp_sim, 2, sd)
gdp_l1 <- gdp_fc - gdp_sd; gdp_u1 <- gdp_fc + gdp_sd
gdp_l2 <- gdp_fc - 2*gdp_sd; gdp_u2 <- gdp_fc + 2*gdp_sd

## plotting inflation fan chart
out_png <- file.path(script_dir, "hw2_q3e_inflation.png")
png(out_png, width = 1100, height = 700, res = 140)
par(mar = c(6, 4, 3, 1) + 0.1)

x <- 1:H
ylim <- range(c(infl_l2, infl_u2, infl_real), na.rm = TRUE)

plot(x, infl_fc, type = "n", ylim = ylim,
     xlab = "", ylab = "Inflation",
     main = "Inflation AR(4): Forecasts from 2014Q4 to 2019Q4")


polygon(c(x, rev(x)), c(infl_l2, rev(infl_u2)), col = "grey85", border = NA)

polygon(c(x, rev(x)), c(infl_l1, rev(infl_u1)), col = "grey70", border = NA)

lines(x, infl_fc, lwd = 2)
lines(x, infl_real, lwd = 2, lty = 2)
abline(h = 0, lty = 3)

axis(1, at = x, labels = q_label, las = 2, cex.axis = 0.8)

legend("topleft",
       legend = c("Point forecast", "Realized", "±1 sd", "±2 sd"),
       lty = c(1, 2, NA, NA), lwd = c(2, 2, NA, NA),
       pch = c(NA, NA, 15, 15),
       col = c("black", "black", "grey70", "grey85"),
       pt.cex = c(1, 1, 2, 2),
       bty = "n")

dev.off()
cat("Saved inflation fan chart to:", out_png, "\n")

## plotting GDP fan chart
out_png <- file.path(script_dir, "hw2_q3e_gdp_growth.png")
png(out_png, width = 1100, height = 700, res = 140)
par(mar = c(6, 4, 3, 1) + 0.1)

ylim <- range(c(gdp_l2, gdp_u2, gdp_real), na.rm = TRUE)

plot(x, gdp_fc, type = "n", ylim = ylim,
     xlab = "", ylab = "GDP Growth",
     main = "GDP Growth AR(0): Forecasts from 2014Q4 to 2019Q4")

polygon(c(x, rev(x)), c(gdp_l2, rev(gdp_u2)), col = "grey85", border = NA)
polygon(c(x, rev(x)), c(gdp_l1, rev(gdp_u1)), col = "grey70", border = NA)

lines(x, gdp_fc, lwd = 2)
lines(x, gdp_real, lwd = 2, lty = 2)
abline(h = 0, lty = 3)

axis(1, at = x, labels = q_label, las = 2, cex.axis = 0.8)

legend("topleft",
       legend = c("Point forecast", "Realized", "±1 sd", "±2 sd"),
       lty = c(1, 2, NA, NA), lwd = c(2, 2, NA, NA),
       pch = c(NA, NA, 15, 15),
       col = c("black", "black", "grey70", "grey85"),
       pt.cex = c(1, 1, 2, 2),
       bty = "n")

dev.off()
cat("Saved GDP growth fan chart to:", out_png, "\n")


## forecast error
infl_error <- infl_real - infl_fc
gdp_error  <- gdp_real - gdp_fc

out_png <- file.path(script_dir, "hw2_q3e_inflation_error.png")
png(out_png, width = 1000, height = 600, res = 140)

plot(1:H, infl_error, type = "b", pch = 16,
     xlab = "", ylab = "Forecast Error",
     main = "Inflation Forecast Errors (Realized - Forecast)")

abline(h = 0, lty = 2)
axis(1, at = 1:H, labels = q_label, las = 2, cex.axis = 0.8)

dev.off()
cat("Saved inflation forecast error plot to:", out_png, "\n")

out_png <- file.path(script_dir, "hw2_q3e_gdp_error.png")
png(out_png, width = 1000, height = 600, res = 140)

plot(1:H, gdp_error, type = "b", pch = 16,
     xlab = "", ylab = "Forecast Error",
     main = "GDP Growth Forecast Errors (Realized - Forecast)")

abline(h = 0, lty = 2)
axis(1, at = 1:H, labels = q_label, las = 2, cex.axis = 0.8)

dev.off()
cat("Saved GDP growth forecast error plot to:", out_png, "\n")
