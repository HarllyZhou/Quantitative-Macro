# econ 5345 hw2 — q3

# part (a) ------------------------------------
## setup
rm(list = ls())
library(xtable)
graphics.off()
script_arg <- commandArgs()
script_path <- sub("^--file=", "", script_arg[grep("^--file=", script_arg)])
script_dir <- if (length(script_path) == 1L) dirname(normalizePath(script_path)) else getwd()

path <- file.path(script_dir, "hw2_q3.csv")
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
library(tseries)

## estimation
fit_infl <- arima(data$inflation, order = c(4,0,0),
                  include.mean = TRUE, method = "ML")

fit_gdp <- arima(data$gdp_growth, order = c(0,0,0),
                 include.mean = TRUE, method = "ML")

res_infl <- residuals(fit_infl)
res_gdp  <- residuals(fit_gdp)

## Ljung-Box Q-test
lb_infl <- Box.test(res_infl, lag = 12, type = "Ljung-Box")
lb_gdp  <- Box.test(res_gdp, lag = 12, type = "Ljung-Box")

lb_infl
lb_gdp

## Jarque-Bera test
jb_infl <- jarque.bera.test(res_infl)
jb_gdp  <- jarque.bera.test(res_gdp)

jb_infl
jb_gdp

## ACF/PACF
out_png <- file.path(script_dir, "hw2_q3_residual_diagnostics.png")
png(out_png, width = 1000, height = 800, res = 140)

par(mfrow = c(2,2))

acf(res_infl, main = "ACF: AR(4) Inflation Residuals")
pacf(res_infl, main = "PACF: AR(4) Inflation Residuals")

acf(res_gdp, main = "ACF: AR(0) GDP Growth Residuals")
pacf(res_gdp, main = "PACF: AR(0) GDP Growth Residuals")

dev.off()

cat("Saved residual diagnostics to:", out_png, "\n")

