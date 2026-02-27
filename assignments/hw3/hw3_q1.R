## econ5345 hw3 -- q1

rm(list = ls())

# ---- paths / output ----
get_script_path <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_arg) >= 1) {
    return(normalizePath(sub("^--file=", "", file_arg[1])))
  }
  ofile <- tryCatch(sys.frames()[[1]]$ofile, error = function(e) NULL)
  if (!is.null(ofile)) {
    return(normalizePath(ofile))
  }
  NULL
}

script_path <- get_script_path()
output_dir <- if (!is.null(script_path)) {
  dirname(script_path)
} else if (dir.exists(file.path(getwd(), "assignments", "hw3"))) {
  file.path(getwd(), "assignments", "hw3")
} else {
  getwd()
}

save_png <- function(filename, width = 2000, height = 1400, res = 220, expr) {
  png(filename = filename, width = width, height = height, res = res)
  on.exit(dev.off(), add = TRUE)
  force(expr)
}


# ---- transforms ----
logdiff_pct <- function(x) {
  100 * diff(log(as.numeric(x)))
}

# HP filter (cycle) without external packages
hp_cycle <- function(x, lambda = 1600) {
  x <- as.numeric(x)
  T <- length(x)
  if (T < 6) stop("Need at least 6 observations for HP filter.")

  k0 <- c(1, 5, rep(6, T - 4), 5, 1)
  k1 <- c(-2, rep(-4, T - 3), -2)  # length T-1
  k2 <- rep(1, T - 2)

  A <- diag(1 + lambda * k0, T, T)
  for (i in 1:(T - 1)) {
    A[i, i + 1] <- lambda * k1[i]
    A[i + 1, i] <- lambda * k1[i]
  }
  for (i in 1:(T - 2)) {
    A[i, i + 2] <- lambda * k2[i]
    A[i + 2, i] <- lambda * k2[i]
  }

  trend <- solve(A, x)
  x - trend
}

# ---- spectral estimators ----
periodogram_div_2pi <- function(x) {
  x <- as.numeric(x)
  x <- x - mean(x, na.rm = TRUE)
  T <- length(x)

  fk <- fft(x) / sqrt(T)     # matches lecture slide f_k
  Iw <- Mod(fk)^2            # periodogram
  Sw <- Iw / (2 * pi)        # requested scaling

  omega <- 2 * pi * (0:(T - 1)) / T
  list(omega = omega, S = Sw, T = T)
}

fit_ar_ols_aic_grid <- function(x, p_max = 20) {
  x <- as.numeric(x)
  x <- x - mean(x, na.rm = TRUE)
  T <- length(x)

  p_max_eff <- max(0, min(p_max, T - 2))
  aic_grid <- rep(NA_real_, p_max + 1) # indices 1..p_max+1 correspond to p=0..p_max
  best <- list(p = 0, phi = numeric(0), sigma2 = var(x), aic = Inf)

  # p = 0 baseline (white noise)
  sigma2_0 <- mean(x^2)
  aic0 <- T * log(sigma2_0)
  aic_grid[1] <- aic0
  best$aic <- aic0
  best$sigma2 <- sigma2_0

  for (p in 1:p_max_eff) {
    emb <- embed(x, p + 1)      # cols: x_t, x_{t-1}, ..., x_{t-p}
    y <- emb[, 1]
    X <- emb[, 2:(p + 1), drop = FALSE]
    fit <- lm(y ~ X - 1)  # no intercept; x is demeaned
    e <- resid(fit)
    sigma2 <- mean(e^2)
    # Use a consistent sample-size scaling across p.
    aic <- T * log(sigma2) + 2 * p
    aic_grid[p + 1] <- aic
    if (is.finite(aic) && aic < best$aic) {
      best <- list(p = p, phi = as.numeric(coef(fit)), sigma2 = sigma2, aic = aic)
    }
  }
  list(best = best, aic_grid = aic_grid)
}

ar_spectrum <- function(omega, phi, sigma2) {
  p <- length(phi)
  if (p == 0) {
    return(rep(sigma2 / (2 * pi), length(omega)))
  }
  denom <- rep(1 + 0i, length(omega))
  for (j in 1:p) {
    denom <- denom - phi[j] * exp(-1i * omega * j)
  }
  (sigma2 / (2 * pi)) * (1 / (Mod(denom)^2))
}

nw_spectrum_bartlett <- function(x, omega, M) {
  x <- as.numeric(x)
  x <- x - mean(x, na.rm = TRUE)
  T <- length(x)
  M <- max(0, min(M, T - 1))

  gamma <- numeric(M + 1)
  for (k in 0:M) {
    gamma[k + 1] <- sum(x[(k + 1):T] * x[1:(T - k)]) / T
  }

  if (M == 0) return(rep(gamma[1] / (2 * pi), length(omega)))

  w <- 1 - (1:M) / (M + 1)  # Bartlett / Newey-West weights
  S <- gamma[1] + 2 * rowSums(vapply(1:M, function(k) w[k] * gamma[k + 1] * cos(k * omega), numeric(length(omega))))
  S / (2 * pi)
}

prewhiten_nw <- function(x, omega, M) {
  x <- as.numeric(x)
  x <- x - mean(x, na.rm = TRUE)
  T <- length(x)
  if (T < 3) stop("Too short for pre-whitening.")

  y <- x[2:T]
  X <- x[1:(T - 1)]
  phi <- as.numeric(coef(lm(y ~ X - 1)))
  e <- y - phi * X

  Se <- nw_spectrum_bartlett(e, omega, M)
  recolor <- 1 / (Mod(1 - phi * exp(-1i * omega))^2)
  list(phi = phi, S = Se * recolor)
}

# ---- analysis blocks ----
clean_series <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  x - mean(x)
}

make_omega_grid <- function(n = 2000L) {
  seq(0, pi, length.out = n)
}

choose_nw_bandwidth <- function(T, M_factor = 4) {
  min(floor(M_factor * T^(1/3)), T - 1)
}

compute_ar_aic_and_spectrum <- function(x, omega_grid, p_max = 20) {
  ar_out <- fit_ar_ols_aic_grid(x, p_max = p_max)
  ar_fit <- ar_out$best
  S <- ar_spectrum(omega_grid, ar_fit$phi, ar_fit$sigma2)
  list(fit = ar_fit, aic_grid = ar_out$aic_grid, S = S)
}

compute_nw_spectrum <- function(x, omega_grid, M) {
  nw_spectrum_bartlett(x, omega_grid, M)
}

compute_prewhitened_nw_spectrum <- function(x, omega_grid, M) {
  prewhiten_nw(x, omega_grid, M)
}

plot_methods_2x2 <- function(pg, omega_grid, Sar, Snw, Spw, figure_title, out_png) {
  save_png(out_png, expr = {
    par(mfrow = c(2, 2), mar = c(4, 4, 1.5, 1), oma = c(0, 0, 3, 0))

    # (a) periodogram / (2*pi)
    keep <- pg$omega <= pi
    plot(
      pg$omega[keep], pg$S[keep],
      type = "l",
      xlab = expression(omega),
      ylab = expression(I(omega)/(2*pi)),
      main = ""
    )

    # (b) AR spectrum
    plot(
      omega_grid, Sar,
      type = "l",
      xlab = expression(omega),
      ylab = expression(hat(s)(omega)),
      main = ""
    )

    # (c) NW spectrum
    plot(
      omega_grid, Snw,
      type = "l",
      xlab = expression(omega),
      ylab = expression(hat(s)(omega)),
      main = ""
    )

    # (d) prewhitened NW
    plot(
      omega_grid, Spw,
      type = "l",
      xlab = expression(omega),
      ylab = expression(hat(s)(omega)),
      main = ""
    )

    mtext(figure_title, outer = TRUE, cex = 1.05, line = 1)
  })
}

# ---- analysis wrapper (short) ----
analyze_one <- function(x, figure_title, file_stub, p_max = 20, M_factor = 4) {
  x <- clean_series(x)
  T <- length(x)

  pg <- periodogram_div_2pi(x)
  omega_grid <- make_omega_grid(2000L)

  ar <- compute_ar_aic_and_spectrum(x, omega_grid, p_max = p_max)
  M <- choose_nw_bandwidth(T, M_factor = M_factor)
  Snw <- compute_nw_spectrum(x, omega_grid, M)
  pw <- compute_prewhitened_nw_spectrum(x, omega_grid, M)

  out_png <- file.path(output_dir, paste0(file_stub, "_methods.png"))
  plot_methods_2x2(pg, omega_grid, ar$S, Snw, pw$S, figure_title, out_png)

  list(
    figure_title = figure_title,
    T = T,
    M = M,
    ar_p = ar$fit$p,
    ar_aic = ar$fit$aic,
    ar_aic_grid = ar$aic_grid,
    ar_sigma2 = ar$fit$sigma2,
    ar_phi1 = if (ar$fit$p >= 1) ar$fit$phi[1] else NA_real_,
    pw_phi = pw$phi,
    output_png = basename(out_png)
  )
}

# ---- load data ----
csv_path <- file.path(output_dir, "hw3_q1_data.csv")
dat <- read.csv(csv_path, stringsAsFactors = FALSE)
if ("observation_date" %in% names(dat)) {
  dat$observation_date <- as.Date(dat$observation_date)
}

# Expect at least real GDP level as GDPC1 (FRED series name)
if (!("GDPC1" %in% names(dat))) stop("Expected column GDPC1 in hw3_q1_data.csv.")
if (!("GDPDEF_PC1" %in% names(dat))) stop("Expected column GDPDEF_PC1 in hw3_q1_data.csv.")
if (!("EFFR" %in% names(dat))) stop("Expected column EFFR in hw3_q1_data.csv.")

gdp_level <- dat$GDPC1
gdp_growth <- logdiff_pct(gdp_level)

res <- list()
res[[1]] <- analyze_one(
  gdp_growth,
  figure_title = "Real GDP growth (demeaned)",
  file_stub = "hw3_q1_gdp_growth"
)

# HP cyclical component (apply to log level)
gdp_log <- 100 * log(as.numeric(gdp_level))
gdp_log <- gdp_log[is.finite(gdp_log)]
gdp_cycle <- hp_cycle(gdp_log, lambda = 1600)
res[[2]] <- analyze_one(
  gdp_cycle,
  figure_title = "Real GDP (HP cyclical component)",
  file_stub = "hw3_q1_gdp_hp_cycle"
)

# Inflation (already a rate): demean only, no log-diff
infl <- as.numeric(dat$GDPDEF_PC1)
res[[3]] <- analyze_one(
  infl,
  figure_title = "GDP deflator inflation rate (demeaned)",
  file_stub = "hw3_q1_inflation"
)

infl_cycle <- hp_cycle(infl[is.finite(infl)], lambda = 1600)
res[[4]] <- analyze_one(
  infl_cycle,
  figure_title = "GDP deflator inflation (HP cyclical component)",
  file_stub = "hw3_q1_inflation_hp_cycle"
)

# Federal funds rate (already a rate): demean only, no log-diff
ffr <- as.numeric(dat$EFFR)
res[[5]] <- analyze_one(
  ffr,
  figure_title = "Effective fed funds rate (demeaned)",
  file_stub = "hw3_q1_ffr"
)

ffr_cycle <- hp_cycle(ffr[is.finite(ffr)], lambda = 1600)
res[[6]] <- analyze_one(
  ffr_cycle,
  figure_title = "Effective fed funds rate (HP cyclical component)",
  file_stub = "hw3_q1_ffr_hp_cycle"
)

