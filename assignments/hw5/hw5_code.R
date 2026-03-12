# econ 5345 hw5 code
rm(list = ls())
options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  if (!requireNamespace("MASS", quietly = TRUE)) stop("Package 'MASS' is required (comes with base R).")
})

# parameters

H <- 20                 # IRF/FEVD horizon
p_max <- 8              # maximum lag considered for selection
set.seed(5345)

B_mc <- 2000            # Parametric Monte-Carlo draws for IRF bands
B_boot <- 2000          # Bootstrap draws for IRF bands

# Subsample for the additional IRF requested (inclusive)
irf_subsample_start <- as.Date("1974-01-01")
irf_subsample_end   <- as.Date("1990-12-31")

## Variable ordering for Q3 (Cholesky)
order_q3 <- c("GDPC1_PCH", "GDPDEF_PCH", "TWEXM_PCH", "FEDFUNDS")
## Variable ordering for Q4 (Cholesky)
order_q4 <- c("TWEXM_PCH", "GDPC1_PCH", "GDPDEF_PCH", "FEDFUNDS")

## cumulation for changes to levels
cumulate_idx_default <- 1:3

# path

candidate_paths <- c(
  "assignments/hw5/hw5_data.csv",
  "hw5_data.csv"
)
data_path <- candidate_paths[file.exists(candidate_paths)][1]
if (is.na(data_path)) stop("Could not find hw5_data.csv. Tried: ", paste(candidate_paths, collapse = ", "))

out_dir <- "assignments/hw5"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# functions

logdet <- function(S) as.numeric(determinant(S, logarithm = TRUE)$modulus)

stop_if_not_posdef <- function(S, name = "matrix") {
  ev <- eigen(S, symmetric = TRUE, only.values = TRUE)$values
  if (min(ev) <= 1e-12) stop(name, " is not positive definite (min eigenvalue = ", signif(min(ev), 4), ").")
  invisible(TRUE)
}

build_design <- function(y, p, drop_lags, include_const = TRUE) {
  Tn <- nrow(y)
  k <- ncol(y)
  if (drop_lags < p) stop("drop_lags must be >= p.")
  if (Tn <= drop_lags) stop("Not enough observations for the requested drop_lags.")

  n <- Tn - drop_lags
  Y <- y[(drop_lags + 1):Tn, , drop = FALSE]

  X_parts <- list()
  if (include_const) {
    X_parts[[length(X_parts) + 1]] <- matrix(1, nrow = n, ncol = 1)
  }
  for (lag in 1:p) {
    X_parts[[length(X_parts) + 1]] <- y[(drop_lags + 1 - lag):(Tn - lag), , drop = FALSE]
  }
  X <- do.call(cbind, X_parts)

  coln <- character(0)
  if (include_const) coln <- c(coln, "const")
  if (p > 0) {
    for (lag in 1:p) coln <- c(coln, paste0(colnames(y), "_L", lag))
  }
  colnames(X) <- coln
  colnames(Y) <- colnames(y)

  list(X = X, Y = Y, n = n, k = k)
}

extract_var_mats <- function(B, k, p, include_const = TRUE) {
  if (include_const) {
    c_vec <- as.numeric(B[1, ])
    start <- 2
  } else {
    c_vec <- rep(0, k)
    start <- 1
  }
  A_list <- vector("list", p)
  if (p > 0) {
    for (lag in 1:p) {
      rows <- start + (lag - 1) * k + (0:(k - 1))
      B_block <- B[rows, , drop = FALSE]  
      A_list[[lag]] <- t(B_block)        
    }
  }
  list(c = c_vec, A = A_list)
}

estimate_var_ols <- function(y, p, drop_lags = p, include_const = TRUE) {
  d <- build_design(y, p = p, drop_lags = drop_lags, include_const = include_const)
  X <- d$X
  Y <- d$Y
  n <- d$n
  k <- d$k
  q <- ncol(X)

  qrX <- qr(X)
  if (qrX$rank < q) stop("Design matrix is rank-deficient (rank=", qrX$rank, ", q=", q, ").")

  B <- qr.coef(qrX, Y)  # q x k
  E <- qr.resid(qrX, Y) # n x k
  B <- as.matrix(B)
  E <- as.matrix(E)

  XtX_inv <- chol2inv(qr.R(qrX))

  Sigma_mle <- crossprod(E) / n
  df <- n - q
  if (df <= 0) stop("Non-positive residual df; reduce p or increase sample.")
  Sigma_df <- crossprod(E) / df

  mats <- extract_var_mats(B, k = k, p = p, include_const = include_const)

  list(
    p = p, k = k, n = n, q = q, df = df,
    X = X, Y = Y, B = B, E = E,
    XtX_inv = XtX_inv,
    c = mats$c, A = mats$A,
    Sigma_mle = Sigma_mle,
    Sigma_df = Sigma_df
  )
}

compute_loglik_mle <- function(Sigma_mle, n) {
  k <- nrow(Sigma_mle)
  stop_if_not_posdef(Sigma_mle, "Sigma_mle")
  ll <- -(n * k / 2) * log(2 * pi) - (n / 2) * logdet(Sigma_mle) - (n * k / 2)
  as.numeric(ll)
}

compute_ic_fixed_sample <- function(y, p_max, include_const = TRUE) {
  Tn <- nrow(y)
  k <- ncol(y)
  n_fixed <- Tn - p_max
  if (n_fixed <= 0) stop("p_max is too large for sample size.")

  out <- data.frame(
    p = 1:p_max,
    n = n_fixed,
    logLik = NA_real_,
    logdetSigma = NA_real_,
    AIC = NA_real_,
    BIC = NA_real_,
    HQ = NA_real_,
    LR_to_pplus1 = NA_real_,
    LR_df = k^2,
    LR_pval = NA_real_
  )

  fits <- vector("list", p_max)

  for (pp in 1:p_max) {
    fit <- estimate_var_ols(y, p = pp, drop_lags = p_max, include_const = include_const)
    fits[[pp]] <- fit

    logdetS <- logdet(fit$Sigma_mle)
    npar <- k^2 * pp + (if (include_const) k else 0)

    out$logLik[out$p == pp] <- compute_loglik_mle(fit$Sigma_mle, n = n_fixed)
    out$logdetSigma[out$p == pp] <- logdetS
    out$AIC[out$p == pp] <- logdetS + (2 * npar) / n_fixed
    out$BIC[out$p == pp] <- logdetS + (log(n_fixed) * npar) / n_fixed
    out$HQ[out$p == pp] <- logdetS + (2 * log(log(n_fixed)) * npar) / n_fixed
  }

  for (pp in 1:(p_max - 1)) {
    lr <- n_fixed * (out$logdetSigma[out$p == pp] - out$logdetSigma[out$p == (pp + 1)])
    out$LR_to_pplus1[out$p == pp] <- lr
    out$LR_pval[out$p == pp] <- 1 - pchisq(lr, df = k^2)
  }

  list(table = out, fits = fits, n_fixed = n_fixed)
}

compute_irf_structural <- function(A_list, Sigma, H, shock_index) {
  k <- nrow(Sigma)
  p <- length(A_list)
  stop_if_not_posdef(Sigma, "Sigma")

  Psi <- array(0, dim = c(k, k, H + 1))
  Psi[, , 1] <- diag(k)
  if (H >= 1) {
    for (h in 1:H) {
      tmp <- matrix(0, k, k)
      for (i in 1:min(h, p)) {
        tmp <- tmp + A_list[[i]] %*% Psi[, , (h - i) + 1]
      }
      Psi[, , h + 1] <- tmp
    }
  }

  P <- t(chol(Sigma))
  Theta <- array(0, dim = c(k, k, H + 1))
  for (h in 0:H) Theta[, , h + 1] <- Psi[, , h + 1] %*% P

  irf <- Theta[, shock_index, , drop = FALSE]
  dim(irf) <- c(k, H + 1)
  list(irf = irf, Theta = Theta)
}

cumulate_irf_levels <- function(irf, cumulate_idx) {
  out <- irf
  out[cumulate_idx, ] <- t(apply(out[cumulate_idx, , drop = FALSE], 1, cumsum))
  out
}

compute_fevd <- function(Theta, shock_index, H) {
  k <- dim(Theta)[1]
  fevd <- matrix(NA_real_, nrow = k, ncol = H + 1)
  for (h in 0:H) {
    num <- rep(0, k)
    den <- rep(0, k)
    for (s in 0:h) {
      th <- Theta[, , s + 1, drop = FALSE]
      th <- th[, , 1]
      num <- num + th[, shock_index]^2
      den <- den + rowSums(th^2)
    }
    fevd[, h + 1] <- num / den
  }
  fevd
}

sig_stars <- function(p) {
  ifelse(p < 0.001, "***",
         ifelse(p < 0.01, "**",
                ifelse(p < 0.05, "*",
                       ifelse(p < 0.1, ".", ""))))
}

pretty_regressor_hw5 <- function(x) {
  if (x == "const") return("Constant")
  m <- regexec("^(.*)_L([0-9]+)$", x)
  r <- regmatches(x, m)[[1]]
  if (length(r) == 3) {
    base <- r[2]
    lag <- as.integer(r[3])
    base_tex <- switch(
      base,
      dlog_rgdp_100 = "\\Delta\\log(Real\\ GDP)",
      dlog_defl_100 = "\\Delta\\log(GDP\\ Deflator)",
      dlog_exr_100  = "\\Delta\\log(EXR)",
      ffr           = "FFR",
      base
    )
    return(paste0("$", base_tex, "_{t-", lag, "}$"))
  }
  gsub("_", "\\\\_", x, fixed = TRUE)
}

var_coef_stats <- function(fit) {
  B <- fit$B                      # q x k
  q <- nrow(B)
  k <- ncol(B)
  dxx <- diag(fit$XtX_inv)
  se <- matrix(NA_real_, nrow = q, ncol = k)
  tval <- matrix(NA_real_, nrow = q, ncol = k)
  pval <- matrix(NA_real_, nrow = q, ncol = k)

  for (i in 1:k) {
    se[, i] <- sqrt(fit$Sigma_df[i, i] * dxx)
    tval[, i] <- B[, i] / se[, i]
    pval[, i] <- 2 * pt(-abs(tval[, i]), df = fit$df)
  }

  rownames(B) <- colnames(fit$X)
  rownames(se) <- colnames(fit$X)
  rownames(tval) <- colnames(fit$X)
  rownames(pval) <- colnames(fit$X)

  colnames(B) <- colnames(fit$Y)
  colnames(se) <- colnames(fit$Y)
  colnames(tval) <- colnames(fit$Y)
  colnames(pval) <- colnames(fit$Y)

  list(beta = B, se = se, t = tval, p = pval)
}

write_var_coef_table_tex <- function(fit, file, caption, label, dep_labels = NULL) {
  st <- var_coef_stats(fit)
  beta <- st$beta
  tval <- st$t
  pval <- st$p

  rhs <- vapply(rownames(beta), pretty_regressor_hw5, character(1))
  stars <- sig_stars(pval)

  # cell: coef + stars, (t)
  cells <- matrix("", nrow = nrow(beta), ncol = ncol(beta))
  for (i in 1:nrow(beta)) {
    for (j in 1:ncol(beta)) {
      cells[i, j] <- sprintf("\\shortstack{%.4f%s\\\\(%.3f)}", beta[i, j], stars[i, j], tval[i, j])
    }
  }

  if (is.null(dep_labels)) {
    dep_labels <- colnames(beta)
  }

  con <- file(file, open = "wt")
  on.exit(close(con), add = TRUE)

  writeLines("\\begin{table}[H]", con)
  writeLines("\\centering", con)
  writeLines(paste0("\\caption{", caption, "}"), con)
  writeLines(paste0("\\label{", label, "}"), con)
  writeLines("\\scriptsize", con)
  writeLines(paste0("\\begin{tabular}{l ", paste(rep("c", ncol(beta)), collapse = " "), "}"), con)
  writeLines("\\hline", con)
  writeLines(paste0("RHS regressor & ", paste(dep_labels, collapse = " & "), " \\\\"), con)
  writeLines("\\hline", con)

  for (i in 1:nrow(beta)) {
    writeLines(paste0(rhs[i], " & ", paste(cells[i, ], collapse = " & "), " \\\\"), con)
  }

  writeLines("\\hline", con)
  writeLines("\\end{tabular}", con)
  writeLines("\\vspace{0.5em}", con)
  writeLines("\\caption*{\\footnotesize Signif. codes: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1. $t$-values in parentheses.}", con)
  writeLines("\\end{table}", con)
}

write_var_output_txt <- function(fit, file, header) {
  st <- var_coef_stats(fit)
  sink(file)
  cat(header, "\n\n", sep = "")
  cat("Sample size (effective n):", fit$n, "\n")
  cat("Lags p:", fit$p, "\n")
  cat("Residual df:", fit$df, "\n\n")
  cat("Sigma (MLE):\n")
  print(fit$Sigma_mle)
  cat("\nCoefficients by equation (Estimate, Std.Error, t, p-value):\n\n")

  for (j in 1:ncol(st$beta)) {
    cat("Equation:", colnames(st$beta)[j], "\n")
    tab <- cbind(
      Estimate = st$beta[, j],
      StdError = st$se[, j],
      t = st$t[, j],
      p = st$p[, j]
    )
    print(round(tab, 6))
    cat("\n")
  }
  sink()
}

irf_bands_parametric <- function(var_fit, H, shock_index, cumulate_idx, B = 2000) {
  k <- var_fit$k
  p <- var_fit$p
  q <- var_fit$q
  df <- var_fit$df

  Sigma_u <- var_fit$Sigma_df
  stop_if_not_posdef(Sigma_u, "Sigma_df")

  Lv <- t(chol(var_fit$XtX_inv)) 

  irfs <- array(NA_real_, dim = c(B, k, H + 1))

  ok <- 0L
  for (b in 1:B) {
    out_b <- tryCatch({
      Sigma_b <- rWishart(1, df = df, Sigma = Sigma_u)[, , 1] / df
      stop_if_not_posdef(Sigma_b, "Sigma_b")
      Ru <- chol(Sigma_b) 
      Z <- matrix(rnorm(q * k), nrow = q, ncol = k)
      B_b <- var_fit$B + Lv %*% Z %*% Ru

      mats_b <- extract_var_mats(B_b, k = k, p = p, include_const = TRUE)
      irf_b <- compute_irf_structural(mats_b$A, Sigma_b, H = H, shock_index = shock_index)$irf
      cumulate_irf_levels(irf_b, cumulate_idx = cumulate_idx)
    }, error = function(e) NULL)

    if (!is.null(out_b)) {
      ok <- ok + 1L
      irfs[b, , ] <- out_b
    }
  }
  if (ok < B) cat("Parametric MC: completed ", ok, "/", B, " successful draws.\n", sep = "")

  lower <- apply(irfs, c(2, 3), quantile, probs = 0.05, na.rm = TRUE)
  upper <- apply(irfs, c(2, 3), quantile, probs = 0.95, na.rm = TRUE)
  med <- apply(irfs, c(2, 3), quantile, probs = 0.50, na.rm = TRUE)

  list(lower = lower, upper = upper, median = med, draws = irfs)
}

irf_bands_bootstrap <- function(y, p, H, shock_index, cumulate_idx, B = 2000) {
  fit <- estimate_var_ols(y, p = p, drop_lags = p, include_const = TRUE)
  k <- fit$k
  Tn <- nrow(y)

  res <- fit$E
  res <- sweep(res, 2, colMeans(res)) 
  n <- nrow(res)

  irfs <- array(NA_real_, dim = c(B, k, H + 1))

  ok <- 0L
  for (b in 1:B) {
    out_b <- tryCatch({
      idx <- sample.int(n, size = n, replace = TRUE)
      u_star <- res[idx, , drop = FALSE]

      y_star <- y
      for (t in (p + 1):Tn) {
        pred <- fit$c
        for (lag in 1:p) {
          pred <- pred + fit$A[[lag]] %*% as.numeric(y_star[t - lag, ])
        }
        y_star[t, ] <- as.numeric(pred) + u_star[t - p, ]
      }

      fit_b <- estimate_var_ols(y_star, p = p, drop_lags = p, include_const = TRUE)
      irf_b <- compute_irf_structural(fit_b$A, fit_b$Sigma_mle, H = H, shock_index = shock_index)$irf
      cumulate_irf_levels(irf_b, cumulate_idx = cumulate_idx)
    }, error = function(e) NULL)

    if (!is.null(out_b)) {
      ok <- ok + 1L
      irfs[b, , ] <- out_b
    }
  }
  if (ok < B) cat("Bootstrap: completed ", ok, "/", B, " successful draws.\n", sep = "")

  lower <- apply(irfs, c(2, 3), quantile, probs = 0.05, na.rm = TRUE)
  upper <- apply(irfs, c(2, 3), quantile, probs = 0.95, na.rm = TRUE)
  med <- apply(irfs, c(2, 3), quantile, probs = 0.50, na.rm = TRUE)

  list(lower = lower, upper = upper, median = med, draws = irfs)
}

plot_irf_grid <- function(irf, bands1 = NULL, bands2 = NULL, var_names, main_title, file) {
  k <- nrow(irf)
  H <- ncol(irf) - 1
  h <- 0:H

  png(file, width = 10, height = 7, units = "in", res = 200)
  op <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 1), oma = c(0, 0, 2.5, 0))
  on.exit({ par(op); dev.off() }, add = TRUE)

  for (i in 1:k) {
    y <- irf[i, ]
    ylim <- range(y, na.rm = TRUE)
    if (!is.null(bands1)) ylim <- range(ylim, bands1$lower[i, ], bands1$upper[i, ], na.rm = TRUE)
    if (!is.null(bands2)) ylim <- range(ylim, bands2$lower[i, ], bands2$upper[i, ], na.rm = TRUE)

    plot(h, y, type = "n", xlab = "Horizon", ylab = "", main = var_names[i], ylim = ylim)
    abline(h = 0, col = "gray70")

    if (!is.null(bands1)) {
      polygon(
        x = c(h, rev(h)),
        y = c(bands1$lower[i, ], rev(bands1$upper[i, ])),
        border = NA, col = rgb(0.2, 0.4, 0.9, 0.25)
      )
    }
    if (!is.null(bands2)) {
      polygon(
        x = c(h, rev(h)),
        y = c(bands2$lower[i, ], rev(bands2$upper[i, ])),
        border = NA, col = rgb(0.9, 0.3, 0.2, 0.22)
      )
    }

    lines(h, y, lwd = 2)
    if (!is.null(bands1) || !is.null(bands2)) {
      legend_items <- character(0)
      legend_cols <- character(0)
      if (!is.null(bands1)) {
        legend_items <- c(legend_items, "Parametric MC 5–95%")
        legend_cols <- c(legend_cols, rgb(0.2, 0.4, 0.9, 0.35))
      }
      if (!is.null(bands2)) {
        legend_items <- c(legend_items, "Bootstrap 5–95%")
        legend_cols <- c(legend_cols, rgb(0.9, 0.3, 0.2, 0.30))
      }
      legend("topright", legend = legend_items, fill = legend_cols, border = NA, bty = "n", cex = 0.9)
    }
  }
  mtext(main_title, outer = TRUE, side = 3, line = 0.5, cex = 1.1)
  invisible(TRUE)
}

plot_fevd_grid <- function(fevd, var_names, main_title, file) {
  k <- nrow(fevd)
  H <- ncol(fevd) - 1
  h <- 0:H

  png(file, width = 10, height = 7, units = "in", res = 200)
  op <- par(mfrow = c(2, 2), mar = c(4, 4, 3, 1), oma = c(0, 0, 2.5, 0))
  on.exit({ par(op); dev.off() }, add = TRUE)

  for (i in 1:k) {
    plot(h, fevd[i, ], type = "l", lwd = 2, xlab = "Horizon", ylab = "Share",
         main = var_names[i], ylim = c(0, 1))
    abline(h = 0, col = "gray70")
  }
  mtext(main_title, outer = TRUE, side = 3, line = 0.5, cex = 1.1)
  invisible(TRUE)
}

# data

raw <- read.csv(data_path)
raw$observation_date <- as.Date(raw$observation_date)

raw <- raw[order(raw$observation_date), ]

Y_all <- raw[, c("GDPC1_PCH", "GDPDEF_PCH", "TWEXM_PCH", "FEDFUNDS")]
Y_all <- as.matrix(Y_all)
colnames(Y_all) <- c("dlog_rgdp_100", "dlog_defl_100", "dlog_exr_100", "ffr")

keep <- complete.cases(Y_all)
Y_all <- Y_all[keep, , drop = FALSE]
dates <- raw$observation_date[keep]

if (nrow(Y_all) <= p_max + 5) stop("Not enough observations after dropping NA rows.")

# Q1: Lag Selection

sel <- compute_ic_fixed_sample(Y_all, p_max = p_max, include_const = TRUE)
ic_tbl <- sel$table

p_aic <- ic_tbl$p[which.min(ic_tbl$AIC)]
p_bic <- ic_tbl$p[which.min(ic_tbl$BIC)]

lag_txt <- file.path(out_dir, "hw5_lag_selection_fixed_sample.txt")
sink(lag_txt)
cat("Lag selection for 4-variable VAR (fixed sample size n = T - p_max)\n")
cat("p_max =", p_max, "\n\n")
print(ic_tbl, row.names = FALSE)
cat("\nSelected lag by AIC:", p_aic, "\n")
cat("Selected lag by BIC:", p_bic, "\n")
cat("\nSequential LR tests (p vs p+1): reject p if p-value < 0.05\n")
print(ic_tbl[, c("p", "LR_to_pplus1", "LR_df", "LR_pval")], row.names = FALSE)
sink()

cat("\n=== Lag selection using fixed sample (n = T - p_max) ===\n")
print(ic_tbl)
cat("\nSelected lag by AIC:", p_aic, "\n")
cat("Selected lag by BIC:", p_bic, "\n")
cat("\nSequential LR tests (p vs p+1): reject small p if p-value < 0.05\n")
print(ic_tbl[, c("p", "LR_to_pplus1", "LR_df", "LR_pval")])

# Q2: OLS Estimation

fit_aic <- estimate_var_ols(Y_all, p = p_aic, drop_lags = p_aic, include_const = TRUE)
cat("\n=== VAR(", p_aic, ") OLS estimated on n=", fit_aic$n, " obs ===\n", sep = "")
cat("Sigma_mle:\n"); print(fit_aic$Sigma_mle)

q2_txt <- file.path(out_dir, "hw5_q2_var_ols_output.txt")
write_var_output_txt(
  fit = fit_aic,
  file = q2_txt,
  header = paste0(
    "HW5 Question 2: VAR(", p_aic, ") OLS estimates\n",
    "Variables (ordering): [Δlog(real GDP), Δlog(GDP deflator), Δlog(EXR), FFR]"
  )
)

q2_tex <- file.path(out_dir, "hw5_q2_var_coefficients.tex")
write_var_coef_table_tex(
  fit = fit_aic,
  file = q2_tex,
  caption = paste0(
    "VAR(", p_aic, ") least squares estimates. Each cell reports the coefficient (with significance stars) and the $t$-statistic in parentheses."
  ),
  label = "tab:hw5_q2_var_ols",
  dep_labels = c("$\\Delta\\log(Real\\ GDP)$", "$\\Delta\\log(GDP\\ Deflator)$", "$\\Delta\\log(EXR)$", "$FFR$")
)

# Q3: Cholesky (I)

reorder_to <- function(Ymat, current_names, target_order) {
  idx <- match(target_order, current_names)
  if (any(is.na(idx))) stop("Ordering references unknown variables: ", paste(target_order[is.na(idx)], collapse = ", "))
  Ymat[, idx, drop = FALSE]
}

Y_q3 <- reorder_to(raw[, order_q3], current_names = order_q3, target_order = order_q3)
Y_q3 <- as.matrix(Y_q3)
colnames(Y_q3) <- c("dlog_rgdp_100", "dlog_defl_100", "dlog_exr_100", "ffr")
Y_q3 <- Y_q3[keep, , drop = FALSE]

fit_q3 <- estimate_var_ols(Y_q3, p = p_aic, drop_lags = p_aic, include_const = TRUE)
shock_idx_q3 <- which(colnames(Y_q3) == "ffr")

irf_q3_raw <- compute_irf_structural(fit_q3$A, fit_q3$Sigma_mle, H = H, shock_index = shock_idx_q3)
irf_q3 <- cumulate_irf_levels(irf_q3_raw$irf, cumulate_idx = cumulate_idx_default)

cat("\nComputing parametric Monte-Carlo bands...\n")
bands_mc_q3 <- irf_bands_parametric(fit_q3, H = H, shock_index = shock_idx_q3,
                                    cumulate_idx = cumulate_idx_default, B = B_mc)
cat("Computing bootstrap bands...\n")
bands_boot_q3 <- irf_bands_bootstrap(Y_q3, p = p_aic, H = H, shock_index = shock_idx_q3,
                                     cumulate_idx = cumulate_idx_default, B = B_boot)

plot_irf_grid(
  irf = irf_q3,
  bands1 = bands_mc_q3,
  bands2 = bands_boot_q3,
  var_names = c("log(RGDP) (cum.)", "log(Deflator) (cum.)", "log(EXR) (cum.)", "FFR"),
  main_title = paste0("IRFs to 1-sd FFR shock (Cholesky, ordering: RGDP, DEFL, EXR, FFR) | VAR(", p_aic, ")"),
  file = file.path(out_dir, "hw5_irf_q3_order_mc_vs_boot.png")
)

# Additional IRF (Q3 ordering) using only 1974–1990 subsample
sub_mask <- dates >= irf_subsample_start & dates <= irf_subsample_end
Y_q3_sub <- Y_q3[sub_mask, , drop = FALSE]
if (nrow(Y_q3_sub) > p_aic + 5) {
  fit_q3_sub <- estimate_var_ols(Y_q3_sub, p = p_aic, drop_lags = p_aic, include_const = TRUE)
  irf_q3_sub_raw <- compute_irf_structural(fit_q3_sub$A, fit_q3_sub$Sigma_mle, H = H, shock_index = shock_idx_q3)
  irf_q3_sub <- cumulate_irf_levels(irf_q3_sub_raw$irf, cumulate_idx = cumulate_idx_default)

  plot_irf_grid(
    irf = irf_q3_sub,
    bands1 = NULL,
    bands2 = NULL,
    var_names = c("log(RGDP) (cum.)", "log(Deflator) (cum.)", "log(EXR) (cum.)", "FFR"),
    main_title = paste0(
      "IRFs to 1-sd FFR shock (Q3 ordering) | Subsample 1974–1990 | VAR(", p_aic, ")"
    ),
    file = file.path(out_dir, "hw5_irf_q3_order_1974_1990.png")
  )
} else {
  warning("Subsample 1974–1990 too short for VAR(", p_aic, "); skipping subsample IRF.")
}

fevd_q3 <- compute_fevd(irf_q3_raw$Theta, shock_index = shock_idx_q3, H = H)
plot_fevd_grid(
  fevd = fevd_q3,
  var_names = c("log(RGDP)", "log(Deflator)", "log(EXR)", "FFR"),
  main_title = paste0("FEVD share due to FFR shock (ordering: RGDP, DEFL, EXR, FFR) | VAR(", p_aic, ")"),
  file = file.path(out_dir, "hw5_fevd_q3_order_ffr_shock.png")
)

write.csv(
  data.frame(h = 0:H, t(fevd_q3)),
  file = file.path(out_dir, "hw5_fevd_q3_order_ffr_shock.csv"),
  row.names = FALSE
)

# Q4: Cholesky (II)

Y_q4 <- raw[, order_q4]
Y_q4 <- as.matrix(Y_q4)
colnames(Y_q4) <- c("dlog_exr_100", "dlog_rgdp_100", "dlog_defl_100", "ffr")
Y_q4 <- Y_q4[keep, , drop = FALSE]

fit_q4 <- estimate_var_ols(Y_q4, p = p_aic, drop_lags = p_aic, include_const = TRUE)
shock_idx_q4 <- which(colnames(Y_q4) == "ffr")

irf_q4_raw <- compute_irf_structural(fit_q4$A, fit_q4$Sigma_mle, H = H, shock_index = shock_idx_q4)
irf_q4 <- cumulate_irf_levels(irf_q4_raw$irf, cumulate_idx = cumulate_idx_default)

plot_irf_grid(
  irf = irf_q4,
  bands1 = NULL,
  bands2 = NULL,
  var_names = c("log(EXR) (cum.)", "log(RGDP) (cum.)", "log(Deflator) (cum.)", "FFR"),
  main_title = paste0("IRFs to 1-sd FFR shock (Cholesky, ordering: EXR, RGDP, DEFL, FFR) | VAR(", p_aic, ")"),
  file = file.path(out_dir, "hw5_irf_q4_order.png")
)

cat("\nDone. Outputs written to:\n- ", normalizePath(out_dir), "\n", sep = "")
cat("Key files:\n")
cat("- hw5_lag_selection_fixed_sample.txt\n")
cat("- hw5_irf_q3_order_mc_vs_boot.png\n")
cat("- hw5_fevd_q3_order_ffr_shock.png\n")
cat("- hw5_irf_q4_order.png\n\n")
