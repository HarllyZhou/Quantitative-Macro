## HW4 - Question 3
## Three-variable quarterly VAR: Δlog(real GDP), Δlog(CPI), FFR
## - Lag selection: AIC, BIC, sequential LR tests over p = 0..10
##   using a fixed effective sample size (T-10) for all p
## - Then (separately) estimate the VAR using vars::VAR() with the AIC-selected lag
## - Cholesky IRFs to 1-sd FFR shock, horizons 0..20
## - Error bands (5th/95th percentiles): parametric Monte Carlo vs bootstrap
## - FEVD contribution of FFR shocks, horizons 0..20
##
## Data file (provided): hw4_q3.csv with columns:
## observation_date, GDPC1, CPIAUCSL, FEDFUNDS

rm(list = ls())
options(stringsAsFactors = FALSE)

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_flag <- "--file="
  m <- grep(file_flag, args)
  if (length(m) > 0) {
    return(dirname(normalizePath(sub(file_flag, "", args[m[1]]), winslash = "/")))
  }

  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    p <- rstudioapi::getActiveDocumentContext()$path
    if (!is.null(p) && nzchar(p)) return(dirname(normalizePath(p, winslash = "/")))
  }

  getwd()
}

script_dir <- get_script_dir()
setwd(script_dir)

needed_pkgs <- c("readr", "dplyr", "zoo", "ggplot2", "tidyr", "tibble", "vars")
missing_pkgs <- needed_pkgs[!vapply(needed_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(
    "Missing packages: ", paste(missing_pkgs, collapse = ", "),
    "\nInstall them, e.g.: install.packages(c(", paste(sprintf('\"%s\"', missing_pkgs), collapse = ", "), "))"
  )
}

library(readr)
library(dplyr)
library(zoo)
library(ggplot2)
library(tidyr)
library(vars)

parse_fred_date <- function(x) as.Date(x)

read_q3_data <- function(path) {
  df <- readr::read_csv(path, show_col_types = FALSE)
  req <- c("observation_date", "GDPC1", "CPIAUCSL", "FEDFUNDS")
  miss <- setdiff(req, names(df))
  if (length(miss) > 0) stop("Missing columns in ", path, ": ", paste(miss, collapse = ", "))

  df %>%
    transmute(
      date = parse_fred_date(.data$observation_date),
      tq = zoo::as.yearqtr(parse_fred_date(.data$observation_date)),
      GDPC1 = as.numeric(.data$GDPC1),
      CPIAUCSL = as.numeric(.data$CPIAUCSL),
      FEDFUNDS = suppressWarnings(as.numeric(.data$FEDFUNDS))
    ) %>%
    arrange(.data$date)
}

make_var_dataset <- function(df_raw, end_tq = zoo::as.yearqtr("2008 Q4")) {
  df_raw %>%
    filter(!is.na(.data$tq), .data$tq <= end_tq) %>%
    mutate(
      dlog_gdp = 100 * (log(.data$GDPC1) - log(dplyr::lag(.data$GDPC1))),
      dlog_cpi = 100 * (log(.data$CPIAUCSL) - log(dplyr::lag(.data$CPIAUCSL))),
      ffr = .data$FEDFUNDS
    ) %>%
    transmute(
      tq = .data$tq,
      dlog_gdp = .data$dlog_gdp,
      dlog_cpi = .data$dlog_cpi,
      ffr = .data$ffr
    ) %>%
    filter(is.finite(.data$dlog_gdp), is.finite(.data$dlog_cpi), is.finite(.data$ffr))
}

build_fixed_sample_mats <- function(Y, P, p, include_const = TRUE) {
  Y <- as.matrix(Y)
  Tn <- nrow(Y)
  k <- ncol(Y)
  if (P < 0) stop("P must be >= 0")
  if (p < 0 || p > P) stop("p must be in 0..P")
  if (Tn <= P) stop("Need T > P observations")

  idx <- (P + 1):Tn
  n <- length(idx)

  Y_dep <- Y[idx, , drop = FALSE]
  m <- (if (include_const) 1 else 0) + k * p
  X <- matrix(NA_real_, nrow = n, ncol = m)
  col_i <- 1

  if (include_const) {
    X[, col_i] <- 1
    col_i <- col_i + 1
  }

  if (p > 0) {
    for (lag in 1:p) {
      X[, col_i:(col_i + k - 1)] <- Y[idx - lag, , drop = FALSE]
      col_i <- col_i + k
    }
  }

  list(Y = Y_dep, X = X, n = n, k = k, T = Tn)
}

estimate_var_fixed <- function(Y, P, p, include_const = TRUE) {
  mats <- build_fixed_sample_mats(Y, P = P, p = p, include_const = include_const)
  Y_dep <- mats$Y
  X <- mats$X
  n <- mats$n
  k <- mats$k

  B <- tryCatch(qr.solve(X, Y_dep), error = function(e) NULL)
  if (is.null(B)) stop("VAR estimation failed (singular design).")

  E <- Y_dep - X %*% B
  Sigma <- crossprod(E) / n

  detS <- determinant(Sigma, logarithm = TRUE)
  if (!is.finite(detS$modulus[1]) || detS$sign <= 0) stop("Non-positive definite Sigma.")

  ll <- as.numeric(-n * k / 2 * (1 + log(2 * pi)) - n / 2 * detS$modulus[1])

  n_params <- k * ((if (include_const) 1 else 0) + k * p)
  aic <- -2 * ll + 2 * n_params
  bic <- -2 * ll + log(n) * n_params

  c_row <- if (include_const) as.numeric(B[1, ]) else rep(0, k)
  B_blocks <- vector("list", p)
  A_list <- vector("list", p)
  if (p > 0) {
    start <- (if (include_const) 2 else 1)
    for (i in 1:p) {
      block <- B[start:(start + k - 1), , drop = FALSE] # k x k (row-lag form)
      B_blocks[[i]] <- block
      A_list[[i]] <- t(block) # column-vector VAR convention
      start <- start + k
    }
  }

  L <- t(chol(Sigma)) # lower-triangular, Sigma = L %*% t(L)

  list(
    P = P,
    p = p,
    n = n,
    k = k,
    include_const = include_const,
    B = B,
    c_row = c_row,
    B_blocks = B_blocks,
    A_list = A_list,
    E = E,
    Sigma = Sigma,
    L = L,
    ll = ll,
    aic = aic,
    bic = bic,
    n_params = n_params
  )
}

ic_lr_fixed_sample <- function(Y, P = 10, include_const = TRUE, alpha = 0.05) {
  k <- ncol(Y)
  fits <- vector("list", P + 1)
  ll <- rep(NA_real_, P + 1)
  aic <- rep(NA_real_, P + 1)
  bic <- rep(NA_real_, P + 1)
  n_params <- rep(NA_real_, P + 1)
  n <- NA_integer_

  for (p in 0:P) {
    est <- tryCatch(estimate_var_fixed(Y, P = P, p = p, include_const = include_const), error = function(e) NULL)
    fits[[p + 1]] <- est
    if (!is.null(est)) {
      ll[p + 1] <- est$ll
      aic[p + 1] <- est$aic
      bic[p + 1] <- est$bic
      n_params[p + 1] <- est$n_params
      n <- est$n
    }
  }

  ic_tbl <- tibble::tibble(
    p = 0:P,
    n = n,
    n_params = n_params,
    logLik = ll,
    AIC = aic,
    BIC = bic
  )

  aic_p <- ic_tbl$p[which.min(ic_tbl$AIC)]
  bic_p <- ic_tbl$p[which.min(ic_tbl$BIC)]

  df_lr <- k^2
  p_null <- 0:(P - 1)
  p_alt <- 1:P
  logLik_null <- ll[1:P]
  logLik_alt <- ll[2:(P + 1)]
  LR <- 2 * (logLik_alt - logLik_null)
  p_value <- stats::pchisq(LR, df = df_lr, lower.tail = FALSE)

  lr_tbl <- tibble::tibble(
    p_null = p_null,
    p_alt = p_alt,
    logLik_null = logLik_null,
    logLik_alt = logLik_alt,
    LR = LR,
    df = df_lr,
    p_value = p_value
  )

  lr_choice <- NA_integer_
  for (i in seq_len(nrow(lr_tbl))) {
    if (is.na(lr_tbl$p_value[i])) next
    if (lr_tbl$p_value[i] > alpha) {
      lr_choice <- lr_tbl$p_null[i]
      break
    }
  }
  if (is.na(lr_choice)) lr_choice <- P

  list(
    ic_tbl = ic_tbl,
    lr_tbl = lr_tbl,
    aic_p = as.integer(aic_p),
    bic_p = as.integer(bic_p),
    lr_p = as.integer(lr_choice),
    fits = fits
  )
}

lag_selection_table_fixed <- function(sel) {
  # Combine IC table with sequential LR(p-1 vs p) results in one table.
  ic <- sel$ic_tbl
  lr <- sel$lr_tbl %>%
    transmute(
      p = .data$p_alt,
      LR = .data$LR,
      LR_p = .data$p_value
    )

  out <- ic %>%
    left_join(lr, by = "p") %>%
    mutate(
      LR = ifelse(.data$p == 0, NA_real_, .data$LR),
      LR_p = ifelse(.data$p == 0, NA_real_, .data$LR_p)
    )

  out
}

to_latex_tabular <- function(df, caption = NULL, label = NULL, digits = 3) {
  # Minimal LaTeX tabular (no extra dependencies).
  fmt_num <- function(x) ifelse(is.na(x), "", formatC(x, format = "f", digits = digits))
  fmt_int <- function(x) ifelse(is.na(x), "", as.character(as.integer(x)))

  df_out <- df %>%
    transmute(
      p = fmt_int(.data$p),
      n = fmt_int(.data$n),
      logLik = fmt_num(.data$logLik),
      AIC = fmt_num(.data$AIC),
      BIC = fmt_num(.data$BIC),
      LR = fmt_num(.data$LR),
      pval = fmt_num(.data$LR_p)
    )

  header <- c("\\begin{table}[!htbp]",
              "\\centering",
              "\\begin{tabular}{r r r r r r r}",
              "\\hline",
              "p & n & logLik & AIC & BIC & LR & p-value \\\\",
              "\\hline")

  body <- apply(df_out, 1, function(r) paste(r, collapse = " & "))
  body <- paste0(body, " \\\\")

  footer <- c("\\hline",
              "\\end{tabular}")

  caplab <- character(0)
  if (!is.null(caption)) caplab <- c(caplab, paste0("\\caption{", caption, "}"))
  if (!is.null(label)) caplab <- c(caplab, paste0("\\label{", label, "}"))

  endtbl <- "\\end{table}"

  paste(c(header, body, footer, caplab, endtbl), collapse = "\n")
}

extract_from_vars_fit <- function(fit) {
  A_list <- vars::Acoef(fit) # list of kxk matrices
  c_vec <- as.numeric(vars::detcoef(fit)[, "const"])
  Sigma <- summary(fit)$covres
  list(A_list = A_list, c_vec = c_vec, Sigma = Sigma, p = fit$p, k = ncol(Sigma))
}

simulate_var_from_vars <- function(A_list, c_vec, Y0, innov) {
  # Row recursion: y_t = c + sum_{i=1}^p y_{t-i} %*% t(A_i) + u_t
  Y0 <- as.matrix(Y0)
  innov <- as.matrix(innov)
  p <- length(A_list)
  k <- ncol(Y0)
  Tobs <- nrow(Y0)
  if (p == 0) stop("simulate_var_from_vars requires p >= 1")
  if (length(c_vec) != k) stop("c_vec length mismatch")
  if (!all(dim(innov) == c(Tobs - p, k))) stop("innov must be (Tobs-p) x k")

  Ysim <- Y0
  for (t in (p + 1):Tobs) {
    y_t <- c_vec
    for (i in 1:p) {
      y_t <- y_t + as.numeric(Ysim[t - i, , drop = FALSE] %*% t(A_list[[i]]))
    }
    y_t <- y_t + innov[t - p, ]
    Ysim[t, ] <- y_t
  }
  Ysim
}

irf_from_vars_fit <- function(fit, n_ahead = 20) {
  ir <- vars::irf(
    fit,
    impulse = "ffr",
    response = c("dlog_gdp", "dlog_cpi", "ffr"),
    n.ahead = n_ahead,
    ortho = TRUE,
    boot = FALSE
  )
  mat <- ir$irf[["ffr"]] # (n_ahead+1) x 3
  colnames(mat) <- c("dlog_gdp", "dlog_cpi", "ffr")
  mat[, "dlog_gdp"] <- cumsum(mat[, "dlog_gdp"])
  mat[, "dlog_cpi"] <- cumsum(mat[, "dlog_cpi"])
  mat
}

irf_bands_parametric_mc_vars <- function(Y, fit, n_ahead = 20, B = 500, seed = 1) {
  set.seed(seed)
  Y <- as.matrix(Y)
  mats <- extract_from_vars_fit(fit)
  p <- mats$p
  k <- mats$k
  if (p == 0) stop("Parametric MC needs p >= 1 (VAR(0) not handled here).")

  cholS <- chol(mats$Sigma) # upper-triangular
  Tobs <- nrow(Y)

  irf_draws <- array(NA_real_, dim = c(B, n_ahead + 1, k),
                     dimnames = list(NULL, 0:n_ahead, colnames(Y)))
  kept <- 0L

  for (b in 1:B) {
    innov <- matrix(rnorm((Tobs - p) * k), nrow = (Tobs - p), ncol = k) %*% cholS
    Ysim <- simulate_var_from_vars(mats$A_list, mats$c_vec, Y0 = Y, innov = innov)
    fit_b <- tryCatch(vars::VAR(as.data.frame(Ysim), p = p, type = "const"), error = function(e) NULL)
    if (is.null(fit_b)) next
    mat_b <- tryCatch(irf_from_vars_fit(fit_b, n_ahead = n_ahead), error = function(e) NULL)
    if (is.null(mat_b)) next
    kept <- kept + 1L
    irf_draws[kept, , ] <- mat_b
  }

  irf_draws <- irf_draws[seq_len(kept), , , drop = FALSE]
  list(draws = irf_draws, kept = kept, B = B)
}

irf_bands_bootstrap_vars <- function(Y, fit, n_ahead = 20, B = 500, seed = 2) {
  set.seed(seed)
  Y <- as.matrix(Y)
  mats <- extract_from_vars_fit(fit)
  p <- mats$p
  k <- mats$k
  if (p == 0) stop("Bootstrap needs p >= 1 (VAR(0) not handled here).")

  res <- residuals(fit)
  res <- res[stats::complete.cases(res), , drop = FALSE] # (Tobs-p) x k

  Tobs <- nrow(Y)
  if (nrow(res) != (Tobs - p)) {
    # In case vars drops extra rows for some reason, align to available residuals.
    Tobs <- nrow(res) + p
    Y <- Y[seq_len(Tobs), , drop = FALSE]
  }

  irf_draws <- array(NA_real_, dim = c(B, n_ahead + 1, k),
                     dimnames = list(NULL, 0:n_ahead, colnames(Y)))
  kept <- 0L

  for (b in 1:B) {
    idx <- sample.int(nrow(res), size = (Tobs - p), replace = TRUE)
    innov <- as.matrix(res[idx, , drop = FALSE])
    Ysim <- simulate_var_from_vars(mats$A_list, mats$c_vec, Y0 = Y, innov = innov)
    fit_b <- tryCatch(vars::VAR(as.data.frame(Ysim), p = p, type = "const"), error = function(e) NULL)
    if (is.null(fit_b)) next
    mat_b <- tryCatch(irf_from_vars_fit(fit_b, n_ahead = n_ahead), error = function(e) NULL)
    if (is.null(mat_b)) next
    kept <- kept + 1L
    irf_draws[kept, , ] <- mat_b
  }

  irf_draws <- irf_draws[seq_len(kept), , , drop = FALSE]
  list(draws = irf_draws, kept = kept, B = B)
}

fevd_ffr_from_vars <- function(fit, n_ahead = 20) {
  mats <- extract_from_vars_fit(fit)
  # Wrap into the minimal structure expected by fevd_ffr()
  est <- list(
    p = mats$p,
    k = mats$k,
    A_list = mats$A_list,
    L = t(chol(mats$Sigma))
  )
  fevd_ffr(est, n_ahead = n_ahead, shock_var = "ffr")
}

ma_coefs <- function(A_list, H) {
  p <- length(A_list)
  if (p == 0) stop("Need p >= 1 to compute MA coefficients.")
  k <- nrow(A_list[[1]])
  Phi <- array(0, dim = c(H + 1, k, k))
  Phi[1, , ] <- diag(k)

  for (h in 1:H) {
    acc <- matrix(0, nrow = k, ncol = k)
    for (i in 1:min(p, h)) {
      acc <- acc + A_list[[i]] %*% Phi[h - i + 1, , ]
    }
    Phi[h + 1, , ] <- acc
  }
  Phi
}

ma_coefs_levels <- function(Phi) {
  # Transform MA matrices so that the first two variables are cumulated (log-levels),
  # while FFR remains in levels.
  H1 <- dim(Phi)[1]
  k <- dim(Phi)[2]
  out <- array(0, dim = dim(Phi))
  S <- matrix(0, nrow = k, ncol = k)
  for (h in 1:H1) {
    S <- S + Phi[h, , ]
    out[h, , ] <- Phi[h, , ]
    out[h, 1:2, ] <- S[1:2, , drop = FALSE]
  }
  out
}

irf_to_ffr_shock <- function(est, n_ahead = 20, shock_var = "ffr") {
  k <- est$k
  if (k != 3) stop("Expected k = 3 variables.")
  var_names <- c("dlog_gdp", "dlog_cpi", "ffr")
  shock_idx <- match(shock_var, var_names)
  if (is.na(shock_idx)) stop("Unknown shock_var: ", shock_var)

  if (est$p == 0) {
    Phi <- array(0, dim = c(n_ahead + 1, k, k))
    Phi[1, , ] <- diag(k)
  } else {
    Phi <- ma_coefs(est$A_list, H = n_ahead)
  }

  impact0 <- est$L[, shock_idx, drop = FALSE] # k x 1
  resp <- matrix(NA_real_, nrow = n_ahead + 1, ncol = k)
  for (h in 0:n_ahead) {
    resp[h + 1, ] <- as.numeric(Phi[h + 1, , ] %*% impact0)
  }
  colnames(resp) <- var_names

  # Convert Δlog responses to log-level responses by cumulation (GDP and CPI only)
  resp[, "dlog_gdp"] <- cumsum(resp[, "dlog_gdp"])
  resp[, "dlog_cpi"] <- cumsum(resp[, "dlog_cpi"])

  resp
}

simulate_var_row <- function(est, Y_init, innov) {
  # Row-vector simulation:
  # y_t = c + sum_{i=1}^p y_{t-i} B_i + u_t
  Y_init <- as.matrix(Y_init)
  innov <- as.matrix(innov)
  Tn <- nrow(Y_init)
  k <- ncol(Y_init)
  P <- est$P
  p <- est$p
  if (Tn <= P) stop("Need T > P for simulation")
  if (!all(dim(innov) == c(Tn - P, k))) stop("innov must be (T-P) x k")

  Ysim <- Y_init
  if (p == 0) {
    for (t in (P + 1):Tn) {
      Ysim[t, ] <- est$c_row + innov[t - P, ]
    }
    return(Ysim)
  }

  for (t in (P + 1):Tn) {
    y_t <- est$c_row
    for (i in 1:p) {
      y_t <- y_t + as.numeric(Ysim[t - i, , drop = FALSE] %*% est$B_blocks[[i]])
    }
    y_t <- y_t + innov[t - P, ]
    Ysim[t, ] <- y_t
  }

  Ysim
}

irf_bands_parametric_mc <- function(Y, est, n_ahead = 20, B = 500, seed = 1) {
  set.seed(seed)
  Y <- as.matrix(Y)
  Tn <- nrow(Y)
  k <- ncol(Y)
  P <- est$P
  cholS <- chol(est$Sigma) # upper-triangular

  irf_draws <- array(NA_real_, dim = c(B, n_ahead + 1, k),
                     dimnames = list(NULL, 0:n_ahead, colnames(Y)))
  kept <- 0L

  for (b in 1:B) {
    innov <- matrix(rnorm((Tn - P) * k), nrow = (Tn - P), ncol = k) %*% cholS
    Ysim <- simulate_var_row(est, Y_init = Y, innov = innov)

    est_b <- tryCatch(estimate_var_fixed(Ysim, P = P, p = est$p, include_const = est$include_const), error = function(e) NULL)
    if (is.null(est_b)) next
    mat_b <- tryCatch(irf_to_ffr_shock(est_b, n_ahead = n_ahead), error = function(e) NULL)
    if (is.null(mat_b)) next

    kept <- kept + 1L
    irf_draws[kept, , ] <- mat_b
  }

  irf_draws <- irf_draws[seq_len(kept), , , drop = FALSE]
  list(draws = irf_draws, kept = kept, B = B)
}

irf_bands_bootstrap <- function(Y, est, n_ahead = 20, B = 500, seed = 2) {
  set.seed(seed)
  Y <- as.matrix(Y)
  Tn <- nrow(Y)
  k <- ncol(Y)
  P <- est$P
  res <- est$E

  irf_draws <- array(NA_real_, dim = c(B, n_ahead + 1, k),
                     dimnames = list(NULL, 0:n_ahead, colnames(Y)))
  kept <- 0L

  for (b in 1:B) {
    idx <- sample.int(nrow(res), size = (Tn - P), replace = TRUE)
    innov <- as.matrix(res[idx, , drop = FALSE])
    Ysim <- simulate_var_row(est, Y_init = Y, innov = innov)

    est_b <- tryCatch(estimate_var_fixed(Ysim, P = P, p = est$p, include_const = est$include_const), error = function(e) NULL)
    if (is.null(est_b)) next
    mat_b <- tryCatch(irf_to_ffr_shock(est_b, n_ahead = n_ahead), error = function(e) NULL)
    if (is.null(mat_b)) next

    kept <- kept + 1L
    irf_draws[kept, , ] <- mat_b
  }

  irf_draws <- irf_draws[seq_len(kept), , , drop = FALSE]
  list(draws = irf_draws, kept = kept, B = B)
}

fevd_ffr <- function(est, n_ahead = 20, shock_var = "ffr") {
  k <- est$k
  if (k != 3) stop("Expected k = 3 variables.")
  var_names <- c("dlog_gdp", "dlog_cpi", "ffr")
  shock_idx <- match(shock_var, var_names)
  if (is.na(shock_idx)) stop("Unknown shock_var: ", shock_var)

  if (est$p == 0) {
    Phi <- array(0, dim = c(n_ahead + 1, k, k))
    Phi[1, , ] <- diag(k)
  } else {
    Phi <- ma_coefs(est$A_list, H = n_ahead)
  }

  # FEVD requested for log levels of GDP/CPI => use cumulated MA rows for the first two variables.
  Psi <- ma_coefs_levels(Phi)
  L <- est$L

  shares <- matrix(NA_real_, nrow = n_ahead + 1, ncol = k)
  colnames(shares) <- var_names

  # For each horizon h, use cumulative sum of squared impacts up to h.
  impact <- array(0, dim = c(n_ahead + 1, k, k))
  for (h in 0:n_ahead) {
    impact[h + 1, , ] <- Psi[h + 1, , ] %*% L
  }

  for (j in 1:k) {
    for (h in 0:n_ahead) {
      denom <- 0
      num <- 0
      for (s in 0:h) {
        row_imp <- impact[s + 1, j, ]
        denom <- denom + sum(row_imp^2)
        num <- num + row_imp[shock_idx]^2
      }
      shares[h + 1, j] <- if (denom > 0) num / denom else NA_real_
    }
  }

  tibble::tibble(
    h = 0:n_ahead,
    log_gdp_share = shares[, "dlog_gdp"],
    log_cpi_share = shares[, "dlog_cpi"],
    ffr_share = shares[, "ffr"]
  ) %>%
    pivot_longer(cols = ends_with("_share"), names_to = "variable", values_to = "share") %>%
    mutate(
      variable = dplyr::recode(
        .data$variable,
        log_gdp_share = "dlog_gdp",
        log_cpi_share = "dlog_cpi",
        ffr_share = "ffr"
      )
    )
}

irf_quantiles <- function(irf_draws, probs = c(0.05, 0.95)) {
  # irf_draws: B x (H) x K
  q_lo <- apply(irf_draws, c(2, 3), stats::quantile, probs = probs[1], na.rm = TRUE)
  q_hi <- apply(irf_draws, c(2, 3), stats::quantile, probs = probs[2], na.rm = TRUE)
  list(lower = q_lo, upper = q_hi)
}

plot_irfs_with_bands <- function(base_mat, bands_mc, bands_boot, out_path) {
  H <- nrow(base_mat) - 1
  horizons <- 0:H

  base_df <- as.data.frame(base_mat) %>%
    mutate(h = horizons) %>%
    pivot_longer(cols = c("dlog_gdp", "dlog_cpi", "ffr"), names_to = "variable", values_to = "base")

  bands_to_df <- function(bands, method) {
    lo <- as.data.frame(bands$lower) %>% mutate(h = horizons)
    hi <- as.data.frame(bands$upper) %>% mutate(h = horizons)
    lo_long <- pivot_longer(lo, cols = c("dlog_gdp", "dlog_cpi", "ffr"), names_to = "variable", values_to = "lower")
    hi_long <- pivot_longer(hi, cols = c("dlog_gdp", "dlog_cpi", "ffr"), names_to = "variable", values_to = "upper")
    out <- left_join(lo_long, hi_long, by = c("h", "variable"))
    out$method <- method
    out
  }

  rib <- bind_rows(
    bands_to_df(bands_mc, "Parametric MC"),
    bands_to_df(bands_boot, "Bootstrap")
  )

  df <- left_join(rib, base_df, by = c("h", "variable"))

  var_labels <- c(
    dlog_gdp = "log(real GDP) response (cumulated Δlog)",
    dlog_cpi = "log(CPI) response (cumulated Δlog)",
    ffr = "FFR response (level)"
  )

  p <- ggplot(df, aes(x = .data$h)) +
    geom_ribbon(aes(ymin = .data$lower, ymax = .data$upper, fill = .data$method), alpha = 0.22) +
    geom_line(aes(y = .data$base), linewidth = 0.5, color = "black") +
    facet_wrap(~variable, scales = "free_y", labeller = as_labeller(var_labels)) +
    labs(
      title = "IRFs to 1-sd FFR shock (Cholesky), with 90% bands",
      subtitle = "Bands: parametric Monte Carlo vs residual bootstrap (5th/95th percentiles)",
      x = "Horizon (quarters)",
      y = NULL,
      fill = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")

  ggsave(out_path, p, width = 10, height = 5.6, dpi = 160)
  invisible(p)
}

plot_fevd_ffr <- function(fevd_df, out_path) {
  # fevd_df: columns h, variable, share
  if (!all(c("h", "variable", "share") %in% names(fevd_df))) {
    stop("fevd_df must contain columns: h, variable, share")
  }

  var_labels <- c(
    dlog_gdp = "log(real GDP) (cumulated Δlog)",
    dlog_cpi = "log(CPI) (cumulated Δlog)",
    ffr = "FFR"
  )

  p <- ggplot(fevd_df, aes(x = .data$h, y = .data$share)) +
    geom_line(linewidth = 0.5) +
    facet_wrap(~variable, scales = "free_y", labeller = as_labeller(var_labels)) +
    labs(
      title = "Forecast variance decomposition (share from FFR shocks)",
      x = "Horizon (quarters)",
      y = "Share of forecast error variance"
    ) +
    theme_minimal(base_size = 12)

  ggsave(out_path, p, width = 10, height = 4.8, dpi = 160)
  invisible(p)
}

# -----------------------------
# Main
# -----------------------------

data_path <- "hw4_q3.csv"
end_tq <- zoo::as.yearqtr("2008 Q4")
P_max <- 10
n_ahead <- 20
B_mc <- 500
B_boot <- 500

raw <- read_q3_data(data_path)
df <- make_var_dataset(raw, end_tq = end_tq)

Y <- df %>%
  select(dlog_gdp, dlog_cpi, ffr) %>%
  as.data.frame()
Y_mat <- as.matrix(Y)

# -----------------------------
# Part (a): fixed-sample lag selection (p = 0..10) + LaTeX table
# -----------------------------

part_a_out <- "hw4_q3_part_a_output.txt"
part_a_tex <- "hw4_q3_part_a_lag_selection.tex"

sink(part_a_out)
cat("HW4 Question 3 - Part (a)\n")
cat("Working directory:", normalizePath(getwd(), winslash = "/"), "\n")
cat("Data:", data_path, "\n")
cat("Sample end:", as.character(end_tq), "\n")
cat("Observations used (after differencing + NA drop):", nrow(Y_mat), "\n")
cat("Lag grid: p = 0..", P_max, "\n", sep = "")
cat("Fixed effective sample for all p: n = T - ", P_max, "\n\n", sep = "")

sel <- ic_lr_fixed_sample(Y_mat, P = P_max, include_const = TRUE, alpha = 0.05)
tab_a <- lag_selection_table_fixed(sel)

cat("Lag-selection table (IC + sequential LR), fixed sample:\n")
print(tab_a)
cat("\nAIC-selected p:", sel$aic_p, "\n")
cat("BIC-selected p:", sel$bic_p, "\n")
cat("LR-selected p:", sel$lr_p, "\n\n")

latex_a <- to_latex_tabular(
  tab_a,
  caption = "Lag selection for VAR using fixed effective sample size $n = T-10$ (all $p$). Sequential LR tests compare $p-1$ vs $p$.",
  label = "tab:hw4q3_lag_selection",
  digits = 3
)

cat("LaTeX table output:\n\n")
cat(latex_a, "\n")
sink()

writeLines(latex_a, part_a_tex)

# -----------------------------
# Part (b)-(d): estimate VAR with vars::VAR() using AIC-selected lag
# -----------------------------

part_bcd_out <- "hw4_q3_part_bcd_output.txt"
sink(part_bcd_out)
cat("HW4 Question 3 - Parts (b)-(d)\n")
cat("Using AIC-selected lag from Part (a): p* =", sel$aic_p, "\n\n")

p_for_var <- as.integer(sel$aic_p)
if (p_for_var < 1) {
  cat("Note: vars::VAR() requires p >= 1. Using p = 1 for estimation.\n\n")
  p_for_var <- 1L
}

fit <- vars::VAR(Y, p = p_for_var, type = "const")
cat("VAR estimation (vars::VAR) summary:\n")
print(summary(fit))

cat("\nIRFs: Cholesky ordering [dlog_gdp, dlog_cpi, ffr]\n")
cat("Impulse: ffr (1-sd shock), horizons 0..", n_ahead, "\n\n", sep = "")
base_irf <- irf_from_vars_fit(fit, n_ahead = n_ahead)
cat("Baseline (point) IRF matrix (cumulated to log levels for GDP/CPI):\n")
print(base_irf)

cat("\nParametric Monte Carlo bands...\n")
mc <- irf_bands_parametric_mc_vars(Y = Y_mat, fit = fit, n_ahead = n_ahead, B = B_mc, seed = 123)
cat("Kept draws:", mc$kept, "out of", mc$B, "\n")
bands_mc <- irf_quantiles(mc$draws, probs = c(0.05, 0.95))

cat("\nResidual bootstrap bands...\n")
bt <- irf_bands_bootstrap_vars(Y = Y_mat, fit = fit, n_ahead = n_ahead, B = B_boot, seed = 456)
cat("Kept draws:", bt$kept, "out of", bt$B, "\n")
bands_boot <- irf_quantiles(bt$draws, probs = c(0.05, 0.95))

plot_irfs_with_bands(
  base_mat = base_irf,
  bands_mc = bands_mc,
  bands_boot = bands_boot,
  out_path = "hw4_q3_irf_ffrshock.png"
)

fevd_df <- fevd_ffr_from_vars(fit, n_ahead = n_ahead)
plot_fevd_ffr(fevd_df = fevd_df, out_path = "hw4_q3_fevd_ffr.png")

cat("\nSaved outputs:\n")
cat("- Part (a) output:", part_a_out, "\n")
cat("- Part (a) LaTeX table:", part_a_tex, "\n")
cat("- Part (b)-(d) output:", part_bcd_out, "\n")
cat("- Plots: hw4_q3_irf_ffrshock.png, hw4_q3_fevd_ffr.png\n")

cat("\nNote for (iii): A 'price puzzle' (CPI rising after a contractionary policy shock)\n")
cat("can arise from omitted information / inadequate identification; see Sims (1992).\n")
sink()
