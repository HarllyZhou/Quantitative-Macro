# ============================================================
# ECON 5345 - HW6 - Question 2
# Stand-alone Blanchard-Quah VAR code
# Outputs are saved only as .txt or .png files.
# ============================================================

rm(list = ls())
graphics.off()

# paths and user controls ------------------------------------
get_script_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)

  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
  }

  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    active_path <- tryCatch(
      rstudioapi::getActiveDocumentContext()$path,
      error = function(e) ""
    )
    if (nzchar(active_path)) {
      return(dirname(normalizePath(active_path)))
    }
  }

  getwd()
}

script_dir <- get_script_dir()
data_file <- file.path(script_dir, "hw6_data.csv")
local_r_lib <- file.path(script_dir, "r_packages")
if (!dir.exists(local_r_lib)) {
  dir.create(local_r_lib, recursive = TRUE)
}
.libPaths(c(local_r_lib, .libPaths()))

MAX_LAG_BIC <- 12
H_MAX <- 50
BOOT_RUNS <- 2000
BOOT_SEED <- 12345

# packages ----------------------------------------------------
required_packages <- c("vars")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org", lib = local_r_lib)
  }
}
library(vars)

# helpers -----------------------------------------------------
write_txt_table <- function(x, file_path, row_names = FALSE) {
  utils::write.csv(x, file = file_path, row.names = row_names)
}

prepare_data <- function(file_path) {
  if (!file.exists(file_path)) {
    stop("Data file not found: ", file_path)
  }

  raw <- read.csv(file_path, stringsAsFactors = FALSE)
  required_cols <- c("observation_date", "GDPC1", "UNRATE")
  missing_cols <- setdiff(required_cols, names(raw))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  raw$observation_date <- as.Date(raw$observation_date)
  raw <- raw[order(raw$observation_date), required_cols]

  prepared <- data.frame(
    observation_date = raw$observation_date,
    dy = c(NA_real_, diff(log(raw$GDPC1))),
    z = raw$UNRATE
  )
  prepared <- prepared[complete.cases(prepared), ]
  rownames(prepared) <- NULL

  y_matrix <- as.matrix(prepared[, c("dy", "z")])
  colnames(y_matrix) <- c("dy", "z")

  list(data = prepared, Y = y_matrix)
}

select_bic_lag <- function(Y, lag_max) {
  lag_selection <- vars::VARselect(Y, lag.max = lag_max, type = "const")
  bic_values <- as.numeric(lag_selection$criteria["SC(n)", ])
  bic_table <- data.frame(
    lag = seq_len(length(bic_values)),
    BIC = bic_values
  )

  list(
    selected_lag = as.integer(lag_selection$selection["SC(n)"]),
    bic_table = bic_table,
    full_output = lag_selection
  )
}

relabel_bq_matrices <- function(bq_fit, var_names) {
  # In vars::BQ with variables ordered (dy, z), the long-run matrix is lower triangular.
  # The package shock with zero long-run effect on dy is the demand shock, so we reorder
  # columns to report shocks as [demand, supply]. Flip demand shock sign so that
  # y response > 0 and z response < 0 (identification is up to signs).
  impact_hw <- bq_fit$B[, c(2, 1), drop = FALSE]
  impact_hw[, 1] <- -impact_hw[, 1]
  long_run_hw <- bq_fit$LRIM[, c(2, 1), drop = FALSE]
  long_run_hw[, 1] <- -long_run_hw[, 1]

  rownames(impact_hw) <- var_names
  rownames(long_run_hw) <- var_names
  colnames(impact_hw) <- c("demand", "supply")
  colnames(long_run_hw) <- c("demand", "supply")

  list(A = impact_hw, LR = long_run_hw)
}

compute_structural_irf <- function(var_fit, bq_fit, h_max, var_names) {
  phi_array <- vars::Phi(var_fit, nstep = h_max)
  impact_pkg <- bq_fit$B
  # flip demand shock (col 2) so y response > 0, z response < 0
  impact_pkg[, 2] <- -impact_pkg[, 2]

  irf_array <- array(
    NA_real_,
    dim = c(length(var_names), 2L, h_max + 1L),
    dimnames = list(
      response = var_names,
      shock = c("demand", "supply"),
      horizon = 0:h_max
    )
  )

  for (h in 0:h_max) {
    theta_pkg <- phi_array[, , h + 1] %*% impact_pkg
    irf_array[, , h + 1] <- theta_pkg[, c(2, 1), drop = FALSE]
  }

  irf_array
}

# convert dy response to y = accumulated dy (level response)
accumulate_dy_to_y <- function(irf_array) {
  dy_idx <- which(dimnames(irf_array)$response == "dy")
  if (length(dy_idx) == 0) return(irf_array)
  out <- irf_array
  for (j in 1:2) {
    out[dy_idx, j, ] <- cumsum(out[dy_idx, j, ])
  }
  dimnames(out)$response[dy_idx] <- "y"
  out
}

# FEVD for y (level) and z; uses dy IRF (y forecast error = sum of dy errors)
compute_fevd_from_irf <- function(irf_arr) {
  K <- dim(irf_arr)[1]
  H <- dim(irf_arr)[3] - 1L

  out <- array(NA_real_, dim = c(K, K, H + 1L),
               dimnames = list(
                 variable = dimnames(irf_arr)$response,
                 shock    = dimnames(irf_arr)$shock,
                 horizon  = 0:H
               ))

  for (h in 0:H) {
    for (i in 1:K) {
      contrib <- numeric(K)
      for (j in 1:K) {
        contrib[j] <- sum(irf_arr[i, j, 1:(h + 1)]^2)
      }
      out[i, , h + 1] <- contrib / sum(contrib)
    }
  }

  # report first variable as y (level) not dy
  dy_idx <- which(dimnames(out)$variable == "dy")
  if (length(dy_idx) > 0) dimnames(out)$variable[dy_idx] <- "y"

  out
}

simulate_var_bootstrap <- function(var_fit, Y, residual_draws) {
  p <- var_fit$p
  n_vars <- ncol(Y)
  T_obs <- nrow(Y)

  coef_matrix <- vars::Bcoef(var_fit)
  lag_matrices <- vars::Acoef(var_fit)

  const_vector <- rep(0, n_vars)
  if (ncol(coef_matrix) > n_vars * p) {
    const_vector <- as.numeric(coef_matrix[, (n_vars * p) + 1])
  }

  Y_star <- matrix(NA_real_, nrow = T_obs, ncol = n_vars)
  colnames(Y_star) <- colnames(Y)
  Y_star[1:p, ] <- Y[1:p, ]

  for (t in (p + 1):T_obs) {
    fitted_value <- const_vector
    for (lag in 1:p) {
      fitted_value <- fitted_value + as.numeric(lag_matrices[[lag]] %*% Y_star[t - lag, ])
    }
    Y_star[t, ] <- fitted_value + residual_draws[t - p, ]
  }

  Y_star
}

bootstrap_irf_bands <- function(Y, p, h_max, runs, seed) {
  set.seed(seed)

  base_var <- vars::VAR(Y, p = p, type = "const")
  base_residuals <- residuals(base_var)
  n_effective <- nrow(base_residuals)
  n_vars <- ncol(Y)
  var_names <- colnames(Y)

  boot_store <- array(
    NA_real_,
    dim = c(n_vars, 2L, h_max + 1L, runs),
    dimnames = list(
      response = var_names,
      shock = c("demand", "supply"),
      horizon = 0:h_max,
      draw = seq_len(runs)
    )
  )

  success <- 0L
  tries <- 0L
  max_tries <- max(3L * runs, runs + 50L)

  while (success < runs && tries < max_tries) {
    tries <- tries + 1L
    draw_index <- sample.int(n_effective, size = n_effective, replace = TRUE)
    residual_draws <- base_residuals[draw_index, , drop = FALSE]

    Y_star <- tryCatch(
      simulate_var_bootstrap(base_var, Y, residual_draws),
      error = function(e) NULL
    )
    if (is.null(Y_star)) {
      next
    }

    var_star <- tryCatch(
      vars::VAR(Y_star, p = p, type = "const"),
      error = function(e) NULL
    )
    if (is.null(var_star)) {
      next
    }

    bq_star <- tryCatch(
      vars::BQ(var_star),
      error = function(e) NULL
    )
    if (is.null(bq_star)) {
      next
    }

    irf_star <- tryCatch(
      accumulate_dy_to_y(compute_structural_irf(var_star, bq_star, h_max, var_names)),
      error = function(e) NULL
    )
    if (is.null(irf_star)) {
      next
    }

    success <- success + 1L
    boot_store[, , , success] <- irf_star
  }

  if (success == 0L) {
    stop("Bootstrap failed: no successful replications.")
  }

  if (success < runs) {
    warning(
      sprintf(
        "Only %d successful bootstrap replications out of requested %d.",
        success,
        runs
      )
    )
    boot_store <- boot_store[, , , seq_len(success), drop = FALSE]
  }

  lower <- apply(boot_store, c(1, 2, 3), quantile, probs = 0.025, na.rm = TRUE)
  upper <- apply(boot_store, c(1, 2, 3), quantile, probs = 0.975, na.rm = TRUE)

  # boot_store has (y, z) from accumulate_dy_to_y
  resp_names <- if ("dy" %in% var_names) sub("^dy$", "y", var_names) else var_names
  dimnames(lower) <- dimnames(upper) <- list(
    response = resp_names,
    shock = c("demand", "supply"),
    horizon = 0:h_max
  )

  list(lower = lower, upper = upper, n_success = dim(boot_store)[4])
}

irf_array_to_df <- function(irf_array) {
  h_max <- dim(irf_array)[3] - 1L
  out <- vector("list", length = dim(irf_array)[1] * dim(irf_array)[2])
  idx <- 1L

  for (response_name in dimnames(irf_array)$response) {
    for (shock_name in dimnames(irf_array)$shock) {
      out[[idx]] <- data.frame(
        horizon = 0:h_max,
        response = response_name,
        shock = shock_name,
        irf = as.numeric(irf_array[response_name, shock_name, ])
      )
      idx <- idx + 1L
    }
  }

  do.call(rbind, out)
}

fevd_array_to_df <- function(fevd_array) {
  h_max <- dim(fevd_array)[3] - 1L
  out <- vector("list", length = dim(fevd_array)[1] * dim(fevd_array)[2])
  idx <- 1L

  for (variable_name in dimnames(fevd_array)$variable) {
    for (shock_name in dimnames(fevd_array)$shock) {
      out[[idx]] <- data.frame(
        horizon = 0:h_max,
        variable = variable_name,
        shock = shock_name,
        share = as.numeric(fevd_array[variable_name, shock_name, ])
      )
      idx <- idx + 1L
    }
  }

  do.call(rbind, out)
}

plot_irf_bands <- function(irf_array, lower_array, upper_array, file_png, title_text) {
  panel_order <- list(
    c("y", "demand"),
    c("y", "supply"),
    c("z", "demand"),
    c("z", "supply")
  )

  png(file_png, width = 1100, height = 900, res = 140)
  op <- par(no.readonly = TRUE)
  on.exit({
    par(op)
    dev.off()
  }, add = TRUE)

  par(mfrow = c(2, 2), mar = c(4, 4, 3, 1), oma = c(0, 0, 3, 0))
  h <- 0:(dim(irf_array)[3] - 1L)

  for (item in panel_order) {
    response_name <- item[1]
    shock_name <- item[2]

    y <- as.numeric(irf_array[response_name, shock_name, ])
    y_lower <- as.numeric(lower_array[response_name, shock_name, ])
    y_upper <- as.numeric(upper_array[response_name, shock_name, ])

    y_limits <- range(c(y, y_lower, y_upper), na.rm = TRUE)

    plot(
      h, y,
      type = "n",
      ylim = y_limits,
      xlab = "Horizon",
      ylab = "",
      main = paste(response_name, "response to", shock_name, "shock")
    )
    polygon(
      x = c(h, rev(h)),
      y = c(y_lower, rev(y_upper)),
      border = NA,
      col = gray(0.85)
    )
    lines(h, y, lwd = 2)
    abline(h = 0, lty = 2)
  }

  mtext(title_text, outer = TRUE, cex = 1.2, font = 2)
}

plot_fevd <- function(fevd_array, file_png, title_text) {
  panel_order <- list(
    c("y", "demand"),
    c("y", "supply"),
    c("z", "demand"),
    c("z", "supply")
  )

  png(file_png, width = 1100, height = 900, res = 140)
  op <- par(no.readonly = TRUE)
  on.exit({
    par(op)
    dev.off()
  }, add = TRUE)

  par(mfrow = c(2, 2), mar = c(4, 4, 3, 1), oma = c(0, 0, 3, 0))
  h <- 0:(dim(fevd_array)[3] - 1L)

  for (item in panel_order) {
    variable_name <- item[1]
    shock_name <- item[2]
    y <- as.numeric(fevd_array[variable_name, shock_name, ])

    plot(
      h, y,
      type = "l",
      lwd = 2,
      ylim = c(0, 1),
      xlab = "Horizon",
      ylab = "Share",
      main = paste("FEVD of", variable_name, "from", shock_name, "shock")
    )
    abline(h = c(0, 1), lty = 3)
  }

  mtext(title_text, outer = TRUE, cex = 1.2, font = 2)
}

write_summary_file <- function(file_path, case_name, p, var_fit, A_matrix, LR_matrix, n_boot) {
  summary_text <- capture.output({
    cat("Question 2 case:", case_name, "\n\n")
    cat("Lag length p =", p, "\n\n")
    cat("Reduced-form VAR summary:\n")
    print(summary(var_fit))
    cat("\nContemporaneous impact matrix A (columns: demand, supply):\n")
    print(round(A_matrix, 6))
    cat("\nLong-run impact matrix D(1) (columns: demand, supply):\n")
    print(round(LR_matrix, 6))
    cat("\nBootstrap successful replications:\n")
    print(n_boot)
  })

  writeLines(summary_text, con = file_path)
}

run_case <- function(Y, p, case_name, h_max, boot_runs, boot_seed, out_dir) {
  cat("\n----------------------------------------\n")
  cat("Running case:", case_name, "\n")
  cat("Lag length p =", p, "\n")
  cat("----------------------------------------\n")

  var_fit <- vars::VAR(Y, p = p, type = "const")
  bq_fit <- vars::BQ(var_fit)

  labeled_mats <- relabel_bq_matrices(bq_fit, colnames(Y))
  raw_irf <- compute_structural_irf(var_fit, bq_fit, h_max, colnames(Y))
  irf_point <- accumulate_dy_to_y(raw_irf)
  fevd_point <- compute_fevd_from_irf(raw_irf)
  irf_bands <- bootstrap_irf_bands(Y, p, h_max, boot_runs, boot_seed)

  write_txt_table(
    labeled_mats$A,
    file.path(out_dir, paste0("q2_", case_name, "_A_matrix.txt")),
    row_names = TRUE
  )
  write_txt_table(
    labeled_mats$LR,
    file.path(out_dir, paste0("q2_", case_name, "_long_run_matrix.txt")),
    row_names = TRUE
  )
  write_txt_table(
    irf_array_to_df(irf_point),
    file.path(out_dir, paste0("q2_", case_name, "_irf_point.txt"))
  )
  write_txt_table(
    fevd_array_to_df(fevd_point),
    file.path(out_dir, paste0("q2_", case_name, "_fevd_point.txt"))
  )

  irf_band_table <- irf_array_to_df(irf_point)
  irf_band_table$lower <- as.numeric(irf_bands$lower)
  irf_band_table$upper <- as.numeric(irf_bands$upper)
  write_txt_table(
    irf_band_table,
    file.path(out_dir, paste0("q2_", case_name, "_irf_bands.txt"))
  )

  plot_irf_bands(
    irf_array = irf_point,
    lower_array = irf_bands$lower,
    upper_array = irf_bands$upper,
    file_png = file.path(out_dir, paste0("q2_", case_name, "_irf.png")),
    title_text = paste("Impulse responses -", case_name, sprintf("(p = %d)", p))
  )
  plot_fevd(
    fevd_array = fevd_point,
    file_png = file.path(out_dir, paste0("q2_", case_name, "_fevd.png")),
    title_text = paste("Forecast error variance decomposition -", case_name, sprintf("(p = %d)", p))
  )

  write_summary_file(
    file_path = file.path(out_dir, paste0("q2_", case_name, "_summary.txt")),
    case_name = case_name,
    p = p,
    var_fit = var_fit,
    A_matrix = labeled_mats$A,
    LR_matrix = labeled_mats$LR,
    n_boot = irf_bands$n_success
  )

  list(
    p = p,
    var_fit = var_fit,
    bq_fit = bq_fit,
    A = labeled_mats$A,
    LR = labeled_mats$LR,
    irf = irf_point,
    fevd = fevd_point,
    irf_bands = irf_bands
  )
}

# main --------------------------------------------------------
prepared <- prepare_data(data_file)
Y <- prepared$Y

lag_choice <- select_bic_lag(Y, MAX_LAG_BIC)
p_bic <- lag_choice$selected_lag

write_txt_table(
  lag_choice$bic_table,
  file.path(script_dir, "q2_lag_selection_criteria.txt")
)

cat("BIC-selected lag length:", p_bic, "\n")

baseline_case <- run_case(
  Y = Y,
  p = p_bic,
  case_name = "baseline",
  h_max = H_MAX,
  boot_runs = BOOT_RUNS,
  boot_seed = BOOT_SEED,
  out_dir = script_dir
)

p_triple <- 3L * p_bic
cat("Triple-lag case p =", p_triple, "\n")

triple_lag_case <- run_case(
  Y = Y,
  p = p_triple,
  case_name = "triple_lag",
  h_max = H_MAX,
  boot_runs = BOOT_RUNS,
  boot_seed = BOOT_SEED + 1L,
  out_dir = script_dir
)

comparison_table <- data.frame(
  case = c("baseline", "triple_lag"),
  p = c(baseline_case$p, triple_lag_case$p),
  A_demand_to_dy = c(baseline_case$A["dy", "demand"], triple_lag_case$A["dy", "demand"]),
  A_supply_to_dy = c(baseline_case$A["dy", "supply"], triple_lag_case$A["dy", "supply"]),
  A_demand_to_z = c(baseline_case$A["z", "demand"], triple_lag_case$A["z", "demand"]),
  A_supply_to_z = c(baseline_case$A["z", "supply"], triple_lag_case$A["z", "supply"]),
  LR_demand_to_dy = c(baseline_case$LR["dy", "demand"], triple_lag_case$LR["dy", "demand"]),
  LR_supply_to_dy = c(baseline_case$LR["dy", "supply"], triple_lag_case$LR["dy", "supply"]),
  LR_demand_to_z = c(baseline_case$LR["z", "demand"], triple_lag_case$LR["z", "demand"]),
  LR_supply_to_z = c(baseline_case$LR["z", "supply"], triple_lag_case$LR["z", "supply"])
)

write_txt_table(
  comparison_table,
  file.path(script_dir, "q2_baseline_vs_triple_comparison.txt")
)

cat("\nSaved files:\n")
cat(file.path(script_dir, "q2_lag_selection_criteria.txt"), "\n")
cat(file.path(script_dir, "q2_baseline_summary.txt"), "\n")
cat(file.path(script_dir, "q2_baseline_A_matrix.txt"), "\n")
cat(file.path(script_dir, "q2_baseline_long_run_matrix.txt"), "\n")
cat(file.path(script_dir, "q2_baseline_irf_point.txt"), "\n")
cat(file.path(script_dir, "q2_baseline_irf_bands.txt"), "\n")
cat(file.path(script_dir, "q2_baseline_fevd_point.txt"), "\n")
cat(file.path(script_dir, "q2_baseline_irf.png"), "\n")
cat(file.path(script_dir, "q2_baseline_fevd.png"), "\n")
cat(file.path(script_dir, "q2_triple_lag_summary.txt"), "\n")
cat(file.path(script_dir, "q2_triple_lag_A_matrix.txt"), "\n")
cat(file.path(script_dir, "q2_triple_lag_long_run_matrix.txt"), "\n")
cat(file.path(script_dir, "q2_triple_lag_irf_point.txt"), "\n")
cat(file.path(script_dir, "q2_triple_lag_irf_bands.txt"), "\n")
cat(file.path(script_dir, "q2_triple_lag_fevd_point.txt"), "\n")
cat(file.path(script_dir, "q2_triple_lag_irf.png"), "\n")
cat(file.path(script_dir, "q2_triple_lag_fevd.png"), "\n")
cat(file.path(script_dir, "q2_baseline_vs_triple_comparison.txt"), "\n")