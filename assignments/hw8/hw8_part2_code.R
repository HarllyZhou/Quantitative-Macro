rm(list = ls())
graphics.off()

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

install_if_missing <- function(pkgs, lib_path) {
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, repos = "https://cloud.r-project.org", lib = lib_path)
    }
  }
}

modify_params <- function(base, ...) {
  modifyList(base, list(...))
}

script_dir <- get_script_dir()
output_dir <- file.path(script_dir, "hw8_part2_output")
local_r_lib <- file.path(script_dir, "r_packages")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
if (!dir.exists(local_r_lib)) {
  dir.create(local_r_lib, recursive = TRUE)
}

.libPaths(c(local_r_lib, .libPaths()))

HORIZON <- 40L
SIM_T <- 20000L
BURNIN <- 1000L
SIM_SEED <- 12345L
IRF_SHOCK <- 1

baseline_params <- list(
  alpha = 1 / 3,
  eta = 1,
  phi = 1,
  delta = 0.025,
  xi = 1,
  beta = 0.99,
  sigma = 1,
  rho = 0.985,
  sigma_eps = 0.007
)

alt_cases <- list(
  baseline = baseline_params,
  eta_0_5 = modify_params(baseline_params, eta = 0.5),
  eta_10 = modify_params(baseline_params, eta = 10),
  phi_0 = modify_params(baseline_params, phi = 0),
  phi_10 = modify_params(baseline_params, phi = 10),
  sigma_0_1 = modify_params(baseline_params, sigma = 0.1),
  sigma_10 = modify_params(baseline_params, sigma = 10),
  rho_0_5 = modify_params(baseline_params, rho = 0.5),
  rho_0_9999 = modify_params(baseline_params, rho = 0.9999)
)

compute_steady_state <- function(par) {
  R_ss <- 1 / par$beta - (1 - par$delta)
  ky <- par$alpha / R_ss
  iy <- par$delta * ky
  cy <- 1 - iy

  Y_ss <- 1
  K_ss <- ky * Y_ss
  I_ss <- iy * Y_ss
  C_ss <- cy * Y_ss
  L_ss <- ((1 - par$alpha) / (par$xi * C_ss^par$sigma))^(1 / (1 + 1 / par$eta))
  W_ss <- (1 - par$alpha) * Y_ss / L_ss

  list(
    R_ss = R_ss,
    W_ss = W_ss,
    K_ss = K_ss,
    Y_ss = Y_ss,
    C_ss = C_ss,
    I_ss = I_ss,
    L_ss = L_ss,
    ky = ky,
    iy = iy,
    cy = cy
  )
}

compute_linear_coeffs <- function(par) {
  A <- 1 + 1 / par$eta
  den <- par$alpha + 1 / par$eta

  list(
    A = A,
    gk = par$alpha * A / den,
    gc = - (1 - par$alpha) * par$sigma / den,
    gz = A / den
  )
}

policy_residuals <- function(x, par, ss, lin) {
  p <- x[1]
  q <- x[2]
  r <- x[3]
  s <- x[4]

  c(
    lin$gk + lin$gc * r - ss$cy * r - ss$ky * (p - (1 - par$delta)),
    lin$gc * s + lin$gz - ss$cy * s - ss$ky * q,
    par$sigma * r * (p - 1) - (
      par$beta * ss$R_ss * p * (lin$gk + lin$gc * r - 1) +
        par$beta * par$phi * p * (p - 1) -
        par$phi * (p - 1)
    ),
    par$sigma * (r * q + s * (par$rho - 1)) - (
      par$beta * ss$R_ss * (
        q * (lin$gk - 1 + lin$gc * r) +
          lin$gc * s * par$rho +
          lin$gz * par$rho
      ) +
        par$beta * par$phi * q * (p + par$rho - 1) -
        par$phi * q
    )
  )
}

solve_policy <- function(par, warm_start = NULL, seed = 42L, n_random = 50L, tol = 1e-8) {
  ss <- compute_steady_state(par)
  lin <- compute_linear_coeffs(par)

  starts <- list(
    c(0.95, 0.05, 0.02, 0.50),
    c(0.90, 0.10, 0.05, 1.00),
    c(0.98, 0.02, 0.01, 0.20),
    c(0.75, 0.15, 0.10, 0.75),
    c(0.60, 0.25, 0.10, 0.10)
  )

  if (!is.null(warm_start)) {
    starts <- c(list(warm_start), starts)
  }

  set.seed(seed)
  for (i in seq_len(n_random)) {
    starts[[length(starts) + 1L]] <- c(
      runif(1, 0.50, 1.05),
      runif(1, -0.40, 0.40),
      runif(1, -0.50, 1.50),
      runif(1, -1.00, 3.00)
    )
  }

  objective <- function(x) {
    sum(policy_residuals(x, par, ss, lin)^2)
  }

  best_fit <- NULL
  for (start in starts) {
    fit <- tryCatch(
      optim(
        par = start,
        fn = objective,
        method = "BFGS",
        control = list(reltol = 1e-15, maxit = 10000)
      ),
      error = function(e) NULL
    )

    if (is.null(fit)) {
      next
    }

    stable <- abs(fit$par[1]) < 1
    if (!stable || fit$value > tol) {
      next
    }

    if (is.null(best_fit) || fit$value < best_fit$value) {
      best_fit <- fit
    }
  }

  if (is.null(best_fit)) {
    stop("No stable policy-rule solution found for this parameterization.")
  }

  list(
    par = best_fit$par,
    residuals = policy_residuals(best_fit$par, par, ss, lin),
    objective = best_fit$value,
    ss = ss,
    lin = lin
  )
}

compute_controls <- function(k_prev, z_t, par, sol) {
  p <- sol$par[1]
  q <- sol$par[2]
  r <- sol$par[3]
  s <- sol$par[4]

  c_t <- r * k_prev + s * z_t
  k_t <- p * k_prev + q * z_t
  y_t <- sol$ss$cy * c_t + sol$ss$ky * (k_t - (1 - par$delta) * k_prev)
  i_t <- (k_t - (1 - par$delta) * k_prev) / par$delta
  l_t <- (- par$sigma * c_t + y_t) / sol$lin$A
  r_t <- y_t - k_prev
  w_t <- y_t - l_t

  c(
    k = k_t,
    c = c_t,
    y = y_t,
    i = i_t,
    l = l_t,
    r = r_t,
    w = w_t
  )
}

compute_irf <- function(par, sol, horizon = HORIZON, shock = IRF_SHOCK) {
  out <- data.frame(
    horizon = 0:horizon,
    z = NA_real_,
    c = NA_real_,
    i = NA_real_,
    y = NA_real_,
    k = NA_real_,
    l = NA_real_,
    r = NA_real_,
    w = NA_real_
  )

  k_prev <- 0
  z_prev <- 0

  for (t in 0:horizon) {
    eps_t <- if (t == 0) shock else 0
    z_t <- par$rho * z_prev + eps_t
    ctrls <- compute_controls(k_prev, z_t, par, sol)

    out[t + 1L, "z"] <- z_t
    out[t + 1L, names(ctrls)] <- unname(ctrls)

    k_prev <- unname(ctrls[["k"]])
    z_prev <- z_t
  }

  out
}

simulate_model <- function(par, sol, T = SIM_T, burnin = BURNIN, seed = SIM_SEED) {
  TT <- T + burnin
  set.seed(seed)
  eps <- rnorm(TT, mean = 0, sd = par$sigma_eps)

  out <- data.frame(
    t = seq_len(TT),
    z = NA_real_,
    c = NA_real_,
    i = NA_real_,
    y = NA_real_,
    k = NA_real_,
    l = NA_real_,
    r = NA_real_,
    w = NA_real_
  )

  k_prev <- 0
  z_prev <- 0

  for (t in seq_len(TT)) {
    z_t <- par$rho * z_prev + eps[t]
    ctrls <- compute_controls(k_prev, z_t, par, sol)

    out[t, "z"] <- z_t
    out[t, names(ctrls)] <- unname(ctrls)

    k_prev <- unname(ctrls[["k"]])
    z_prev <- z_t
  }

  out[(burnin + 1L):TT, , drop = FALSE]
}

compute_growth_data <- function(df) {
  growth_df <- data.frame(
    dc = diff(df$c),
    dy = diff(df$y),
    dl = diff(df$l),
    di = diff(df$i)
  )

  growth_df
}

cov_matrix_to_table <- function(cov_mat) {
  out <- list()
  idx <- 1L

  for (i in seq_len(nrow(cov_mat))) {
    for (j in i:ncol(cov_mat)) {
      out[[idx]] <- data.frame(
        variable_1 = rownames(cov_mat)[i],
        variable_2 = colnames(cov_mat)[j],
        covariance = cov_mat[i, j]
      )
      idx <- idx + 1L
    }
  }

  do.call(rbind, out)
}

compute_moments <- function(growth_df) {
  var_vec <- apply(growth_df, 2, var)
  cov_mat <- cov(growth_df)

  list(
    variance_table = data.frame(
      variable = names(var_vec),
      variance = as.numeric(var_vec)
    ),
    covariance_matrix = cov_mat,
    covariance_table = cov_matrix_to_table(cov_mat)
  )
}

write_matrix_csv <- function(mat, file_path) {
  utils::write.table(
    as.data.frame(mat),
    file = file_path,
    sep = "\t",
    row.names = TRUE,
    col.names = NA,
    quote = FALSE
  )
}

write_txt_table <- function(df, file_path, row_names = FALSE) {
  utils::write.table(
    df,
    file = file_path,
    sep = "\t",
    row.names = row_names,
    col.names = TRUE,
    quote = FALSE
  )
}

reshape_irf_list <- function(irf_list, case_names) {
  out <- vector("list", length(case_names))

  for (i in seq_along(case_names)) {
    tmp <- irf_list[[case_names[i]]]
    tmp$case <- case_names[i]
    out[[i]] <- tmp
  }

  do.call(rbind, out)
}

plot_single_irf <- function(irf_df, file_png, title_text) {
  vars_to_plot <- c("c", "i", "y", "k", "l")

  png(file_png, width = 1100, height = 900, res = 140)
  op <- par(no.readonly = TRUE)
  on.exit({
    par(op)
    dev.off()
  }, add = TRUE)

  par(mfrow = c(3, 2), mar = c(4, 4, 3, 1), oma = c(0, 0, 3, 0))

  for (var_name in vars_to_plot) {
    y_vals <- irf_df[[var_name]]
    finite_vals <- y_vals[is.finite(y_vals)]
    y_range <- if (length(finite_vals) > 0) range(finite_vals) else c(-1, 1)
    plot(
      irf_df$horizon, y_vals,
      type = "l",
      lwd = 2,
      ylim = y_range,
      xlab = "Horizon",
      ylab = "Log deviation",
      main = paste(var_name, "response")
    )
    abline(h = 0, lty = 2)
  }

  plot.new()
  mtext(title_text, outer = TRUE, cex = 1.2, font = 2)
}

plot_irf_comparison <- function(irf_list, case_names, labels, file_png, title_text) {
  vars_to_plot <- c("c", "i", "y", "k", "l")
  cols <- c("black", "firebrick", "steelblue")
  ltys <- c(1, 2, 3)

  png(file_png, width = 1100, height = 900, res = 140)
  op <- par(no.readonly = TRUE)
  on.exit({
    par(op)
    dev.off()
  }, add = TRUE)

  par(mfrow = c(3, 2), mar = c(4, 4, 3, 1), oma = c(0, 0, 3, 0))

  for (var_name in vars_to_plot) {
    all_vals <- unlist(lapply(case_names, function(nm) irf_list[[nm]][[var_name]]))
    finite_vals <- all_vals[is.finite(all_vals)]
    y_range <- if (length(finite_vals) > 0) range(finite_vals) else c(-1, 1)
    plot(
      irf_list[[case_names[1]]]$horizon,
      irf_list[[case_names[1]]][[var_name]],
      type = "l",
      lwd = 2,
      lty = ltys[1],
      col = cols[1],
      ylim = y_range,
      xlab = "Horizon",
      ylab = "Log deviation",
      main = paste(var_name, "response")
    )

    if (length(case_names) > 1) {
      for (j in 2:length(case_names)) {
        lines(
          irf_list[[case_names[j]]]$horizon,
          irf_list[[case_names[j]]][[var_name]],
          lwd = 2,
          lty = ltys[j],
          col = cols[j]
        )
      }
    }

    abline(h = 0, lty = 2)
  }

  plot.new()
  legend(
    "center",
    legend = labels,
    col = cols[seq_along(labels)],
    lty = ltys[seq_along(labels)],
    lwd = 2,
    bty = "n",
    cex = 1
  )
  mtext(title_text, outer = TRUE, cex = 1.2, font = 2)
}

try_download_data_analogues <- function(start_date = "1954-01-01") {
  ok <- tryCatch({
    install_if_missing("quantmod", local_r_lib)
    library(quantmod)
    TRUE
  }, error = function(e) FALSE)

  if (!ok) {
    return(NULL)
  }

  symbols <- c(
    y = "GDPC1",
    c = "DPCERC1Q225SBEA",
    i = "GPDIC1",
    l = "HOANBS"
  )

  fetched <- tryCatch(
    lapply(symbols, function(sym) quantmod::getSymbols(sym, src = "FRED", auto.assign = FALSE)),
    error = function(e) NULL
  )

  if (is.null(fetched)) {
    return(NULL)
  }

  merged <- do.call(merge, fetched)
  colnames(merged) <- names(symbols)
  merged <- merged[paste0(start_date, "/")]
  merged <- na.omit(merged)

  growth <- na.omit(diff(log(merged)))
  growth_df <- data.frame(
    date = zoo::index(growth),
    dc = as.numeric(growth[, "c"]),
    dy = as.numeric(growth[, "y"]),
    dl = as.numeric(growth[, "l"]),
    di = as.numeric(growth[, "i"])
  )

  growth_df
}

compare_model_data_moments <- function(model_moments, data_moments) {
  model_var <- model_moments$variance_table
  data_var <- data_moments$variance_table

  merged_var <- merge(
    model_var,
    data_var,
    by = "variable",
    suffixes = c("_model", "_data"),
    all = TRUE
  )

  model_cov <- model_moments$covariance_table
  data_cov <- data_moments$covariance_table

  merged_cov <- merge(
    model_cov,
    data_cov,
    by = c("variable_1", "variable_2"),
    suffixes = c("_model", "_data"),
    all = TRUE
  )

  list(variance = merged_var, covariance = merged_cov)
}

save_summary <- function(file_path, solutions, model_moments, data_moments = NULL) {
  txt <- capture.output({
    cat("ECON 5345 HW8 Part II\n")
    cat("=====================\n\n")
    cat("Baseline policy-rule coefficients for:\n")
    cat("  k_t = p * k_{t-1} + q * z_t\n")
    cat("  c_t = r * k_{t-1} + s * z_t\n\n")
    print(round(solutions$baseline$par, 6))
    cat("\nResidual check:\n")
    print(signif(solutions$baseline$residuals, 6))

    cat("\nBaseline steady state objects:\n")
    print(round(unlist(solutions$baseline$ss), 6))

    cat("\nModel variances of growth rates:\n")
    print(model_moments$variance_table, row.names = FALSE)

    cat("\nModel covariance matrix of growth rates:\n")
    print(round(model_moments$covariance_matrix, 8))

    if (!is.null(data_moments)) {
      cat("\nData variances of growth rates:\n")
      print(data_moments$variance_table, row.names = FALSE)

      cat("\nData covariance matrix of growth rates:\n")
      print(round(data_moments$covariance_matrix, 8))
    } else {
      cat("\nData analogues were not downloaded.\n")
    }
  })

  writeLines(txt, con = file_path)
}

solutions <- list()
warm_start <- NULL
for (nm in names(alt_cases)) {
  solutions[[nm]] <- solve_policy(
    par = alt_cases[[nm]],
    warm_start = warm_start
  )
  warm_start <- solutions[[nm]]$par
}

irfs <- list()
for (nm in names(alt_cases)) {
  irfs[[nm]] <- compute_irf(
    par = alt_cases[[nm]],
    sol = solutions[[nm]],
    horizon = HORIZON,
    shock = IRF_SHOCK
  )
}

baseline_irf <- irfs$baseline
write_txt_table(
  baseline_irf,
  file_path = file.path(output_dir, "hw8_part2_baseline_irf.txt")
)

plot_single_irf(
  irf_df = baseline_irf,
  file_png = file.path(output_dir, "hw8_part2_baseline_irf.png"),
  title_text = "Part (f): Baseline impulse responses to a unit technology shock"
)

comparison_groups <- list(
  eta = c("baseline", "eta_0_5", "eta_10"),
  phi = c("baseline", "phi_0", "phi_10"),
  sigma = c("baseline", "sigma_0_1", "sigma_10"),
  rho = c("baseline", "rho_0_5", "rho_0_9999")
)

comparison_labels <- list(
  eta = c("baseline", "eta = 0.5", "eta = 10"),
  phi = c("baseline", "phi = 0", "phi = 10"),
  sigma = c("baseline", "sigma = 0.1", "sigma = 10"),
  rho = c("baseline", "rho = 0.5", "rho = 0.9999")
)

for (grp in names(comparison_groups)) {
  case_names <- comparison_groups[[grp]]

  write_txt_table(
    reshape_irf_list(irfs, case_names),
    file_path = file.path(output_dir, paste0("hw8_part2_", grp, "_comparison_irf.txt"))
  )

  plot_irf_comparison(
    irf_list = irfs,
    case_names = case_names,
    labels = comparison_labels[[grp]],
    file_png = file.path(output_dir, paste0("hw8_part2_", grp, "_comparison_irf.png")),
    title_text = paste("Part (g): IRF comparison for", grp)
  )
}

sim_baseline <- simulate_model(
  par = baseline_params,
  sol = solutions$baseline,
  T = SIM_T,
  burnin = BURNIN,
  seed = SIM_SEED
)

model_growth <- compute_growth_data(sim_baseline)
model_moments <- compute_moments(model_growth)

write_txt_table(
  model_moments$variance_table,
  file_path = file.path(output_dir, "hw8_part2_model_variances.txt")
)
write_txt_table(
  model_moments$covariance_table,
  file_path = file.path(output_dir, "hw8_part2_model_covariances.txt")
)
write_matrix_csv(
  model_moments$covariance_matrix,
  file.path(output_dir, "hw8_part2_model_covariance_matrix.txt")
)

data_growth <- try_download_data_analogues()
data_moments <- NULL

if (!is.null(data_growth)) {
  write_txt_table(
    data_growth,
    file_path = file.path(output_dir, "hw8_part2_data_growth_rates.txt")
  )

  data_moments <- compute_moments(data_growth[, c("dc", "dy", "dl", "di")])

  write_txt_table(
    data_moments$variance_table,
    file_path = file.path(output_dir, "hw8_part2_data_variances.txt")
  )
  write_txt_table(
    data_moments$covariance_table,
    file_path = file.path(output_dir, "hw8_part2_data_covariances.txt")
  )
  write_matrix_csv(
    data_moments$covariance_matrix,
    file.path(output_dir, "hw8_part2_data_covariance_matrix.txt")
  )

  comparison_tables <- compare_model_data_moments(model_moments, data_moments)
  write_txt_table(
    comparison_tables$variance,
    file_path = file.path(output_dir, "hw8_part2_model_vs_data_variances.txt")
  )
  write_txt_table(
    comparison_tables$covariance,
    file_path = file.path(output_dir, "hw8_part2_model_vs_data_covariances.txt")
  )
}

save_summary(
  file_path = file.path(output_dir, "hw8_part2_summary.txt"),
  solutions = solutions,
  model_moments = model_moments,
  data_moments = data_moments
)

cat("Saved output files to:\n")
cat(output_dir, "\n")
