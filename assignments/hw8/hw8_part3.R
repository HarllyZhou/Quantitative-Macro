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

write_matrix_txt <- function(mat, file_path) {
  utils::write.table(
    as.data.frame(mat),
    file = file_path,
    sep = "\t",
    row.names = TRUE,
    col.names = NA,
    quote = FALSE
  )
}

script_dir <- get_script_dir()
output_dir <- file.path(script_dir, "hw8_part3_output")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

HORIZON_IRF <- 40L
HORIZON_FEVD <- 40L

params <- list(
  alpha = 1 / 3,
  eta = 1,
  phi = 1,
  delta = 0.025,
  xi = 1,
  beta = 0.99,
  sigma = 1,
  rho = 0.985,
  sigma_eps = 0.007,
  psi = 0.95,
  sigma_u = 0.004,
  g_share = 0.2
)

compute_steady_state_part3 <- function(par) {
  R_ss <- 1 / par$beta - (1 - par$delta)
  ky <- par$alpha / R_ss
  iy <- par$delta * ky
  gy <- par$g_share
  cy <- 1 - iy - gy

  if (cy <= 0) {
    stop("Steady-state consumption share is not positive.")
  }

  Y_ss <- 1
  K_ss <- ky * Y_ss
  I_ss <- iy * Y_ss
  G_ss <- gy * Y_ss
  C_ss <- cy * Y_ss
  L_ss <- ((1 - par$alpha) / (par$xi * C_ss^par$sigma))^(1 / (1 + 1 / par$eta))
  W_ss <- (1 - par$alpha) * Y_ss / L_ss
  kappa <- G_ss^(1 - par$psi)

  list(
    R_ss = R_ss,
    W_ss = W_ss,
    Y_ss = Y_ss,
    K_ss = K_ss,
    C_ss = C_ss,
    I_ss = I_ss,
    G_ss = G_ss,
    L_ss = L_ss,
    ky = ky,
    iy = iy,
    cy = cy,
    gy = gy,
    kappa = kappa
  )
}

compute_linear_objects_part3 <- function(par) {
  A_l <- 1 + 1 / par$eta
  den <- par$alpha + 1 / par$eta

  list(
    A_l = A_l,
    gk = par$alpha * A_l / den,
    gc = - (1 - par$alpha) * par$sigma / den,
    gz = A_l / den
  )
}

policy_residuals_part3 <- function(x, par, ss, lin) {
  p <- x[1]
  q <- x[2]
  m <- x[3]
  r <- x[4]
  s <- x[5]
  n <- x[6]

  yk <- lin$gk + lin$gc * r
  yz <- lin$gc * s + lin$gz
  yg <- lin$gc * n

  c(
    yk - (ss$cy * r + ss$ky * (p - (1 - par$delta))),
    yz - (ss$cy * s + ss$ky * q),
    yg - (ss$cy * n + ss$ky * m + ss$gy),
    par$sigma * r * (p - 1) - (
      par$beta * ss$R_ss * p * (yk - 1) +
        par$beta * par$phi * p * (p - 1) -
        par$phi * (p - 1)
    ),
    par$sigma * (r * q + s * (par$rho - 1)) - (
      par$beta * ss$R_ss * (q * (yk - 1) + par$rho * yz) +
        par$beta * par$phi * q * (p + par$rho - 1) -
        par$phi * q
    ),
    par$sigma * (r * m + n * (par$psi - 1)) - (
      par$beta * ss$R_ss * (m * (yk - 1) + par$psi * yg) +
        par$beta * par$phi * m * (p + par$psi - 1) -
        par$phi * m
    )
  )
}

solve_policy_part3 <- function(par, tol = 1e-8) {
  ss <- compute_steady_state_part3(par)
  lin <- compute_linear_objects_part3(par)

  starts <- list(
    c(0.95, 0.08, -0.01, 0.53, 0.59, -0.10),
    c(0.95, 0.08, -0.05, 0.53, 0.59, -0.20),
    c(0.90, 0.05, -0.10, 0.50, 0.50, -0.20),
    c(0.98, 0.10, -0.02, 0.60, 0.60, -0.10)
  )

  objective <- function(x) {
    sum(policy_residuals_part3(x, par, ss, lin)^2)
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

    if (fit$value > tol || abs(fit$par[1]) >= 1) {
      next
    }

    if (is.null(best_fit) || fit$value < best_fit$value) {
      best_fit <- fit
    }
  }

  if (is.null(best_fit)) {
    stop("No stable policy solution found for part III.")
  }

  p <- best_fit$par[1]
  q <- best_fit$par[2]
  m <- best_fit$par[3]
  r <- best_fit$par[4]
  s <- best_fit$par[5]
  n <- best_fit$par[6]

  yk <- lin$gk + lin$gc * r
  yz <- lin$gc * s + lin$gz
  yg <- lin$gc * n

  list(
    par = best_fit$par,
    residuals = policy_residuals_part3(best_fit$par, par, ss, lin),
    objective = best_fit$value,
    ss = ss,
    lin = lin,
    policy = list(
      p = p,
      q = q,
      m = m,
      r = r,
      s = s,
      n = n,
      yk = yk,
      yz = yz,
      yg = yg
    )
  )
}

build_state_space_part3 <- function(par, sol) {
  pol <- sol$policy

  A <- matrix(
    c(
      pol$p, pol$q, pol$m,
      0, par$rho, 0,
      0, 0, par$psi
    ),
    nrow = 3,
    byrow = TRUE
  )

  B <- matrix(
    c(
      0, 0,
      1, 0,
      0, 1
    ),
    nrow = 3,
    byrow = TRUE
  )

  H <- rbind(
    c(pol$r, pol$s, pol$n),
    c(pol$yk, pol$yz, pol$yg),
    (c(pol$p, pol$q, pol$m) - c(1 - par$delta, 0, 0)) / par$delta,
    (-par$sigma * c(pol$r, pol$s, pol$n) + c(pol$yk, pol$yz, pol$yg)) / sol$lin$A_l,
    c(1, 0, 0)
  )

  rownames(H) <- c("c", "y", "i", "l", "k")
  colnames(H) <- c("k_lag", "z", "g")
  colnames(B) <- c("tech", "gov")
  rownames(A) <- colnames(A) <- c("k_lag", "z", "g")

  list(A = A, B = B, H = H)
}

compute_irf <- function(state_space, horizon, shock_name) {
  shock_idx <- match(shock_name, colnames(state_space$B))
  if (is.na(shock_idx)) {
    stop("Unknown shock name: ", shock_name)
  }

  x_t <- c(0, 0, 0)
  out <- data.frame(
    horizon = 0:horizon,
    c = NA_real_,
    y = NA_real_,
    l = NA_real_,
    k = NA_real_,
    i = NA_real_,
    z = NA_real_,
    g = NA_real_
  )

  for (h in 0:horizon) {
    v_t <- c(0, 0)
    if (h == 0) {
      v_t[shock_idx] <- 1
    }

    x_t <- state_space$A %*% x_t + state_space$B %*% v_t
    controls <- state_space$H %*% x_t

    out[h + 1L, c("c", "y", "i", "l", "k")] <- as.numeric(controls[c("c", "y", "i", "l", "k"), 1])
    out[h + 1L, "z"] <- x_t[2]
    out[h + 1L, "g"] <- x_t[3]
  }

  out
}

irf_to_long <- function(irf_df, shock_name) {
  vars <- c("c", "y", "l", "k", "i", "z", "g")
  out <- vector("list", length(vars))

  for (j in seq_along(vars)) {
    out[[j]] <- data.frame(
      horizon = irf_df$horizon,
      variable = vars[j],
      shock = shock_name,
      response = irf_df[[vars[j]]]
    )
  }

  do.call(rbind, out)
}

plot_irf_single <- function(irf_df, file_png, title_text) {
  vars_to_plot <- c("c", "y", "l", "k", "i")

  png(file_png, width = 1100, height = 900, res = 140)
  op <- par(no.readonly = TRUE)
  on.exit({
    par(op)
    dev.off()
  }, add = TRUE)

  par(mfrow = c(3, 2), mar = c(4, 4, 3, 1), oma = c(0, 0, 3, 0))

  for (var_name in vars_to_plot) {
    y_vals <- irf_df[[var_name]]
    plot(
      irf_df$horizon,
      y_vals,
      type = "l",
      lwd = 2,
      xlab = "Horizon",
      ylab = "Log deviation",
      main = paste(var_name, "response")
    )
    abline(h = 0, lty = 2)
  }

  plot.new()
  mtext(title_text, outer = TRUE, cex = 1.2, font = 2)
}

plot_irf_comparison <- function(irf_tech, irf_gov, file_png, title_text) {
  vars_to_plot <- c("c", "y", "l", "k", "i")

  png(file_png, width = 1100, height = 900, res = 140)
  op <- par(no.readonly = TRUE)
  on.exit({
    par(op)
    dev.off()
  }, add = TRUE)

  par(mfrow = c(3, 2), mar = c(4, 4, 3, 1), oma = c(0, 0, 3, 0))

  for (var_name in vars_to_plot) {
    y_range <- range(c(irf_tech[[var_name]], irf_gov[[var_name]]))
    plot(
      irf_tech$horizon,
      irf_tech[[var_name]],
      type = "l",
      lwd = 2,
      col = "steelblue",
      ylim = y_range,
      xlab = "Horizon",
      ylab = "Log deviation",
      main = paste(var_name, "response")
    )
    lines(irf_gov$horizon, irf_gov[[var_name]], lwd = 2, col = "firebrick", lty = 2)
    abline(h = 0, lty = 2)
  }

  plot.new()
  legend(
    "center",
    legend = c("technology shock", "government shock"),
    col = c("steelblue", "firebrick"),
    lty = c(1, 2),
    lwd = 2,
    bty = "n"
  )
  mtext(title_text, outer = TRUE, cex = 1.2, font = 2)
}

compute_shock_irf_array <- function(state_space, horizon) {
  vars <- c("c", "y", "l", "i")
  shocks <- c("tech", "gov")

  out <- array(
    NA_real_,
    dim = c(length(vars), length(shocks), horizon + 1L),
    dimnames = list(
      variable = vars,
      shock = shocks,
      horizon = 0:horizon
    )
  )

  for (j in seq_along(shocks)) {
    irf_df <- compute_irf(state_space, horizon, shocks[j])
    for (i in seq_along(vars)) {
      out[i, j, ] <- irf_df[[vars[i]]]
    }
  }

  out
}

compute_fevd <- function(shock_irf, shock_sds) {
  vars <- dimnames(shock_irf)$variable
  shocks <- dimnames(shock_irf)$shock
  horizon_vals <- as.integer(dimnames(shock_irf)$horizon)

  out <- array(
    NA_real_,
    dim = c(length(vars), length(shocks), length(horizon_vals)),
    dimnames = list(
      variable = vars,
      shock = shocks,
      horizon = horizon_vals
    )
  )

  for (i in seq_along(vars)) {
    for (h in seq_along(horizon_vals)) {
      contrib <- numeric(length(shocks))
      for (j in seq_along(shocks)) {
        contrib[j] <- sum(shock_irf[i, j, 1:h]^2) * shock_sds[j]^2
      }
      out[i, , h] <- contrib / sum(contrib)
    }
  }

  out
}

fevd_to_long <- function(fevd_array) {
  vars <- dimnames(fevd_array)$variable
  horizon_vals <- as.integer(dimnames(fevd_array)$horizon)

  out <- list()
  idx <- 1L

  for (var_name in vars) {
    tmp <- data.frame(
      horizon = horizon_vals,
      variable = var_name,
      tech_share = as.numeric(fevd_array[var_name, "tech", ]),
      gov_share = as.numeric(fevd_array[var_name, "gov", ])
    )
    out[[idx]] <- tmp
    idx <- idx + 1L
  }

  do.call(rbind, out)
}

plot_fevd <- function(fevd_array, file_png, title_text) {
  vars <- dimnames(fevd_array)$variable
  horizon_vals <- as.integer(dimnames(fevd_array)$horizon)

  png(file_png, width = 1100, height = 900, res = 140)
  op <- par(no.readonly = TRUE)
  on.exit({
    par(op)
    dev.off()
  }, add = TRUE)

  par(mfrow = c(2, 2), mar = c(4, 4, 3, 1), oma = c(0, 0, 3, 0))

  for (var_name in vars) {
    plot(
      horizon_vals,
      fevd_array[var_name, "tech", ],
      type = "l",
      lwd = 2,
      col = "steelblue",
      ylim = c(0, 1),
      xlab = "Horizon",
      ylab = "Share",
      main = paste("FEVD of", var_name)
    )
    lines(horizon_vals, fevd_array[var_name, "gov", ], lwd = 2, col = "firebrick", lty = 2)
    abline(h = c(0, 1), lty = 3)
  }

  mtext(title_text, outer = TRUE, cex = 1.2, font = 2)
}

solution <- solve_policy_part3(params)
state_space <- build_state_space_part3(params, solution)

irf_gov <- compute_irf(state_space, HORIZON_IRF, "gov")
irf_tech <- compute_irf(state_space, HORIZON_IRF, "tech")

write_txt_table(
  irf_gov,
  file.path(output_dir, "hw8_part3_j_gov_irf.txt")
)
write_txt_table(
  irf_tech,
  file.path(output_dir, "hw8_part3_j_tech_irf.txt")
)
write_txt_table(
  irf_to_long(irf_gov, "gov"),
  file.path(output_dir, "hw8_part3_j_gov_irf_long.txt")
)
write_txt_table(
  irf_to_long(irf_tech, "tech"),
  file.path(output_dir, "hw8_part3_j_tech_irf_long.txt")
)

plot_irf_single(
  irf_gov,
  file.path(output_dir, "hw8_part3_j_gov_irf.png"),
  "Part (j): IRFs to a unit government spending shock"
)
plot_irf_single(
  irf_tech,
  file.path(output_dir, "hw8_part3_j_tech_irf.png"),
  "Part (j): IRFs to a unit technology shock"
)
plot_irf_comparison(
  irf_tech,
  irf_gov,
  file.path(output_dir, "hw8_part3_j_irf_comparison.png"),
  "Part (j): Technology shock versus government shock"
)

shock_irf <- compute_shock_irf_array(state_space, HORIZON_FEVD)
shock_sds <- c(tech = params$sigma_eps, gov = params$sigma_u)
fevd_array <- compute_fevd(shock_irf, shock_sds)
fevd_long <- fevd_to_long(fevd_array)

write_txt_table(
  fevd_long,
  file.path(output_dir, "hw8_part3_k_fevd.txt")
)
write_matrix_txt(
  state_space$A,
  file.path(output_dir, "hw8_part3_state_A_matrix.txt")
)
write_matrix_txt(
  state_space$B,
  file.path(output_dir, "hw8_part3_state_B_matrix.txt")
)
write_matrix_txt(
  state_space$H,
  file.path(output_dir, "hw8_part3_measurement_H_matrix.txt")
)

plot_fevd(
  fevd_array,
  file.path(output_dir, "hw8_part3_k_fevd.png"),
  "Part (k): FEVD from technology and government spending shocks"
)

summary_text <- capture.output({
  cat("ECON 5345 HW8 Part III\n")
  cat("======================\n\n")
  cat("Parameters:\n")
  print(unlist(params))

  cat("\nSteady state with G/Y = 0.2:\n")
  print(round(unlist(solution$ss), 6))

  cat("\nImplied kappa:\n")
  print(round(solution$ss$kappa, 6))

  cat("\nPolicy coefficients for:\n")
  cat("  k_t = p * k_{t-1} + q * z_t + m * g_t\n")
  cat("  c_t = r * k_{t-1} + s * z_t + n * g_t\n\n")
  print(round(solution$par, 6))

  cat("\nResidual check:\n")
  print(signif(solution$residuals, 6))

  cat("\nState-space matrix A in x_t = A x_{t-1} + B v_t:\n")
  print(round(state_space$A, 6))

  cat("\nShock-loading matrix B:\n")
  print(round(state_space$B, 6))

  cat("\nMeasurement matrix H for [c, y, i, l, k]:\n")
  print(round(state_space$H, 6))

  cat("\nFirst 10 horizons of government-shock IRFs:\n")
  print(round(irf_gov[1:11, c("horizon", "c", "y", "l", "k", "i")], 6), row.names = FALSE)

  cat("\nFirst 10 horizons of FEVD shares:\n")
  print(subset(fevd_long, horizon <= 10), row.names = FALSE)
})

writeLines(summary_text, con = file.path(output_dir, "hw8_part3_summary.txt"))

cat("Saved output files to:\n")
cat(output_dir, "\n")
