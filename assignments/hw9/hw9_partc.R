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
output_dir <- file.path(script_dir, "hw9_partc_output")
data_path <- file.path(script_dir, "hw9_partb_output", "hw9_partb_simulated_data.txt")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

fixed_par <- list(
  alpha = 1 / 3,
  eta = 1,
  delta = 0.025,
  xi = 1,
  beta = 0.99,
  sigma = 1,
  sigma_e2 = 0.25
)

theta_true <- c(rho = 0.9, phi = 5, sigma_eps = 1)
theta_init <- theta_true

observed_df <- read.table(data_path, header = TRUE, sep = "\t")
y_data <- t(as.matrix(observed_df[, c("c_star", "i_star", "y_star", "l_star")]))
T_obs <- ncol(y_data)

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

  list(
    R_ss = R_ss,
    ky = ky,
    iy = iy,
    cy = cy,
    Y_ss = Y_ss,
    K_ss = K_ss,
    I_ss = I_ss,
    C_ss = C_ss,
    L_ss = L_ss
  )
}

compute_linear_objects <- function(par) {
  A_l <- 1 + 1 / par$eta
  den <- par$alpha + 1 / par$eta

  list(
    A_l = A_l,
    gk = par$alpha * A_l / den,
    gc = - (1 - par$alpha) * par$sigma / den,
    gz = A_l / den
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

solve_reduced_form <- function(par, ss, lin, warm_start = NULL, tol = 1e-8) {
  starts <- list(
    c(0.95, 0.05, 0.02, 0.50),
    c(0.90, 0.10, 0.05, 1.00),
    c(0.98, 0.02, 0.01, 0.20)
  )

  if (!is.null(warm_start)) {
    starts <- c(list(warm_start), starts)
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

    if (abs(fit$par[1]) >= 1) {
      next
    }

    if (is.null(best_fit) || fit$value < best_fit$value) {
      best_fit <- fit
    }
  }

  if (is.null(best_fit) || best_fit$value > tol) {
    stop("No stable reduced-form solution found.")
  }

  list(
    par = best_fit$par,
    residuals = policy_residuals(best_fit$par, par, ss, lin),
    objective = best_fit$value
  )
}

build_state_space <- function(par, lin, sol) {
  p <- sol$par[1]
  q <- sol$par[2]
  r <- sol$par[3]
  s <- sol$par[4]

  h_ck <- r
  h_cz <- s
  h_ik <- (p - (1 - par$delta)) / par$delta
  h_iz <- q / par$delta
  h_yk <- lin$gk + lin$gc * r
  h_yz <- lin$gc * s + lin$gz
  h_lk <- (-par$sigma * h_ck + h_yk) / lin$A_l
  h_lz <- (-par$sigma * h_cz + h_yz) / lin$A_l

  H <- rbind(
    c(h_ck, h_cz),
    c(h_ik, h_iz),
    c(h_yk, h_yz),
    c(h_lk, h_lz)
  )

  B <- matrix(
    c(
      p, q,
      0, par$rho
    ),
    nrow = 2,
    byrow = TRUE
  )

  Q <- matrix(
    c(
      0, 0,
      0, par$sigma_eps^2
    ),
    nrow = 2,
    byrow = TRUE
  )

  Xi <- par$sigma_e2 * diag(4)

  rownames(H) <- c("c", "i", "y", "l")
  colnames(H) <- c("k_lag", "z")
  rownames(B) <- colnames(B) <- c("k_lag", "z")
  rownames(Q) <- colnames(Q) <- c("k_lag", "z")
  rownames(Xi) <- colnames(Xi) <- c("c", "i", "y", "l")

  list(B = B, H = H, Q = Q, Xi = Xi)
}

pack_theta <- function(theta) {
  setNames(c(
    qlogis(theta["rho"]),
    log(theta["phi"]),
    log(theta["sigma_eps"])
  ), c("rho", "phi", "sigma_eps"))
}

unpack_theta <- function(theta_raw) {
  setNames(c(
    rho = plogis(theta_raw[1]),
    phi = exp(theta_raw[2]),
    sigma_eps = exp(theta_raw[3])
  ), c("rho", "phi", "sigma_eps"))
}

numeric_hessian <- function(fn, x, step_scale = 1e-4) {
  n <- length(x)
  H <- matrix(0, nrow = n, ncol = n)
  f0 <- fn(x)

  steps <- pmax(step_scale, step_scale * abs(x))

  for (i in seq_len(n)) {
    ei <- rep(0, n)
    ei[i] <- steps[i]

    f_plus <- fn(x + ei)
    f_minus <- fn(x - ei)
    H[i, i] <- (f_plus - 2 * f0 + f_minus) / (steps[i]^2)

    if (i < n) {
      for (j in (i + 1):n) {
        ej <- rep(0, n)
        ej[j] <- steps[j]

        f_pp <- fn(x + ei + ej)
        f_pm <- fn(x + ei - ej)
        f_mp <- fn(x - ei + ej)
        f_mm <- fn(x - ei - ej)

        H[i, j] <- (f_pp - f_pm - f_mp + f_mm) / (4 * steps[i] * steps[j])
        H[j, i] <- H[i, j]
      }
    }
  }

  rownames(H) <- colnames(H) <- c("rho", "phi", "sigma_eps")
  H
}

solve_stationary_cov <- function(B, Q) {
  n <- nrow(B)
  lhs <- diag(n * n) - kronecker(B, B)
  vecQ <- as.vector(Q)
  vecP <- tryCatch(solve(lhs, vecQ), error = function(e) rep(NA_real_, n * n))

  if (any(!is.finite(vecP))) {
    return(NULL)
  }

  P <- matrix(vecP, nrow = n)
  P <- 0.5 * (P + t(P))
  P
}

kalman_filter_loglik <- function(y, state_space, a0 = NULL, P0 = NULL) {
  B <- state_space$B
  H <- state_space$H
  Q <- state_space$Q
  Xi <- state_space$Xi

  n_state <- nrow(B)
  n_obs <- nrow(H)
  T_obs <- ncol(y)

  if (is.null(a0)) {
    a0 <- rep(0, n_state)
  }

  if (is.null(P0)) {
    P0 <- solve_stationary_cov(B, Q)
  }

  if (is.null(P0) || any(!is.finite(P0))) {
    return(list(loglik = -Inf))
  }

  a_pred <- a0
  P_pred <- P0
  loglik <- 0

  a_pred_store <- matrix(NA_real_, nrow = n_state, ncol = T_obs)
  a_filt_store <- matrix(NA_real_, nrow = n_state, ncol = T_obs)
  v_store <- matrix(NA_real_, nrow = n_obs, ncol = T_obs)

  for (t in seq_len(T_obs)) {
    y_t <- y[, t]
    v_t <- y_t - H %*% a_pred
    F_t <- H %*% P_pred %*% t(H) + Xi
    F_t <- 0.5 * (F_t + t(F_t))

    chol_F <- tryCatch(chol(F_t), error = function(e) NULL)
    if (is.null(chol_F)) {
      return(list(loglik = -Inf))
    }

    logdet_F <- 2 * sum(log(diag(chol_F)))
    F_inv_v <- backsolve(chol_F, forwardsolve(t(chol_F), v_t))
    quad <- as.numeric(crossprod(v_t, F_inv_v))

    loglik <- loglik - 0.5 * (n_obs * log(2 * pi) + logdet_F + quad)

    K_t <- P_pred %*% t(H) %*% chol2inv(chol_F)
    a_filt <- a_pred + K_t %*% v_t
    P_filt <- P_pred - K_t %*% H %*% P_pred
    P_filt <- 0.5 * (P_filt + t(P_filt))

    a_pred_store[, t] <- a_pred
    a_filt_store[, t] <- a_filt
    v_store[, t] <- v_t

    a_pred <- B %*% a_filt
    P_pred <- B %*% P_filt %*% t(B) + Q
    P_pred <- 0.5 * (P_pred + t(P_pred))
  }

  list(
    loglik = loglik,
    a_pred = a_pred_store,
    a_filt = a_filt_store,
    innovations = v_store
  )
}

ss_fixed <- compute_steady_state(fixed_par)
lin_fixed <- compute_linear_objects(fixed_par)

model_cache <- new.env(parent = emptyenv())
model_cache$warm_start <- NULL

state_space_from_theta <- function(theta, cache_env = NULL) {
  par <- modifyList(fixed_par, as.list(theta))
  warm_start <- if (!is.null(cache_env) && exists("warm_start", envir = cache_env)) {
    get("warm_start", envir = cache_env)
  } else {
    NULL
  }

  sol <- solve_reduced_form(par, ss_fixed, lin_fixed, warm_start = warm_start)

  if (!is.null(cache_env)) {
    assign("warm_start", sol$par, envir = cache_env)
  }

  list(
    par = par,
    sol = sol,
    state_space = build_state_space(par, lin_fixed, sol)
  )
}

neg_loglik <- function(theta_raw, y, cache_env = NULL) {
  theta <- unpack_theta(theta_raw)

  if (!is.finite(theta["rho"]) || !is.finite(theta["phi"]) || !is.finite(theta["sigma_eps"])) {
    return(1e12)
  }

  model_obj <- tryCatch(
    state_space_from_theta(theta, cache_env = cache_env),
    error = function(e) NULL
  )

  if (is.null(model_obj)) {
    return(1e12)
  }

  kf_obj <- kalman_filter_loglik(y, model_obj$state_space)
  if (!is.finite(kf_obj$loglik)) {
    return(1e12)
  }

  -kf_obj$loglik
}

theta_init_raw <- pack_theta(theta_init)

fit <- optim(
  par = theta_init_raw,
  fn = neg_loglik,
  y = y_data,
  cache_env = model_cache,
  method = "BFGS",
  hessian = TRUE,
  control = list(reltol = 1e-10, maxit = 1000, trace = 0)
)

theta_hat <- unpack_theta(fit$par)
final_model <- state_space_from_theta(theta_hat)
final_kf <- kalman_filter_loglik(y_data, final_model$state_space)
hessian_raw <- numeric_hessian(
  fn = function(x) neg_loglik(x, y = y_data, cache_env = NULL),
  x = fit$par
)

cov_raw <- tryCatch(solve(hessian_raw), error = function(e) NULL)

if (is.null(cov_raw)) {
  se_theta <- c(rho = NA_real_, phi = NA_real_, sigma_eps = NA_real_)
  cov_theta <- matrix(NA_real_, nrow = 3, ncol = 3)
} else {
  jacobian <- diag(c(
    theta_hat["rho"] * (1 - theta_hat["rho"]),
    theta_hat["phi"],
    theta_hat["sigma_eps"]
  ))
  rownames(jacobian) <- colnames(jacobian) <- c("rho", "phi", "sigma_eps")

  cov_theta <- jacobian %*% cov_raw %*% jacobian
  cov_theta <- 0.5 * (cov_theta + t(cov_theta))
  se_theta <- sqrt(diag(cov_theta))
}

estimate_table <- data.frame(
  parameter = c("rho", "phi", "sigma_eps"),
  true_value = as.numeric(theta_true[c("rho", "phi", "sigma_eps")]),
  initial_value = as.numeric(theta_init[c("rho", "phi", "sigma_eps")]),
  estimate = as.numeric(theta_hat[c("rho", "phi", "sigma_eps")]),
  std_error = as.numeric(se_theta[c("rho", "phi", "sigma_eps")])
)

filtered_state_df <- data.frame(
  t = seq_len(T_obs),
  k_lag_pred = final_kf$a_pred[1, ],
  z_pred = final_kf$a_pred[2, ],
  k_lag_filt = final_kf$a_filt[1, ],
  z_filt = final_kf$a_filt[2, ]
)

innovations_df <- data.frame(
  t = seq_len(T_obs),
  v_c = final_kf$innovations[1, ],
  v_i = final_kf$innovations[2, ],
  v_y = final_kf$innovations[3, ],
  v_l = final_kf$innovations[4, ]
)

write_txt_table(
  estimate_table,
  file.path(output_dir, "hw9_partc_estimates.txt")
)

write_matrix_txt(
  hessian_raw,
  file.path(output_dir, "hw9_partc_hessian_raw_params.txt")
)

write_matrix_txt(
  cov_theta,
  file.path(output_dir, "hw9_partc_cov_theta.txt")
)

write_matrix_txt(
  final_model$state_space$B,
  file.path(output_dir, "hw9_partc_state_B_matrix_hat.txt")
)

write_matrix_txt(
  final_model$state_space$H,
  file.path(output_dir, "hw9_partc_measurement_H_matrix_hat.txt")
)

write_matrix_txt(
  final_model$state_space$Q,
  file.path(output_dir, "hw9_partc_state_Q_matrix_hat.txt")
)

write_txt_table(
  filtered_state_df,
  file.path(output_dir, "hw9_partc_filtered_states.txt")
)

write_txt_table(
  innovations_df,
  file.path(output_dir, "hw9_partc_innovations.txt")
)

summary_text <- capture.output({
  cat("ECON 5345 HW9 Part (c)\n")
  cat("======================\n\n")

  cat("Number of observations:\n")
  print(T_obs)

  cat("\nOptimization convergence code:\n")
  print(fit$convergence)

  cat("\nMaximized log-likelihood:\n")
  print(final_kf$loglik)

  cat("\nParameter estimates and standard errors:\n")
  print(estimate_table, row.names = FALSE)

  cat("\nHessian with respect to transformed parameters:\n")
  print(round(hessian_raw, 8))

  cat("\nCovariance matrix for theta (delta method):\n")
  print(round(cov_theta, 8))

  cat("\nEstimated state transition matrix B:\n")
  print(round(final_model$state_space$B, 8))

  cat("\nEstimated measurement matrix H:\n")
  print(round(final_model$state_space$H, 8))

  cat("\nEstimated state innovation covariance Q:\n")
  print(round(final_model$state_space$Q, 8))

  cat("\nReduced-form coefficients [p, q, r, s] at theta_hat:\n")
  print(round(final_model$sol$par, 8))

  cat("\nResidual check at theta_hat:\n")
  print(signif(final_model$sol$residuals, 6))
})

writeLines(summary_text, con = file.path(output_dir, "hw9_partc_summary.txt"))
