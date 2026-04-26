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
    c(0.98, 0.02, 0.01, 0.20),
    c(0.75, 0.15, 0.10, 0.75)
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
        control = list(reltol = 1e-10, maxit = 1000)
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

solve_stationary_cov <- function(B, Q) {
  n <- nrow(B)
  lhs <- diag(n * n) - kronecker(B, B)
  vecQ <- as.vector(Q)
  vecP <- tryCatch(solve(lhs, vecQ), error = function(e) rep(NA_real_, n * n))

  if (any(!is.finite(vecP))) {
    return(NULL)
  }

  P <- matrix(vecP, nrow = n)
  0.5 * (P + t(P))
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

    a_pred <- B %*% a_filt
    P_pred <- B %*% P_filt %*% t(B) + Q
    P_pred <- 0.5 * (P_pred + t(P_pred))
  }

  list(loglik = loglik)
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

script_dir <- get_script_dir()
output_dir <- file.path(script_dir, "hw10_output")
data_path <- file.path(script_dir, "..", "hw9", "hw9_partb_output", "hw9_partb_simulated_data.txt")

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

observed_df <- read.table(data_path, header = TRUE, sep = "\t")
y_data <- t(as.matrix(observed_df[, c("c_star", "i_star", "y_star", "l_star")]))
T_obs <- ncol(y_data)

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

  list(state_space = build_state_space(par, lin_fixed, sol))
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

set.seed(20260424)

theta_start_raw <- pack_theta(theta_true)
hessian_raw <- numeric_hessian(
  fn = function(x) neg_loglik(x, y = y_data, cache_env = NULL),
  x = theta_start_raw
)

proposal_cov <- tryCatch(-solve(hessian_raw), error = function(e) NULL)
if (is.null(proposal_cov) || any(!is.finite(proposal_cov))) {
  proposal_cov <- tryCatch(solve(hessian_raw), error = function(e) diag(3) * 1e-2)
}
proposal_cov <- 0.5 * (proposal_cov + t(proposal_cov))
proposal_var <- pmax(diag(proposal_cov), 1e-8)
proposal_sd <- sqrt(proposal_var)

n_draws <- 200000L
adapt_until <- 30000L
adapt_every <- 1500L

param_names <- c("rho", "phi", "sigma_eps")

chain_raw <- matrix(NA_real_, nrow = n_draws, ncol = 3)
colnames(chain_raw) <- param_names
chain_theta <- matrix(NA_real_, nrow = n_draws, ncol = 3)
colnames(chain_theta) <- param_names
loglik_path <- rep(NA_real_, n_draws)

accept_total <- rep(0L, 3)
accept_block <- rep(0L, 3)
scale_vec <- rep(1, 3)

current_raw <- theta_start_raw
current_nll <- neg_loglik(current_raw, y = y_data, cache_env = model_cache)

for (draw in seq_len(n_draws)) {
  for (j in seq_len(3)) {
    cand_raw <- current_raw + scale_vec * proposal_sd * stats::rnorm(3)
    cand_nll <- neg_loglik(cand_raw, y = y_data, cache_env = model_cache)
    
    log_alpha <- -cand_nll + current_nll
    if (is.finite(log_alpha) && log(stats::runif(1)) < min(0, log_alpha)) {
      current_raw <- cand_raw
      current_nll <- cand_nll
      accept_total <- accept_total + 1L
    }
  }

  chain_raw[draw, ] <- current_raw
  chain_theta[draw, ] <- unpack_theta(current_raw)
  loglik_path[draw] <- -current_nll
  
  if (draw %% 100L == 0L) {
    cat(
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      "| draw =", draw,
      "of", n_draws,
      "| progress =", sprintf("%.2f%%", 100 * draw / n_draws),
      "| loglik =", sprintf("%.4f", -current_nll),
      "\n"
    )
    flush.console()
  }

  if (draw <= adapt_until && (draw %% adapt_every == 0L)) {
    block_rate <- accept_block / adapt_every
    up_idx <- which(block_rate > 0.40)
    down_idx <- which(block_rate < 0.25)

    if (length(up_idx) > 0) {
      scale_vec[up_idx] <- scale_vec[up_idx] * 1.10
    }
    if (length(down_idx) > 0) {
      scale_vec[down_idx] <- scale_vec[down_idx] * 0.90
    }

    accept_block[] <- 0L
  }
}

accept_rate_total <- accept_total / n_draws

results_df <- data.frame(
  draw = seq_len(n_draws),
  rho = chain_theta[, "rho"],
  phi = chain_theta[, "phi"],
  sigma_eps = chain_theta[, "sigma_eps"],
  loglik = loglik_path
)

accept_df <- data.frame(
  parameter = param_names,
  proposal_sd = proposal_sd,
  final_scale = scale_vec,
  accept_rate = accept_rate_total
)

write_txt_table(results_df, file.path(output_dir, "hw10_mcmc_chain.txt"))
write_txt_table(accept_df, file.path(output_dir, "hw10_mcmc_acceptance.txt"))
write_matrix_txt(hessian_raw, file.path(output_dir, "hw10_hessian_raw_at_init.txt"))
write_matrix_txt(proposal_cov, file.path(output_dir, "hw10_proposal_covariance.txt"))

png(file.path(output_dir, "hw10_chain_plots.png"), width = 1800, height = 1200, res = 150)
par(mfrow = c(3, 1), mar = c(3, 4, 2, 1))
plot(chain_theta[, "rho"], type = "l", col = "steelblue", xlab = "Draw", ylab = "rho", main = "MCMC Chain: rho")
abline(h = theta_true["rho"], col = "red", lty = 2)
plot(chain_theta[, "phi"], type = "l", col = "steelblue", xlab = "Draw", ylab = "phi", main = "MCMC Chain: phi")
abline(h = theta_true["phi"], col = "red", lty = 2)
plot(chain_theta[, "sigma_eps"], type = "l", col = "steelblue", xlab = "Draw", ylab = "sigma_eps", main = "MCMC Chain: sigma_eps")
abline(h = theta_true["sigma_eps"], col = "red", lty = 2)
dev.off()

burn_plot <- 1000L
plot_idx <- seq.int(max(1L, burn_plot), n_draws)

png(file.path(output_dir, "hw10_objective_path.png"), width = 1600, height = 900, res = 150)
plot(
  plot_idx,
  loglik_path[plot_idx],
  type = "l",
  col = "darkgreen",
  xlab = "Draw",
  ylab = "Log-likelihood",
  main = "Log-likelihood Path (post burn-in)"
)
dev.off()

png(file.path(output_dir, "hw10_scatter_vs_objective.png"), width = 1800, height = 1200, res = 150)
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
plot(chain_theta[plot_idx, "rho"], loglik_path[plot_idx], pch = 16, cex = 0.3, col = rgb(0, 0, 1, 0.2), xlab = "rho", ylab = "Log-likelihood", main = "rho vs log-likelihood")
plot(chain_theta[plot_idx, "phi"], loglik_path[plot_idx], pch = 16, cex = 0.3, col = rgb(1, 0.5, 0, 0.2), xlab = "phi", ylab = "Log-likelihood", main = "phi vs log-likelihood")
plot(chain_theta[plot_idx, "sigma_eps"], loglik_path[plot_idx], pch = 16, cex = 0.3, col = rgb(0, 0.5, 0, 0.2), xlab = "sigma_eps", ylab = "Log-likelihood", main = "sigma_eps vs log-likelihood")
plot.new()
text(0.5, 0.5, labels = sprintf("T = %d draws", n_draws), cex = 1.2)
dev.off()

summary_text <- capture.output({
  cat("ECON 5345 HW10\n")
  cat("============\n\n")
  cat("Observations:\n")
  print(T_obs)
  cat("\nMCMC draws:\n")
  print(n_draws)
  cat("\nAcceptance rates:\n")
  print(accept_df, row.names = FALSE)
  cat("\nPosterior means (all draws):\n")
  print(colMeans(chain_theta))
  cat("\nPosterior standard deviations (all draws):\n")
  print(apply(chain_theta, 2, stats::sd))
  cat("\nMax log-likelihood in chain:\n")
  print(max(loglik_path))
})

writeLines(summary_text, con = file.path(output_dir, "hw10_summary.txt"))
