# econ 5345 hw7 q2

rm(list = ls())
graphics.off()

set.seed(123)

# =========================================================
# True parameters
# =========================================================
alpha_true <- 0.8
theta_true <- 0.4
sigma2_true <- 3
sigma_true <- sqrt(sigma2_true)

sample_sizes <- c(50, 100, 500)
n_rep <- 1000

# =========================================================
# State-space form
# z_t = (y_t, y_{t-1}, e_t, e_{t-1})'
# z_t = A z_{t-1} + B e_t
# y_t = H z_t
# =========================================================
state_space_matrices <- function(alpha, theta, sigma) {
  A <- matrix(c(
    alpha, 0,     theta, 0,
    1,     0,     0,     0,
    0,     0,     0,     0,
    0,     0,     1,     0
  ), nrow = 4, byrow = TRUE)

  B <- matrix(c(1, 0, 1, 0), ncol = 1)
  H <- matrix(c(1, 0, 0, 0), nrow = 1)
  Q <- sigma^2 * (B %*% t(B))
  R <- matrix(0, nrow = 1, ncol = 1)

  list(A = A, B = B, H = H, Q = Q, R = R)
}

# =========================================================
# Simulate ARMA(1,1)
# y_t = alpha y_{t-1} + e_t + theta e_{t-1}
# =========================================================
simulate_arma11 <- function(T, alpha, theta, sigma, burnin = 200) {
  TT <- T + burnin
  e <- rnorm(TT, mean = 0, sd = sigma)
  y <- numeric(TT)

  y[1] <- 0
  for (t in 2:TT) {
    y[t] <- alpha * y[t - 1] + theta * e[t - 1] + e[t]
  }

  y[(burnin + 1):TT]
}

# =========================================================
# Kalman filter negative log-likelihood
# par = (a_raw, t_raw, log_sigma)
# alpha = tanh(a_raw), theta = tanh(t_raw), sigma = exp(log_sigma)
# =========================================================
kalman_nll_arma11 <- function(par, y) {
  alpha <- tanh(par[1])
  theta <- tanh(par[2])
  sigma <- exp(par[3])

  mats <- state_space_matrices(alpha, theta, sigma)
  A <- mats$A
  H <- mats$H
  Q <- mats$Q
  R <- mats$R

  z_pred <- matrix(0, nrow = 4, ncol = 1)
  P_pred <- diag(1e6, 4)

  loglik <- 0
  tiny <- 1e-8

  for (t in 1:length(y)) {
    y_hat <- H %*% z_pred
    v_t <- matrix(y[t], nrow = 1) - y_hat
    F_t <- H %*% P_pred %*% t(H) + R
    F_t[1, 1] <- max(F_t[1, 1], tiny)

    loglik <- loglik - 0.5 * (
      log(2 * pi) +
      log(F_t[1, 1]) +
      v_t[1, 1]^2 / F_t[1, 1]
    )

    K_t <- P_pred %*% t(H) %*% solve(F_t)
    z_upd <- z_pred + K_t %*% v_t
    P_upd <- P_pred - K_t %*% H %*% P_pred

    z_pred <- A %*% z_upd
    P_pred <- A %*% P_upd %*% t(A) + Q
  }

  -loglik
}

# =========================================================
# Estimate by MLE with Kalman filter
# =========================================================
estimate_arma11_kf <- function(y, init_alpha = 0.5, init_theta = 0.2, init_sigma = sd(y)) {
  init_alpha <- max(min(init_alpha, 0.99), -0.99)
  init_theta <- max(min(init_theta, 0.99), -0.99)
  init_sigma <- max(init_sigma, 1e-4)

  init_par <- c(
    atanh(init_alpha),
    atanh(init_theta),
    log(init_sigma)
  )

  fit <- optim(
    par = init_par,
    fn = kalman_nll_arma11,
    y = y,
    method = "BFGS",
    hessian = TRUE,
    control = list(maxit = 2000, reltol = 1e-10)
  )

  alpha_hat <- tanh(fit$par[1])
  theta_hat <- tanh(fit$par[2])
  sigma_hat <- exp(fit$par[3])

  vcov_raw <- tryCatch(solve(fit$hessian), error = function(e) NULL)

  if (is.null(vcov_raw)) {
    se_alpha <- NA_real_
    se_theta <- NA_real_
    se_sigma <- NA_real_
  } else {
    J <- diag(c(
      1 - alpha_hat^2,
      1 - theta_hat^2,
      sigma_hat
    ))
    vcov_hat <- J %*% vcov_raw %*% t(J)
    se_alpha <- sqrt(vcov_hat[1, 1])
    se_theta <- sqrt(vcov_hat[2, 2])
    se_sigma <- sqrt(vcov_hat[3, 3])
  }

  list(
    alpha = alpha_hat,
    theta = theta_hat,
    sigma = sigma_hat,
    sigma2 = sigma_hat^2,
    se_alpha = se_alpha,
    se_theta = se_theta,
    se_sigma = se_sigma,
    loglik = -fit$value,
    convergence = fit$convergence,
    counts = fit$counts,
    raw_fit = fit
  )
}

# =========================================================
# Part (b): one simulation for T = 50, 100, 500
# =========================================================
ys <- vector("list", length(sample_sizes))
names(ys) <- paste0("T", sample_sizes)

for (k in seq_along(sample_sizes)) {
  ys[[k]] <- simulate_arma11(
    T = sample_sizes[k],
    alpha = alpha_true,
    theta = theta_true,
    sigma = sigma_true
  )
}

results_b <- vector("list", length(ys))
names(results_b) <- names(ys)

for (k in seq_along(ys)) {
  results_b[[k]] <- estimate_arma11_kf(
    y = ys[[k]],
    init_alpha = 0.5,
    init_theta = 0.2,
    init_sigma = sd(ys[[k]])
  )
}



sink("hw7_q2_b_output.txt")

cat("\n====================================\n")
cat("Question 2(a)\n")
cat("====================================\n")
cat("State vector:\n")
cat("z_t = (y_t, y_{t-1}, e_t, e_{t-1})'\n\n")
cat("Transition matrix A:\n")
print(state_space_matrices(alpha_true, theta_true, sigma_true)$A)
cat("\nLoading vector B:\n")
print(state_space_matrices(alpha_true, theta_true, sigma_true)$B)
cat("\nObservation matrix H:\n")
print(state_space_matrices(alpha_true, theta_true, sigma_true)$H)

cat("\n====================================\n")
cat("Question 2(b)\n")
cat("====================================\n")

for (nm in names(results_b)) {
  fit <- results_b[[nm]]

  cat("\n------------------------------------\n")
  cat("Sample size:", nm, "\n")
  cat("------------------------------------\n")
  cat("Convergence code:", fit$convergence, "\n")
  cat("Log-likelihood:", round(fit$loglik, 6), "\n\n")
  cat("Estimates:\n")
  cat("alpha   =", round(fit$alpha, 6), "\n")
  cat("theta   =", round(fit$theta, 6), "\n")
  cat("sigma   =", round(fit$sigma, 6), "\n")
  cat("sigma^2 =", round(fit$sigma2, 6), "\n\n")
  cat("Standard errors:\n")
  cat("se(alpha) =", round(fit$se_alpha, 6), "\n")
  cat("se(theta) =", round(fit$se_theta, 6), "\n")
  cat("se(sigma) =", round(fit$se_sigma, 6), "\n")
}

summary_b <- data.frame(
  sample = names(results_b),
  alpha_hat = sapply(results_b, function(x) x$alpha),
  theta_hat = sapply(results_b, function(x) x$theta),
  sigma_hat = sapply(results_b, function(x) x$sigma),
  sigma2_hat = sapply(results_b, function(x) x$sigma2),
  se_alpha = sapply(results_b, function(x) x$se_alpha),
  se_theta = sapply(results_b, function(x) x$se_theta),
  se_sigma = sapply(results_b, function(x) x$se_sigma),
  loglik = sapply(results_b, function(x) x$loglik),
  convergence = sapply(results_b, function(x) x$convergence)
)

cat("\nSummary table for part (b):\n")
print(summary_b, row.names = FALSE)

sink()



# =========================================================
# Part (c): Monte Carlo
# =========================================================
run_mc_one <- function(T, alpha_true, theta_true, sigma_true) {
  y <- simulate_arma11(T = T, alpha = alpha_true, theta = theta_true, sigma = sigma_true)

  fit <- tryCatch(
    estimate_arma11_kf(
      y = y,
      init_alpha = 0.5,
      init_theta = 0.2,
      init_sigma = sd(y)
    ),
    error = function(e) NULL
  )

  if (is.null(fit)) {
    return(c(alpha_hat = NA, theta_hat = NA, sigma_hat = NA, sigma2_hat = NA, convergence = NA))
  }

  c(
    alpha_hat = fit$alpha,
    theta_hat = fit$theta,
    sigma_hat = fit$sigma,
    sigma2_hat = fit$sigma2,
    convergence = fit$convergence
  )
}

mc_results <- vector("list", length(sample_sizes))
names(mc_results) <- paste0("T", sample_sizes)

for (k in seq_along(sample_sizes)) {
  T_now <- sample_sizes[k]
  out <- matrix(NA_real_, nrow = n_rep, ncol = 5)
  colnames(out) <- c("alpha_hat", "theta_hat", "sigma_hat", "sigma2_hat", "convergence")

  for (r in 1:n_rep) {
    out[r, ] <- run_mc_one(
      T = T_now,
      alpha_true = alpha_true,
      theta_true = theta_true,
      sigma_true = sigma_true
    )

    if (r %% 100 == 0) {
      cat("Finished T =", T_now, ", replication =", r, "\n")
    }
  }

  mc_results[[k]] <- as.data.frame(out)
}

mc_summary_one <- function(df, alpha_true, theta_true, sigma_true, sigma2_true) {
  df_ok <- df[!is.na(df$alpha_hat) & df$convergence == 0, ]

  stat_fun <- function(est, true) {
    bias <- mean(est - true)
    variance <- var(est)
    mse <- mean((est - true)^2)
    c(bias = bias, variance = variance, mse = mse)
  }

  s_alpha <- stat_fun(df_ok$alpha_hat, alpha_true)
  s_theta <- stat_fun(df_ok$theta_hat, theta_true)
  s_sigma <- stat_fun(df_ok$sigma_hat, sigma_true)
  s_sigma2 <- stat_fun(df_ok$sigma2_hat, sigma2_true)

  data.frame(
    parameter = c("alpha", "theta", "sigma", "sigma2"),
    bias = c(s_alpha["bias"], s_theta["bias"], s_sigma["bias"], s_sigma2["bias"]),
    variance = c(s_alpha["variance"], s_theta["variance"], s_sigma["variance"], s_sigma2["variance"]),
    mse = c(s_alpha["mse"], s_theta["mse"], s_sigma["mse"], s_sigma2["mse"]),
    n_success = nrow(df_ok),
    success_rate = nrow(df_ok) / nrow(df)
  )
}

mc_summaries <- vector("list", length(mc_results))
names(mc_summaries) <- names(mc_results)

for (k in seq_along(mc_results)) {
  mc_summaries[[k]] <- mc_summary_one(
    df = mc_results[[k]],
    alpha_true = alpha_true,
    theta_true = theta_true,
    sigma_true = sigma_true,
    sigma2_true = sigma2_true
  )
}

sink("hw7_q2_c_output.txt")

cat("\n====================================\n")
cat("Question 2(c)\n")
cat("====================================\n")

for (nm in names(mc_summaries)) {
  cat("\n------------------------------------\n")
  cat("Monte Carlo summary:", nm, "\n")
  cat("------------------------------------\n")
  print(mc_summaries[[nm]], row.names = FALSE)
}

# Optional combined table
mc_summary_table <- do.call(
  rbind,
  lapply(names(mc_summaries), function(nm) {
    cbind(sample = nm, mc_summaries[[nm]])
  })
)

cat("\nCombined Monte Carlo summary:\n")
print(mc_summary_table, row.names = FALSE)

sink()