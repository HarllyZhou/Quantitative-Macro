# econ5345 hw2 - q2

rm(list = ls())
library(xtable)
graphics.off()

set.seed(123)

# part (a) -------------------------------------------------------------
T25 <- 25
psi <- rep(0, T25+1)
psi[1] <- 1

for (idx in 1:T25) {
  psi[idx+1] <- 1.2 * 0.8^(idx - 1)
}

psi_25 <- data.frame(
  t = 0:T25,
  psi = psi
)

get_script_dir <- function() {
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    path <- rstudioapi::getActiveDocumentContext()$path
    if (nzchar(path)) return(dirname(path))
  }
  getwd()
}

script_dir <- get_script_dir()
out_png <- file.path(script_dir, "hw2_q2a_irf.png")

png(out_png, width = 900, height = 700, res = 130)
plot(psi_25$t, psi_25$psi, type = "o", pch = 16,
     xlab = "t", ylab = "Impulse response",
     main = "IRF to unit shock in e_0")
abline(h = 0, lty = 3)
dev.off()

cat("Saved irf plot to:", out_png, "\n")

########################################################
# part (d) -------------------------------------------------------------

set.seed(123)
T <- 200
burning <- 100

e <- rnorm(T + 1, mean = 0, sd = sqrt(3))


simulate_y <- function(e) {
  y <- numeric(length(e))
  y_lag <- 0
  e_lag <- 0
  for (t in seq_along(e)) {
    y[t] <- 0.8 * y_lag + e[t] + 0.4 * e_lag
    y_lag <- y[t]
    e_lag <- e[t]
  }
  return(y)
}

y <- simulate_y(e)

y_burning <- numeric(T-burning)
for (t in 1:T-burning) {
  y_burning[t] <- y[t+burning]
}


y_plot <- data.frame(
  t = 1:100,
  y = y_burning
)

get_script_dir <- function() {
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    path <- rstudioapi::getActiveDocumentContext()$path
    if (nzchar(path)) return(dirname(path))
  }
  getwd()
}

script_dir <- get_script_dir()
out_png <- file.path(script_dir, "hw2_q2d_y.png")

png(out_png, width = 900, height = 700, res = 130)
plot(y_plot$t, y_plot$y, type = "o", pch = 16,
     xlab = "t", ylab = "y",
     main = "Simulated path of y")
abline(h = 0, lty = 3)
dev.off()

cat("Saved irf plot to:", out_png, "\n")

########################################################
# part (e) -------------------------------------------------------------
## i. Hannan-Rissanen

y_use <- as.numeric(y_burning)
n <- length(y_use)

M <- 3

## # Long autoregression with M = 3
Y1 <- y_use[(M+1):n]
X1 <- embed(y_use, M + 1)[, 2:(M+1), drop = FALSE]
df1 <- data.frame(Y1, X1)
colnames(df1) <- c("Y", paste0("yL", 1:M))

fit1 <- lm(Y ~ ., data = df1) 
ehat1 <- residuals(fit1)

## # OLS
Y2  <- y_use[(M+2):n] - ehat1[2:length(ehat1)]
y_lag <- y_use[(M+1):(n-1)]
e_lag <- ehat1[1:(length(ehat1)-1)]

fit2 <- lm(Y2 ~ y_lag + e_lag)
coef2 <- coef(fit2)
phi_2   <- unname(coef2["ylag"])
theta_2 <- unname(coef2["elag"])

cat("HR Step 2 estimates:\n")
cat(sprintf("  phi_hat   = %.6f\n", phi_2))
cat(sprintf("  theta_hat = %.6f\n\n", theta_2))

tab2 <- xtable(summary(fit2),
               caption = "Hannan--Rissanen Estimates",
               label = "tab:hr_step2")

print(tab2,
      type = "latex",
      include.rownames = TRUE)

## ii. MoM -------------------------------------------------------------

stopifnot(length(y_burning) >= 10)
y_mom <- as.numeric(y_burning)
y_mom <- y_mom[is.finite(y_mom)]

innovation <- function(y, alpha, theta) {
  n <- length(y)
  e <- numeric(n)
  e_lag <- 0
  y_lag <- 0
  for (t in 1:n) {
    e[t] <- y[t] - alpha * y_lag - theta * e_lag
    y_lag <- y[t]
    e_lag <- e[t]
  }
  e
}

# generate moments: g_k = mean(e_t * y_{t-k})
moments <- function(par, y, K) {
  n <- length(y)
  alpha <- par[1]; theta <- par[2]
  e <- innovation(y, alpha, theta)
  sapply(K, function(k) mean(e[(k+1):n] * y[1:(n-k)]))
}

objective <- function(par, y, K) {
  g <- moments(par, y, K)
  sum(g^2)
}

K_mom <- c(1, 2)

fit <- optim(
  par = c(0.7, 0.2),
  fn  = objective,
  y   = y_mom,
  K   = K_mom,
  method = "L-BFGS-B",
  lower = c(-0.99, -0.99),
  upper = c( 0.99,  0.9)
)

fit$par



alpha_hat <- fit$par[1]
theta_hat <- fit$par[2]

cat("MOM estimates (K = {1,2}):\n")
cat(sprintf("  alpha_hat   = %.6f\n", alpha_hat))
cat(sprintf("  theta_hat = %.6f\n", theta_hat))
cat(sprintf("  objective = %.6e\n\n", fit$value))

# part (f) -------------------------------------------------------------
## Simulation 
R <- 1000
T <- 200
burning <- 100

alpha_true <- 0.8
theta_true <- 0.4
sigma2_e   <- 3

M <- 3
K_mom <- c(1, 2)

set.seed(123)

y_burn_list <- vector("list", R)

for (r in 1:R) {
  e_r <- rnorm(T + 1, mean = 0, sd = sqrt(sigma2_e))
  y_r <- simulate_y(e_r)  # uses your DGP: 0.8 and 0.4 inside

  # burn (keep exactly T-burning observations, matching your code)
  y_burn_r <- numeric(T - burning)
  for (t in 1:(T - burning)) {
    y_burn_r[t] <- y_r[t + burning]
  }

  y_burn_list[[r]] <- y_burn_r
}

cat("Simulation done. Stored", length(y_burn_list), "burnt samples.\n")

## Hannan-Rissanen procedure as a whole function

hr_estimator <- function(y_use, M = 3) {
  y_use <- as.numeric(y_use)
  n <- length(y_use)
  if (n <= M + 5) return(c(alpha = NA_real_, theta = NA_real_))

  # Step 1 long AR(M)
  Y1 <- y_use[(M+1):n]
  X1 <- embed(y_use, M + 1)[, 2:(M+1), drop = FALSE]
  df1 <- data.frame(Y1, X1)
  colnames(df1) <- c("Y", paste0("yL", 1:M))
  fit1 <- lm(Y ~ ., data = df1)
  ehat1 <- residuals(fit1)

  # Step 2 OLS: y_t - ehat_t on y_{t-1}, ehat_{t-1}
  Y2    <- y_use[(M+2):n] - ehat1[2:length(ehat1)]
  y_lag <- y_use[(M+1):(n-1)]
  e_lag <- ehat1[1:(length(ehat1)-1)]
  fit2 <- lm(Y2 ~ y_lag + e_lag)

  c(alpha = unname(coef(fit2)[2]),
    theta = unname(coef(fit2)[3]))
}

## MoM as a whole function

mom_estimator <- function(y_mom, K = c(1,2)) {
  y_mom <- as.numeric(y_mom)
  y_mom <- y_mom[is.finite(y_mom)]
  n <- length(y_mom)
  if (n <= max(K) + 5) return(c(alpha = NA_real_, theta = NA_real_))

  innovation <- function(y, alpha, theta) {
    n <- length(y)
    e <- numeric(n)
    e_lag <- 0
    y_lag <- 0
    for (t in 1:n) {
      e[t] <- y[t] - alpha * y_lag - theta * e_lag
      y_lag <- y[t]
      e_lag <- e[t]
    }
    e
  }

  moments <- function(par, y, K) {
    n <- length(y)
    alpha <- par[1]; theta <- par[2]
    e <- innovation(y, alpha, theta)
    sapply(K, function(k) mean(e[(k+1):n] * y[1:(n-k)]))
  }

  objective <- function(par, y, K) {
    g <- moments(par, y, K)
    if (any(!is.finite(g))) return(1e50)
    sum(g^2)
  }

  fit <- optim(
    par = c(0.7, 0.2),
    fn  = objective,
    y   = y_mom,
    K   = K,
    method = "L-BFGS-B",
    lower = c(-0.99, -0.99),
    upper = c( 0.99,  0.9)
  )

  c(alpha = fit$par[1], theta = fit$par[2])
}

## Generate estimates 

alpha_hat_hr  <- numeric(R)
theta_hat_hr  <- numeric(R)
alpha_hat_mom <- numeric(R)
theta_hat_mom <- numeric(R)

for (r in 1:R) {
  y_burn_r <- y_burn_list[[r]]

  est_hr <- hr_estimator(y_burn_r, M = M)
  alpha_hat_hr[r] <- est_hr["alpha"]
  theta_hat_hr[r] <- est_hr["theta"]

  est_mm <- mom_estimator(y_burn_r, K = K_mom)
  alpha_hat_mom[r] <- est_mm["alpha"]
  theta_hat_mom[r] <- est_mm["theta"]
}

## Data tabulation and visualization

ok_hr  <- is.finite(alpha_hat_hr)  & is.finite(theta_hat_hr)
ok_mom <- is.finite(alpha_hat_mom) & is.finite(theta_hat_mom)

summ_stats <- function(xhat, xtrue) {
  bias <- mean(xhat - xtrue)
  mse  <- mean((xhat - xtrue)^2)
  sdv  <- sd(xhat)
  c(mean_hat = mean(xhat), bias = bias, mse = mse, sd = sdv)
}

s_alpha_hr  <- summ_stats(alpha_hat_hr[ok_hr], alpha_true)
s_theta_hr  <- summ_stats(theta_hat_hr[ok_hr], theta_true)
s_alpha_mom <- summ_stats(alpha_hat_mom[ok_mom], alpha_true)
s_theta_mom <- summ_stats(theta_hat_mom[ok_mom], theta_true)

summary_df <- data.frame(
  method    = c("HR", "HR", "MoM ", "MoM"),
  parameter = c("alpha", "theta", "alpha", "theta"),
  true      = c(alpha_true, theta_true, alpha_true, theta_true),
  mean_hat  = c(s_alpha_hr["mean_hat"],  s_theta_hr["mean_hat"],
                s_alpha_mom["mean_hat"], s_theta_mom["mean_hat"]),
  bias      = c(s_alpha_hr["bias"],  s_theta_hr["bias"],
                s_alpha_mom["bias"], s_theta_mom["bias"]),
  mse       = c(s_alpha_hr["mse"],   s_theta_hr["mse"],
                s_alpha_mom["mse"],  s_theta_mom["mse"]),
  sd        = c(s_alpha_hr["sd"],    s_theta_hr["sd"],
                s_alpha_mom["sd"],   s_theta_mom["sd"])
)

tab_mc <- xtable(summary_df,
                 caption = sprintf("Parameter estimates statistics.",
                                   R, T, burning, M),
                 label = "tab:q2f")

print(tab_mc, type = "latex", include.rownames = FALSE, comment = FALSE)

plot_dist <- function(x, xtrue, main_title, xlab, fname) {
  png(file.path(script_dir, fname), width = 900, height = 700, res = 130)
  hist(x, breaks = 35, freq = FALSE, main = main_title, xlab = xlab)
  abline(v = xtrue, lty = 2)
  lines(density(x))
  dev.off()
}

plot_dist(alpha_hat_hr[ok_hr],  alpha_true,
          "Distribution of alpha_hat (HR)", "alpha_hat", "hw2_q2f_alpha_hr.png")
plot_dist(theta_hat_hr[ok_hr],  theta_true,
          "Distribution of theta_hat (HR)", "theta_hat", "hw2_q2f_theta_hr.png")

plot_dist(alpha_hat_mom[ok_mom], alpha_true,
          "Distribution of alpha_hat (MoM)", "alpha_hat", "hw2_q2f_alpha_mom.png")
plot_dist(theta_hat_mom[ok_mom], theta_true,
          "Distribution of theta_hat (MoM)", "theta_hat", "hw2_q2f_theta_mom.png")

cat("Saved plots to:\n",
    file.path(script_dir, "hw2_q2f_alpha_hr.png"), "\n",
    file.path(script_dir, "hw2_q2f_theta_hr.png"), "\n",
    file.path(script_dir, "hw2_q2f_alpha_mom.png"), "\n",
    file.path(script_dir, "hw2_q2f_theta_mom.png"), "\n")

# part (g) -------------------------------------------------------------
## T=50
R <- 1000
T <- 100
burning <- 50

set.seed(123)

y_burn_list <- vector("list", R)

for (r in 1:R) {
  e_r <- rnorm(T + 1, mean = 0, sd = sqrt(sigma2_e))
  y_r <- simulate_y(e_r)  # uses your DGP: 0.8 and 0.4 inside

  # burn (keep exactly T-burning observations, matching your code)
  y_burn_r <- numeric(T - burning)
  for (t in 1:(T - burning)) {
    y_burn_r[t] <- y_r[t + burning]
  }

  y_burn_list[[r]] <- y_burn_r
}

cat("Simulation done. Stored", length(y_burn_list), "burnt samples.\n")

alpha_hat_hr50  <- numeric(R)
theta_hat_hr50  <- numeric(R)
alpha_hat_mom50 <- numeric(R)
theta_hat_mom50 <- numeric(R)

for (r in 1:R) {
  y_burn_r <- y_burn_list[[r]]

  est_hr <- hr_estimator(y_burn_r, M = M)
  alpha_hat_hr50[r] <- est_hr["alpha"]
  theta_hat_hr50[r] <- est_hr["theta"]

  est_mm <- mom_estimator(y_burn_r, K = K_mom)
  alpha_hat_mom50[r] <- est_mm["alpha"]
  theta_hat_mom50[r] <- est_mm["theta"]
}

## Data tabulation and visualization

ok_hr  <- is.finite(alpha_hat_hr50)  & is.finite(theta_hat_hr50)
ok_mom <- is.finite(alpha_hat_mom50) & is.finite(theta_hat_mom50)

s_alpha_hr50  <- summ_stats(alpha_hat_hr50[ok_hr], alpha_true)
s_theta_hr50  <- summ_stats(theta_hat_hr50[ok_hr], theta_true)
s_alpha_mom50 <- summ_stats(alpha_hat_mom50[ok_mom], alpha_true)
s_theta_mom50 <- summ_stats(theta_hat_mom50[ok_mom], theta_true)

summary_df50 <- data.frame(
  method    = c("HR", "HR", "MoM ", "MoM"),
  parameter = c("alpha", "theta", "alpha", "theta"),
  true      = c(alpha_true, theta_true, alpha_true, theta_true),
  mean_hat  = c(s_alpha_hr50["mean_hat"],  s_theta_hr50["mean_hat"],
                s_alpha_mom50["mean_hat"], s_theta_mom50["mean_hat"]),
  bias      = c(s_alpha_hr50["bias"],  s_theta_hr50["bias"],
                s_alpha_mom50["bias"], s_theta_mom50["bias"]),
  mse       = c(s_alpha_hr50["mse"],   s_theta_hr50["mse"],
                s_alpha_mom50["mse"],  s_theta_mom50["mse"]),
  sd        = c(s_alpha_hr50["sd"],    s_theta_hr50["sd"],
                s_alpha_mom50["sd"],   s_theta_mom50["sd"])
)

tab_mc50 <- xtable(summary_df50,
                 caption = sprintf("Parameter estimates statistics."),
                 label = "tab:q2g50")

print(tab_mc50, type = "latex", include.rownames = FALSE, comment = FALSE)

plot_dist(alpha_hat_hr50[ok_hr],  alpha_true,
          "Distribution of alpha_hat (HR)", "alpha_hat", "hw2_q2g50_alpha_hr.png")
plot_dist(theta_hat_hr50[ok_hr],  theta_true,
          "Distribution of theta_hat (HR", "theta_hat", "hw2_q2g50_theta_hr.png")

plot_dist(alpha_hat_mom50[ok_mom], alpha_true,
          "Distribution of alpha_hat (MoM)", "alpha_hat", "hw2_q2g50_alpha_mom.png")
plot_dist(theta_hat_mom50[ok_mom], theta_true,
          "Distribution of theta_hat (MoM)", "theta_hat", "hw2_q2g50_theta_mom.png")

cat("Saved plots to:\n",
    file.path(script_dir, "hw2_q2g50_alpha_hr.png"), "\n",
    file.path(script_dir, "hw2_q2g50_theta_hr.png"), "\n",
    file.path(script_dir, "hw2_q2g50_alpha_mom.png"), "\n",
    file.path(script_dir, "hw2_q2g50_theta_mom.png"), "\n")

## T=500
T <- 1000
burning <- 500

set.seed(123)

y_burn_list <- vector("list", R)

for (r in 1:R) {
  e_r <- rnorm(T + 1, mean = 0, sd = sqrt(sigma2_e))
  y_r <- simulate_y(e_r)  # uses your DGP: 0.8 and 0.4 inside

  # burn (keep exactly T-burning observations, matching your code)
  y_burn_r <- numeric(T - burning)
  for (t in 1:(T - burning)) {
    y_burn_r[t] <- y_r[t + burning]
  }

  y_burn_list[[r]] <- y_burn_r
}

cat("Simulation done. Stored", length(y_burn_list), "burnt samples.\n")

alpha_hat_hr500  <- numeric(R)
theta_hat_hr500  <- numeric(R)
alpha_hat_mom500 <- numeric(R)
theta_hat_mom500 <- numeric(R)

for (r in 1:R) {
  y_burn_r <- y_burn_list[[r]]

  est_hr <- hr_estimator(y_burn_r, M = M)
  alpha_hat_hr500[r] <- est_hr["alpha"]
  theta_hat_hr500[r] <- est_hr["theta"]

  est_mm <- mom_estimator(y_burn_r, K = K_mom)
  alpha_hat_mom500[r] <- est_mm["alpha"]
  theta_hat_mom500[r] <- est_mm["theta"]
}

## Data tabulation and visualization

ok_hr  <- is.finite(alpha_hat_hr500)  & is.finite(theta_hat_hr500)
ok_mom <- is.finite(alpha_hat_mom500) & is.finite(theta_hat_mom500)

s_alpha_hr500  <- summ_stats(alpha_hat_hr500[ok_hr], alpha_true)
s_theta_hr500  <- summ_stats(theta_hat_hr500[ok_hr], theta_true)
s_alpha_mom500 <- summ_stats(alpha_hat_mom500[ok_mom], alpha_true)
s_theta_mom500 <- summ_stats(theta_hat_mom500[ok_mom], theta_true)

summary_df500 <- data.frame(
  method    = c("HR", "HR", "MoM ", "MoM"),
  parameter = c("alpha", "theta", "alpha", "theta"),
  true      = c(alpha_true, theta_true, alpha_true, theta_true),
  mean_hat  = c(s_alpha_hr500["mean_hat"],  s_theta_hr500["mean_hat"],
                s_alpha_mom500["mean_hat"], s_theta_mom500["mean_hat"]),
  bias      = c(s_alpha_hr500["bias"],  s_theta_hr500["bias"],
                s_alpha_mom500["bias"], s_theta_mom500["bias"]),
  mse       = c(s_alpha_hr500["mse"],   s_theta_hr500["mse"],
                s_alpha_mom500["mse"],  s_theta_mom500["mse"]),
  sd        = c(s_alpha_hr500["sd"],    s_theta_hr500["sd"],
                s_alpha_mom500["sd"],   s_theta_mom500["sd"])
)

tab_mc500 <- xtable(summary_df500,
                 caption = sprintf("Parameter estimates statistics."),
                 label = "tab:q2g500")

print(tab_mc500, type = "latex", include.rownames = FALSE, comment = FALSE)

plot_dist(alpha_hat_hr500[ok_hr],  alpha_true,
          "Distribution of alpha_hat (HR)", "alpha_hat", "hw2_q2g500_alpha_hr.png")
plot_dist(theta_hat_hr500[ok_hr],  theta_true,
          "Distribution of theta_hat (HR", "theta_hat", "hw2_q2g500_theta_hr.png")

plot_dist(alpha_hat_mom500[ok_mom], alpha_true,
            "Distribution of alpha_hat (MoM)", "alpha_hat", "hw2_q2g500_alpha_mom.png")
plot_dist(theta_hat_mom500[ok_mom], theta_true,
          "Distribution of theta_hat (MoM)", "theta_hat", "hw2_q2g500_theta_mom.png")

cat("Saved plots to:\n",
    file.path(script_dir, "hw2_q2g500_alpha_hr.png"), "\n",
    file.path(script_dir, "hw2_q2g500_theta_hr.png"), "\n",
    file.path(script_dir, "hw2_q2g500_alpha_mom.png"), "\n",
    file.path(script_dir, "hw2_q2g500_theta_mom.png"), "\n")
