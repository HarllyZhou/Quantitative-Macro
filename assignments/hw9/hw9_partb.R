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
output_dir <- file.path(script_dir, "hw9_partb_output")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

T <- 2000L
BURNIN <- 1000L
SEED <- 12345L

par <- list(
  alpha = 1 / 3,
  eta = 1,
  phi = 5,
  delta = 0.025,
  xi = 1,
  beta = 0.99,
  sigma = 1,
  rho = 0.9,
  sigma_eps = 1,
  sigma_e2 = 0.25
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
    # combine foc of labour and production functions:
    # (alpha+1/eta) l_t = -sigma c_t + z_t + alpha k_t
    # plug this back into the production function:
    # y_t = z_t + alpha k_t +
    ## (1-alpha)/(alpha+1/eta) [-sigma c_t + z_t + alpha k_t]
    ## = (1+1/eta)/(alpha+1/eta) z_t 
    ## + alpha(1+1/eta)/(alpha+1/eta) k_t 
    ## - sigma(1-alpha)/(alpha+1/eta) c_t
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
    # initial guess of
    # c_t = r k_t + s z_t
    # k_t = p k_t-1 + q z_t
    # y_t = gk + gc * r k_t + gz * z_t
    # l_t = (-sigma c_t + y_t) / (alpha+1/eta)
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

solve_reduced_form <- function(par, ss, lin, tol = 1e-8) {
  starts <- list(
    c(0.95, 0.05, 0.02, 0.50),
    c(0.90, 0.10, 0.05, 1.00),
    c(0.98, 0.02, 0.01, 0.20),
    c(0.75, 0.15, 0.10, 0.75),
    c(0.60, 0.25, 0.10, 0.10),
    c(0.95, 0.05, 0.20, 0.80),
    c(0.85, 0.15, 0.15, 1.20),
    c(0.99, 0.01, 0.05, 0.10)
  )

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

  Xi <- par$sigma_e2 * diag(4)

  rownames(H) <- c("c", "i", "y", "l")
  colnames(H) <- c("k_lag", "z")
  rownames(B) <- colnames(B) <- c("k_lag", "z")
  rownames(Xi) <- colnames(Xi) <- c("c", "i", "y", "l")

  list(B = B, H = H, Xi = Xi)
}

simulate_part_b <- function(par, state_space, T, burnin, seed) {
  set.seed(seed)

  TT <- T + burnin
  state <- matrix(0, nrow = 2, ncol = TT + 1)
  latent_x <- matrix(0, nrow = 4, ncol = TT)
  observed_x <- matrix(0, nrow = 4, ncol = TT)
  w_draws <- numeric(TT)
  e_draws <- matrix(0, nrow = 4, ncol = TT)

  for (t in seq_len(TT)) {
    latent_x[, t] <- state_space$H %*% state[, t]

    e_t <- sqrt(par$sigma_e2) * rnorm(4)
    observed_x[, t] <- latent_x[, t] + e_t
    e_draws[, t] <- e_t

    w_tp1 <- rnorm(1)
    u_tp1 <- c(0, par$sigma_eps * w_tp1)
    w_draws[t] <- w_tp1

    state[, t + 1] <- state_space$B %*% state[, t] + u_tp1
  }

  keep <- (burnin + 1L):TT

  data.frame(
    t = seq_len(T),
    c_star = observed_x[1, keep],
    i_star = observed_x[2, keep],
    y_star = observed_x[3, keep],
    l_star = observed_x[4, keep],
    c = latent_x[1, keep],
    i = latent_x[2, keep],
    y = latent_x[3, keep],
    l = latent_x[4, keep],
    k_lag = state[1, keep],
    z = state[2, keep],
    w = w_draws[keep],
    e_c = e_draws[1, keep],
    e_i = e_draws[2, keep],
    e_y = e_draws[3, keep],
    e_l = e_draws[4, keep]
  )
}

ss <- compute_steady_state(par)
lin <- compute_linear_objects(par)
sol <- solve_reduced_form(par, ss, lin)
state_space <- build_state_space(par, lin, sol)

sim_data <- simulate_part_b(
  par = par,
  state_space = state_space,
  T = T,
  burnin = BURNIN,
  seed = SEED
)

write_txt_table(
  sim_data,
  file.path(output_dir, "hw9_partb_simulated_data.txt")
)

write_matrix_txt(
  state_space$B,
  file.path(output_dir, "hw9_partb_state_B_matrix.txt")
)

write_matrix_txt(
  state_space$H,
  file.path(output_dir, "hw9_partb_measurement_H_matrix.txt")
)

write_matrix_txt(
  state_space$Xi,
  file.path(output_dir, "hw9_partb_measurement_Xi_matrix.txt")
)

summary_text <- capture.output({
  cat("ECON 5345 HW9 Part (b)\n")
  cat("======================\n\n")

  cat("Policy coefficients [p, q, r, s]:\n")
  print(round(sol$par, 10))

  cat("\nResidual check:\n")
  print(signif(sol$residuals, 6))

  cat("\nState transition matrix B in s_{t+1} = B s_t + u_{t+1}:\n")
  print(round(state_space$B, 10))

  cat("\nMeasurement matrix H in x_t = H s_t:\n")
  print(round(state_space$H, 10))

  cat("\nMeasurement error covariance Xi:\n")
  print(round(state_space$Xi, 10))

  cat("\nSteady-state objects:\n")
  print(round(unlist(ss), 10))

  cat("\nFirst 10 simulated observations:\n")
  print(round(sim_data[1:10, ], 6), row.names = FALSE)
})

writeLines(summary_text, con = file.path(output_dir, "hw9_partb_summary.txt"))