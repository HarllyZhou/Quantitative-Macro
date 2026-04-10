rm(list = ls())

# ----------------------------
# Part III (j)(k): RBC with G shock
# ----------------------------

# Output directory
out_dir <- "figuretable/hw8"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ----------------------------
# 1. Parameters
# ----------------------------
alpha   <- 1/3
eta     <- 1
phi_adj <- 1
delta   <- 0.025
beta    <- 0.99
sigma   <- 1
rho     <- 0.985
sigma_e <- 0.007

psi     <- 0.95
sigma_u <- 0.004

# ----------------------------
# 2. Steady-state ratios
# ----------------------------
R_ss   <- 1 / beta - (1 - delta)
KY_ss  <- alpha / R_ss
IY_ss  <- delta * KY_ss
GY_ss  <- 0.2
CY_ss  <- 1 - IY_ss - GY_ss

# normalize Y*=1
Y_ss <- 1
G_ss <- GY_ss * Y_ss
kappa <- G_ss^(1 - psi)

cat("Steady-state ratios:\n")
cat("R*   =", R_ss, "\n")
cat("K/Y  =", KY_ss, "\n")
cat("I/Y  =", IY_ss, "\n")
cat("C/Y  =", CY_ss, "\n")
cat("G/Y  =", GY_ss, "\n")
cat("kappa=", kappa, "\n\n")

# ----------------------------
# 3. Static mappings
# ----------------------------
# State vector:
# s_t = [k_{t-1}, z_t, g_t]'
#
# Policy rules:
# c_t = pc %*% s_t
# k_t = pk %*% s_t
#
# Then y_t, l_t, i_t are implied.

A1 <- 1 + 1 / eta
D  <- 1 - (1 - alpha) / A1

e1 <- c(1, 0, 0)
e2 <- c(0, 1, 0)
e3 <- c(0, 0, 1)

get_py <- function(pc) {
  a0 <- c(alpha / D, 1 / D, 0)
  ac <- - ((1 - alpha) * sigma / A1) / D
  a0 + ac * pc
}

get_pl <- function(pc, py) {
  (-sigma * pc + py) / A1
}

get_pi <- function(pk) {
  (pk - (1 - delta) * e1) / delta
}

# ----------------------------
# 4. Solve for policy coefficients
# ----------------------------
# Equations:
# (i) resource constraint with G
# (ii) Euler equation

residuals_policy <- function(par) {
  pc <- par[1:3]
  pk <- par[4:6]

  py <- get_py(pc)

  # E_t s_{t+1} = F s_t
  F <- rbind(
    pk,
    c(0, rho, 0),
    c(0, 0, psi)
  )

  # resource constraint
  res_resource <- py - CY_ss * pc - IY_ss * get_pi(pk) - GY_ss * e3

  # Euler equation
  lhs <- sigma * (pc %*% F - pc)
  rhs <- beta * R_ss * (py %*% F - pk) +
    beta * phi_adj * (pk %*% F - pk) -
    phi_adj * (pk - e1)

  res_euler <- as.numeric(lhs - rhs)

  c(res_resource, res_euler)
}

obj_policy <- function(par) {
  r <- residuals_policy(par)
  sum(r^2)
}

starts <- list(
  c(0.10, 0.30, -0.10, 0.10, 0.20, -0.10),
  c(0.20, 0.40, -0.20, 0.20, 0.30, -0.20),
  c(0.05, 0.20, -0.05, 0.05, 0.10, -0.05),
  c(0.15, 0.50, -0.10, 0.10, 0.50, -0.10)
)

best <- NULL
best_val <- Inf

for (st in starts) {
  fit <- optim(
    par = st,
    fn = obj_policy,
    method = "BFGS",
    control = list(reltol = 1e-14, maxit = 5000)
  )
  if (fit$value < best_val) {
    best <- fit
    best_val <- fit$value
  }
}

pc <- best$par[1:3]
pk <- best$par[4:6]
py <- get_py(pc)
pl <- get_pl(pc, py)
pi_rule <- get_pi(pk)

cat("Optimization objective =", best$value, "\n")
cat("pc =", pc, "\n")
cat("pk =", pk, "\n")
cat("py =", py, "\n")
cat("pl =", pl, "\n")
cat("pi =", pi_rule, "\n\n")

# ----------------------------
# 5. Reduced-form state-space
# ----------------------------
# s_t = A s_{t-1} + B v_t
# v_t = [epsilon_t, u_t]'

A <- rbind(
  c(pk[1], pk[2] * rho, pk[3] * psi),
  c(0, rho, 0),
  c(0, 0, psi)
)

B <- matrix(
  c(pk[2], pk[3],
    1,     0,
    0,     1),
  nrow = 3, byrow = TRUE
)

colnames(B) <- c("tech", "gov")

cat("A matrix:\n")
print(A)
cat("\nB matrix:\n")
print(B)
cat("\n")

# measurement vectors
Hc <- pc
Hy <- py
Hl <- pl
Hi <- pi_rule
Hk <- pk

# ----------------------------
# 6. IRF function
# ----------------------------
make_irf <- function(A, B, Hlist, shock_index, horizon = 40) {
  n_state <- nrow(A)
  x <- matrix(0, nrow = n_state, ncol = horizon + 1)
  x[, 1] <- B[, shock_index]

  out <- matrix(NA_real_, nrow = horizon + 1, ncol = length(Hlist))
  colnames(out) <- names(Hlist)

  for (h in 0:horizon) {
    out[h + 1, "c"] <- sum(Hlist$c * x[, h + 1])
    out[h + 1, "y"] <- sum(Hlist$y * x[, h + 1])
    out[h + 1, "l"] <- sum(Hlist$l * x[, h + 1])
    out[h + 1, "i"] <- sum(Hlist$i * x[, h + 1])
    out[h + 1, "k"] <- sum(Hlist$k * x[, h + 1])

    if (h < horizon) {
      x[, h + 2] <- A %*% x[, h + 1]
    }
  }

  out
}

Hlist <- list(c = Hc, y = Hy, l = Hl, i = Hi, k = Hk)

irf_tech <- make_irf(A, B, Hlist, shock_index = 1, horizon = 40)
irf_gov  <- make_irf(A, B, Hlist, shock_index = 2, horizon = 40)

# ----------------------------
# 7. Plot IRF to government shock (part j)
# ----------------------------
png(file.path(out_dir, "hw8_part3_gov_irf.png"),
    width = 1400, height = 1000, res = 180)

par(mfrow = c(3, 2), mar = c(4, 4, 3, 1), oma = c(0, 0, 2, 0))

vars <- c("c", "y", "l", "i", "k")
titles <- c("c response", "y response", "l response", "i response", "k response")

for (j in seq_along(vars)) {
  v <- vars[j]
  plot(0:40, irf_gov[, v], type = "l", lwd = 2,
       xlab = "Horizon", ylab = "Log deviation", main = titles[j])
  abline(h = 0, lty = 2)
}

plot.new()
mtext("Part III (j): Impulse responses to a unit government spending shock",
      outer = TRUE, cex = 1.4, font = 2)

dev.off()

# ----------------------------
# 8. Plot tech vs government shock comparison
# ----------------------------
png(file.path(out_dir, "hw8_part3_compare_tech_gov_irf.png"),
    width = 1400, height = 1000, res = 180)

par(mfrow = c(3, 2), mar = c(4, 4, 3, 1), oma = c(0, 0, 2, 0))

for (j in seq_along(vars)) {
  v <- vars[j]
  yr <- range(c(irf_tech[, v], irf_gov[, v]))
  plot(0:40, irf_tech[, v], type = "l", lwd = 2, ylim = yr,
       xlab = "Horizon", ylab = "Log deviation", main = titles[j])
  lines(0:40, irf_gov[, v], lwd = 2, lty = 2)
  abline(h = 0, lty = 2)
  legend("topright", legend = c("technology shock", "government shock"),
         lwd = 2, lty = c(1, 2), bty = "n", cex = 0.9)
}

plot.new()
mtext("Part III (j): Technology shock vs government spending shock",
      outer = TRUE, cex = 1.4, font = 2)

dev.off()

# ----------------------------
# 9. Forecast error variance decomposition (part k)
# ----------------------------
# For variable q_t = H x_t, contribution of shock s at horizon h:
# sum_{m=0}^h (H A^m b_s)^2 * var(shock_s)

fevd_one <- function(A, B, H, sigma_vec, horizon = 40) {
  n <- nrow(A)
  n_shock <- ncol(B)

  contrib <- matrix(0, nrow = horizon + 1, ncol = n_shock)
  colnames(contrib) <- c("tech", "gov")

  A_pow <- diag(n)

  for (h in 0:horizon) {
    if (h == 0) {
      A_pow <- diag(n)
    } else {
      A_pow <- A %*% A_pow
    }

    for (s in 1:n_shock) {
      impulse_h <- as.numeric(H %*% A_pow %*% B[, s])
      contrib[h + 1, s] <- if (h == 0) {
        impulse_h^2 * sigma_vec[s]^2
      } else {
        contrib[h, s] + impulse_h^2 * sigma_vec[s]^2
      }
    }
  }

  total <- rowSums(contrib)
  frac <- contrib / total
  frac
}

sigma_vec <- c(sigma_e, sigma_u)

fevd_c <- fevd_one(A, B, Hc, sigma_vec, horizon = 40)
fevd_y <- fevd_one(A, B, Hy, sigma_vec, horizon = 40)
fevd_l <- fevd_one(A, B, Hl, sigma_vec, horizon = 40)
fevd_i <- fevd_one(A, B, Hi, sigma_vec, horizon = 40)

# ----------------------------
# 10. Plot FEVD
# ----------------------------
png(file.path(out_dir, "hw8_part3_fevd.png"),
    width = 1400, height = 1000, res = 180)

par(mfrow = c(2, 2), mar = c(4, 4, 3, 1), oma = c(0, 0, 2, 0))

plot_fevd <- function(fevd_mat, main_txt) {
  plot(0:40, fevd_mat[, "tech"], type = "l", lwd = 2, ylim = c(0, 1),
       xlab = "Horizon", ylab = "Fraction", main = main_txt)
  lines(0:40, fevd_mat[, "gov"], lwd = 2, lty = 2)
  abline(h = c(0, 1), lty = 2)
  legend("right", legend = c("technology shock", "government shock"),
         lwd = 2, lty = c(1, 2), bty = "n", cex = 0.9)
}

plot_fevd(fevd_c, "Consumption")
plot_fevd(fevd_y, "Output")
plot_fevd(fevd_l, "Labor")
plot_fevd(fevd_i, "Investment")

mtext("Part III (k): Forecast variance decomposition",
      outer = TRUE, cex = 1.4, font = 2)

dev.off()

# ----------------------------
# 11. Save FEVD tables
# ----------------------------
fevd_table <- data.frame(
  h = 0:40,
  C_tech = fevd_c[, "tech"], C_gov = fevd_c[, "gov"],
  Y_tech = fevd_y[, "tech"], Y_gov = fevd_y[, "gov"],
  L_tech = fevd_l[, "tech"], L_gov = fevd_l[, "gov"],
  I_tech = fevd_i[, "tech"], I_gov = fevd_i[, "gov"]
)

write.csv(fevd_table, file.path(out_dir, "hw8_part3_fevd.csv"), row.names = FALSE)

cat("Files written to:", out_dir, "\n")
cat(" - hw8_part3_gov_irf.png\n")
cat(" - hw8_part3_compare_tech_gov_irf.png\n")
cat(" - hw8_part3_fevd.png\n")
cat(" - hw8_part3_fevd.csv\n")